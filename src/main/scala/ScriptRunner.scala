/**
* Sclera - SQL Tests Runner
* Copyright 2012 - 2020 Sclera, Inc.
* 
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
* 
*     http://www.apache.org/licenses/LICENSE-2.0
* 
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

package com.scleradb.sqltests.runner

import org.scalatest.CancelAfterFailure
import org.scalatest.funspec.AnyFunSpec

import java.io.{InputStream, InputStreamReader}
import java.sql.{Statement, SQLException}

import scala.util.parsing.input.StreamReader

import com.scleradb.sql.exec.ScalCastEvaluator
import com.scleradb.sqltests.parser._

trait SqlTestRunner extends AnyFunSpec with CancelAfterFailure {
    val isPrintEnabled: Boolean = true

    def runScript(stmt: Statement, scriptFile: String): Unit = {
        val scriptStreamOpt: Option[InputStream] =
            Option(getClass().getResourceAsStream(scriptFile))

        assert(!scriptStreamOpt.isEmpty,
               "Could not access script at \"" + scriptFile + "\"")
        val scriptReader: StreamReader =
            StreamReader(new InputStreamReader(scriptStreamOpt.get))

        SqlTestParser.testStatements(scriptReader).foreach {
            case (TestUpdateSuccess(update, cs), l) =>
                printInfo(withLine(l, cs))
                printInfo(withLine(l, update))
                stmt.executeUpdate(update)

            case (TestUpdateError(update, cs, msg), l) =>
                printInfo(withLine(l, cs))
                printInfo(withLine(l, update))
                val e: Exception = intercept[SQLException] {
                    stmt.executeUpdate(update)
                }

                printInfo(withLine(l, "Exception message: " + e.getMessage()))
                printInfo(withLine(l, "Expected message: " + msg.mkString("/")))

            case (TestQuerySuccess(query, cs, TestResult(cols, rows)), l) =>
                printInfo(withLine(l, cs))
                printInfo(withLine(l, query))
                val rs: java.sql.ResultSet = stmt.executeQuery(query)

                try {
                    val metaData: java.sql.ResultSetMetaData = rs.getMetaData()

                    val rsColCount: Int = metaData.getColumnCount()
                    assert(
                        rsColCount == cols.size,
                        withLine(
                            l,
                            "Found " + rsColCount +
                            " columns, expecting " + cols.size
                        )
                    )

                    val rsCols: List[String] =
                        (1 to rsColCount).toList.map { i =>
                            metaData.getColumnName(i).toUpperCase
                        }

                    val colsUpperCase: List[String] =
                        cols.map { col => col.toUpperCase }

                    assertResult(colsUpperCase) { rsCols }

                    rsCols.foreach { rsCol =>
                        assert(
                            colsUpperCase contains rsCol,
                            withLine(
                                l,
                                "Column \"" + rsCol +
                                "\" not in expected columns: (" +
                                cols.mkString(", ") + ")"
                            )

                        )
                    }

                    cols.zipWithIndex.foreach { case (col, i) =>
                        val rsi: Int = rs.findColumn(col)
                        assert(
                            rsi == i+1,
                            withLine(
                                l,
                                "Found column " + col + " at position " + rsi +
                                ", expecting at " + (i+1)
                            )
                        )
                    }

                    rows.zipWithIndex.foreach { case (row, i) =>
                        assert(
                            rs.next(),
                            withLine(l, "[%d] Result is empty".format(i+1))
                        )

                        val rsRow: List[Option[java.lang.Object]] =
                            cols.map { col =>
                                val rsv: Object = rs.getObject(col)
                                if( rs.wasNull ) None else Some(rsv)
                            }

                        assert(
                            row.size == rsRow.size,
                            withLine(
                                l,
                                "[%d] Found row size %d, expected %s".format(
                                    i+1,
                                    rsRow.size,
                                    row.size
                                )
                            )
                        )

                        val mc: java.math.MathContext =
                            new java.math.MathContext(5)
                        val matches: Boolean =
                            rsRow.zip(row).forall {
                                case (None, v) => (v == "")
                                case (Some(f: java.lang.Float), "") => false
                                case (Some(f: java.lang.Float), v) =>
                                    val rsv: Float =
                                        ScalCastEvaluator.toFloat(v)
                                    (rsv.compare(f) == 0) || {
                                        val rsvx: BigDecimal =
                                            BigDecimal(rsv.toDouble).round(mc)
                                        val fx: BigDecimal =
                                            BigDecimal(f.toDouble).round(mc)
                                        rsvx == fx
                                    }
                                case (Some(f: java.lang.Double), "") => false
                                case (Some(d: java.lang.Double), v) =>
                                    val rsv: Double =
                                        ScalCastEvaluator.toDouble(v)
                                    (rsv.compare(d) == 0) || {
                                        val rsvx: BigDecimal =
                                            BigDecimal(rsv).round(mc)
                                        val dx: BigDecimal =
                                            BigDecimal(d).round(mc)
                                        rsvx == dx
                                    }
                                case (Some(s: java.lang.String), v) =>
                                    s.trim == v.trim
                                case (Some(x), v) => v == x.toString
                            }

                        assert(
                            matches, {
                                val rsRowStrs: List[String] =
                                    rsRow.map {
                                        case None => ""
                                        case Some(rsv) => rsv.toString
                                    }

                                withLine(
                                    l,
                                    "[ROW %d] Found (%s), expected (%s)".format(
                                        i+1,
                                        rsRowStrs.mkString(", "),
                                        row.mkString(", ")
                                    )
                                )
                            }
                        )
                    }

                    assert(
                        !rs.next(),
                        withLine(
                            l,
                            "Result contains extra rows, expected " + rows.size
                        )
                    )
                } finally rs.close()

            case (TestQueryError(query, cs, msg), l) =>
                printInfo(withLine(l, cs))
                printInfo(withLine(l, query))
                val e: Exception = intercept[SQLException] {
                    val rs: java.sql.ResultSet = stmt.executeQuery(query)
                    try rs.next() finally { rs.close() }
                }

                printInfo(withLine(l, "Exception message: " + e.getMessage()))
                printInfo(withLine(l, "Expected message: " + msg.mkString("/")))

            case (TestIssue(issue, s), l) =>
                printInfo(withLine(l, s.comments))
                printInfo(withLine(l, "Issue[" + issue + "]: " + s.statement))

            case (TestNotSupported(s), l) =>
                printInfo(withLine(l, s.comments))
                printInfo(withLine(l, "Not Supported: " + s.statement))

            case (TestIgnored(statement, cs, _, _), l) =>
                printInfo(withLine(l, cs))
                printInfo(withLine(l, "Ignored: " + statement))
        }
    }

    private def withLine(l: Int, s: String): String =
        "[%d] %s".format(
            l, s.split("\n\r".toArray).map(s => s.trim).mkString(" ")
        )

    private def withLine(l: Int, ss: Seq[String]): String =
        "\n" + ss.map(s => withLine(l, s)).mkString("\n")

    private def printInfo(s: String): Unit =
        if( isPrintEnabled ) println(s) else print(">")
}
