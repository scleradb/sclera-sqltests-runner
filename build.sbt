name := "sclera-sqltests-runner"

description := "SQL Tests Runner"

version := "4.0-SNAPSHOT"

homepage := Some(url("https://github.com/scleradb/sclera-tests-runner"))

organization := "com.scleradb"

organizationName := "Sclera, Inc."

organizationHomepage := Some(url("https://www.scleradb.com"))

startYear := Some(2012)

scalaVersion := "2.13.1"

licenses := Seq("Apache License version 2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"))

libraryDependencies ++= Seq(
    "com.scleradb" %% "sclera-core" % "4.0-SNAPSHOT" % "provided",
    "com.scleradb" %% "sclera-sqltests-parser" % "4.0-SNAPSHOT",
    "org.scalatest" %% "scalatest" % "3.1.0" % "provided"
)

scalacOptions ++= Seq(
    "-Werror", "-feature", "-deprecation", "-unchecked"
)

exportJars := true
