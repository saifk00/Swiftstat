
ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "swiftstat"

val chiselVersion = "3.5.1"

/**
 * ANTLR Plugin
 *
 * Lets us compile the PGM format grammar into a parser.
 */
enablePlugins(Antlr4Plugin)
Antlr4 / antlr4GenVisitor := true
Antlr4 / antlr4TreatWarningsAsErrors := true
Antlr4 / antlr4PackageName := Some("org.swiftstat.pgm.antlr")
Antlr4 / antlr4Version := "4.9.3"

/**
 * ScalaPB
 */
Compile / PB.targets := Seq(
  scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"
)

lazy val root = (project in file("."))
  .settings(
    name := "swiftstat",

    // for command line parsing
    libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0",

    // json for scalapb
    libraryDependencies += "com.thesamet.scalapb" %% "scalapb-json4s" % "0.11.1",

    // hardware float
    libraryDependencies += "edu.berkeley.cs" %% "hardfloat" % "1.5-SNAPSHOT",

    // logging
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.10",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",

    // chisel deps
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % "0.5.1" % "test"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-P:chiselplugin:genBundleElements",
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
  )