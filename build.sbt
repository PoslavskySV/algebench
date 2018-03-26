import sbt.Keys._

organization := "cc.redberry"

name := "algebench"

version := "1.0"

scalaVersion := "2.12.3"

crossScalaVersions := Seq("2.11.11", "2.12.3")

moduleName := name.value

resolvers += Resolver.mavenLocal

libraryDependencies ++= Seq(
  "cc.redberry" %% "rings.scaladsl" % "2.3",
  "junit" % "junit" % "4.12" % Test,
  "com.novocode" % "junit-interface" % "0.11" % Test exclude("junit", "junit-dep"),
  "org.rogach" %% "scallop" % "3.1.2"
//  "com.github.scopt" %% "scopt" % "3.7.0"
)

val circeVersion = "0.9.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)


publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._

releaseCrossBuild := true // true if you cross-build the project for multiple Scala versions
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  runClean,
  //runTest,
  releaseStepCommand("publishSigned")
)
