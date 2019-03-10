name := "jackshaft"

version := "0.1.1"

organization := "net.zygfryd"
organizationName := "zygfryd's projects"
organizationHomepage := Some(url("https://zygfryd.net/"))

description := "Incremental (non-blocking) parsing for Scala JSON ASTs, based on jackson-core."

licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))

homepage := Some(url("https://github.com/zygfryd/scala-zygf-jackshaft"))

scmInfo := Some(ScmInfo(url("https://github.com/zygfryd/scala-zygf-jackshaft"),
                        "scm:git@github.com:zygfryd/scala-zygf-jackshaft.git"))

lazy val scala211 = "2.11.12"
lazy val scala212 = "2.12.8"
lazy val scala213 = "2.13.0-M5"
lazy val supportedScalaVersions = List(scala212, scala213, scala211)

scalaVersion := scala212
crossScalaVersions := supportedScalaVersions

libraryDependencies ++= Seq("com.fasterxml.jackson.core" % "jackson-core" % "2.9.8",
                            
                            "io.spray" %% "spray-json" % "1.3.5" % "provided",
                            "com.typesafe.akka" %% "akka-stream" % "2.5.21" % "provided",
                            "com.typesafe.akka" %% "akka-http" % "10.1.7" % "provided",
                            
                            "org.scalatest" %% "scalatest" % "3.0.6-SNAP6" % "test")

scalacOptions ++= Seq("-sourcepath", (baseDirectory in ThisBuild).value.getAbsolutePath,
                      "-deprecation",
                      "-feature",
                      "-unchecked",
                      "-language:experimental.macros",
                      "-Xfatal-warnings",
                      "-Xlint:inaccessible",
                      "-Xlint:infer-any",
                      "-Xlint:missing-interpolator",
                      "-Xlint:option-implicit",
                      "-Xlint:poly-implicit-overload",
                      "-Xlint:type-parameter-shadow") ++ {
  if (scalaBinaryVersion.value == "2.11")
    Seq()
  else
    Seq("-opt:l:inline",
        "-opt-inline-from:zygf.**")
}

javacOptions ++= Seq("-source", "8",
                     "-target", "8")

publishTo := sonatypePublishTo.value
