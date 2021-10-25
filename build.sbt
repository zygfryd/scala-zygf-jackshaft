name := "jackshaft"

version := "0.2.2"

organization := "net.zygfryd"
organizationName := "zygfryd's projects"
organizationHomepage := Some(url("https://zygfryd.net/"))

description := "Incremental (non-blocking) parsing for Scala JSON ASTs, based on jackson-core."

licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))

homepage := Some(url("https://github.com/zygfryd/scala-zygf-jackshaft"))

scmInfo := Some(ScmInfo(url("https://github.com/zygfryd/scala-zygf-jackshaft"),
                        "scm:git@github.com:zygfryd/scala-zygf-jackshaft.git"))

val scala211 = "2.11.12"
val scala212 = "2.12.15"
val scala213 = "2.13.6"
val scala3xx = "3.1.0"
val supportedScalaVersions = List(scala212, scala213, scala211, scala3xx)

scalaVersion := scala3xx
crossScalaVersions := supportedScalaVersions

libraryDependencies ++= Seq("com.fasterxml.jackson.core" % "jackson-core" % "2.10.5",
                            
                            ("io.spray" %% "spray-json" % "1.3.6" % "provided").cross(CrossVersion.for3Use2_13),
                            ("com.typesafe.akka" %% "akka-stream" % "2.5.32" % "provided").cross(CrossVersion.for3Use2_13),
                            ("com.typesafe.akka" %% "akka-http" % "10.1.14" % "provided").cross(CrossVersion.for3Use2_13),
                            
                            "org.scalatest" %% "scalatest" % "3.2.9" % "test")

scalacOptions ++= Seq("-deprecation",
                      "-feature",
                      "-unchecked",
                      "-Xfatal-warnings")

// javacOptions += "-Xlint:unchecked"

scalacOptions ++= {
  if (scalaVersion.value.startsWith("2"))
    Seq("-Xlint:inaccessible",
        "-Xlint:infer-any",
        "-Xlint:missing-interpolator",
        "-Xlint:option-implicit",
        "-Xlint:poly-implicit-overload",
        "-Xlint:type-parameter-shadow")
  else
    Seq("-old-syntax",
        "-no-indent")
}

scalacOptions ++= {
  if (scalaBinaryVersion.value == "2.12" || scalaBinaryVersion.value == "2.13")
    Seq("-opt:l:inline",
        "-opt-inline-from:zygf.**")
  else
    Seq()
}

javacOptions ++= Seq("-source", "8",
                     "-target", "8")

