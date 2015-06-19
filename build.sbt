name := "typefun"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.6"

//sbt_harden_scala.SbtHardenScalac.harden
//sbt_harden_scala.SbtHardenScalac.optimize
scalacOptions ++= Seq("-language:_", "-explaintypes", "-Xlog-implicits", "-Yrecursion", "0")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value