name := "scala-blur"

version := "0.1"

scalaVersion := "2.13.6"
scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")
libraryDependencies ++= Seq(
  "com.storm-enroute" %% "scalameter-core" % "0.19",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0",
)
