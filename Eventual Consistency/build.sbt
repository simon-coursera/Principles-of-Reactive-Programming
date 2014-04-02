name := "week7-2"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3-M1"
  )

retrieveManaged := true

EclipseKeys.relativizeLibs := true

