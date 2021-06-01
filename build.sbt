name := "red-book"

version := "0.1"

scalaVersion := "2.13.1"

//libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.7"
libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.2.7" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.1" % "test")
