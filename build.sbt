lazy val root = (project in file(".")).
  settings(
    name := "QueryDSL",
    version := "1.0",
    scalaVersion := "2.11.7"
  ).
  settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.6" % "test"
    )
  )
