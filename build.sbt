name := "killer-tomatoes"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-persistence" % "2.5.12",
  //"com.typesafe.akka" %% "akka-persistence-cassandra" % "0.84",
  "org.typelevel" %% "cats-core" % "1.1.0",
  //"com.typesafe.akka" %% "akka-persistence-cassandra-launcher" % "0.84" % Test,
  "com.github.dnvriend" %% "akka-persistence-inmemory" % "2.5.1.1",
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  "com.typesafe.akka" %% "akka-testkit" % "2.5.12" % Test
)