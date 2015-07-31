name := "scirc"

version := "1.0"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

libraryDependencies ++= {
  val akkaV = "2.3.11"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "org.scalatest" %% "scalatest" % "2.2.4" % "test"
  )
}

addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.12")
