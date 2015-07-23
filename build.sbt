name := "scirc"

version := "1.0"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= {
  val akkaV = "2.3.11"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaV
  )
}
