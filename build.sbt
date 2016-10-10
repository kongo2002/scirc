name := "scirc"

version := "1.0"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

libraryDependencies ++= {
  val akkaV = "2.4.11"
  val kamonV = "0.6.0"
  Seq(
    "com.typesafe.akka" %% "akka-actor"           % akkaV,
    "com.typesafe.akka" %% "akka-slf4j"           % akkaV,
    "ch.qos.logback"    %  "logback-classic"      % "1.1.3",

    "io.kamon"          %% "kamon-core"           % kamonV,
    "io.kamon"          %% "kamon-akka"           % kamonV,
    "io.kamon"          %% "kamon-statsd"         % kamonV,
    "io.kamon"          %% "kamon-system-metrics" % kamonV,

    "com.typesafe.akka" %% "akka-testkit"         % akkaV   % "test",
    "org.scalatest"     %% "scalatest"            % "2.2.4" % "test"
  )
}

// load aspectj sbt plugin
aspectjSettings

// add `-javaagent` to jvm startup
javaOptions in run <++= AspectjKeys.weaverOptions in Aspectj

// necessary for proper aspectj startup
fork in run := true
