package com.kongo2002.scirc

import akka.actor.{Actor, ActorSystem, Props}
import akka.event.LoggingReceive
import akka.io.{IO, Tcp}

import scala.concurrent.duration._

import java.net.{InetSocketAddress, URI}

class Server(listen: URI) extends Actor {
  val host = listen.getHost
  val port = listen.getPort

  IO(Tcp)(context.system) ! Tcp.Bind(self, new InetSocketAddress(host, port))

  def receive: Receive = LoggingReceive {
    case Tcp.Connected(_, _) =>
      sender ! Tcp.Register(context.actorOf(Props[Client]))
  }
}

object Server {
  def apply(port: Int) = {
    val uri = new URI(s"http://127.0.0.1:$port")
    new Server(uri)
  }

  def getPort(str: Array[String]) = {
    str.toList match {
      case h :: _ => h.toInt
      case _ => 9999
    }
  }

  def main(args: Array[String]) = {
    val port = getPort(args)

    val system = ActorSystem("scirc")
    val server = system.actorOf(Props(Server(port)), "server")

    println(s"scirc running on port $port")

    system.awaitTermination(Duration.Inf)
  }
}
