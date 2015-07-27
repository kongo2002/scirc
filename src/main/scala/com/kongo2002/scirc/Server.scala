package com.kongo2002.scirc

import akka.actor.{Actor, ActorSystem, Props}
import akka.event.LoggingReceive
import akka.io.{IO, Tcp}

import scala.concurrent.duration._

import java.net.{InetSocketAddress, URI}

class Server(listen: URI, hostname: String) extends Actor {
  val host = listen.getHost
  val port = listen.getPort
  val ctx = ServerContext(hostname, 0)

  var nickManager = context.system.deadLetters
  var channelManager = context.system.deadLetters

  IO(Tcp)(context.system) ! Tcp.Bind(self, new InetSocketAddress(host, port))

  override def preStart = {
    nickManager = context.actorOf(Props[NickManager], "nickmanager")
    channelManager = context.actorOf(Props(ChannelManager(ctx)), "channelmanager")
  }

  def receive: Receive = LoggingReceive {
    case Tcp.Connected(rem, _) =>
      sender ! Tcp.Register(context.actorOf(
        Props(ClientActor(ctx, rem, nickManager, channelManager))))
  }
}

object Server {
  def apply(port: Int, hostname: String) = {
    val uri = new URI(s"http://127.0.0.1:$port")
    new Server(uri, hostname)
  }

  def getPort(str: Array[String]) = {
    str.toList match {
      case h :: _ => h.toInt
      case _ => 6667
    }
  }

  def main(args: Array[String]) = {
    // TODO: configuration
    var hostname = "localhost"
    val port = getPort(args)

    val system = ActorSystem("scirc")
    val server = system.actorOf(Props(Server(port, hostname)), "server")

    println(s"scirc running on port $port")

    system.awaitTermination(Duration.Inf)
  }
}
