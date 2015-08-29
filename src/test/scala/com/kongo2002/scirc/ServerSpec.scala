package com.kongo2002.scirc

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.io.{Tcp, IO}
import akka.testkit.{TestKit, ImplicitSender}
import org.scalatest.{WordSpecLike, Matchers}

class ServerSpec extends TestKit(ActorSystem("ServerSpec"))
  with ImplicitSender with WordSpecLike with Matchers {

  "Server" should {
    "handle connections" in {
      val port = 6667
      val host = "test.localhost"
      val remote = new InetSocketAddress("localhost", port)

      system.actorOf(Server.props(host, port))
      val client = IO(Tcp)(system)

      client ! Tcp.Connect(remote)

      expectMsgType[Tcp.Connected]

      client ! Tcp.Close
    }
  }
}
