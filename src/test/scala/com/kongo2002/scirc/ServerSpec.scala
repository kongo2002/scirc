package com.kongo2002.scirc

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.io.{ IO, Tcp }
import akka.testkit.{ ImplicitSender, TestKit }
import org.scalatest.concurrent.Eventually
import org.scalatest.{ Matchers, WordSpecLike }

class ServerSpec extends TestKit(ActorSystem("ServerSpec"))
    with ImplicitSender with WordSpecLike with Matchers with Eventually {

  "Server" should {
    "handle connections" in {
      val port = 6667
      val host = "test.localhost"
      val remote = new InetSocketAddress("localhost", port)

      system.actorOf(Server.props(host, port))
      val client = IO(Tcp)(system)

      // server appears to take a few moments until accepting connections
      eventually {
        client ! Tcp.Connect(remote)
        expectMsgType[Tcp.Connected]
      }

      client ! Tcp.Close
    }
  }
}
