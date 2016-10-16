package com.kongo2002.scirc

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.io.{ IO, Tcp }
import akka.testkit.{ ImplicitSender, TestKit }
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.Eventually
import org.scalatest.{ Matchers, WordSpecLike }

import scala.concurrent.duration.DurationInt

class ServerSpec extends TestKit(ActorSystem("ServerSpec"))
    with ImplicitSender with WordSpecLike with Matchers with Eventually with BeforeAndAfterAll {

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
      }(PatienceConfig(3.seconds, 500.millis))

      client ! Tcp.Close
    }
  }

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }
}
