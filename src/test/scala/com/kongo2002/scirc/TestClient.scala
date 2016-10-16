package com.kongo2002.scirc

import java.net.InetSocketAddress

import akka.actor.{ ActorSystem, Props }
import akka.io.Tcp
import akka.testkit.{ ImplicitSender, TestKit }
import akka.util.ByteString
import org.scalatest.Matchers

abstract class TestClient(systemName: String) extends TestKit(ActorSystem(systemName))
    with ImplicitSender with Matchers {

  val ctx = ServerContext("test.localhost", 0)
  val remote = new InetSocketAddress("localhost", 6667)

  val nickManager = system.actorOf(Props[NickManager], "nickmanager")
  val channelManager = system.actorOf(ChannelManager.props(ctx), "channelmanager")

  val client = system.actorOf(ClientActor.props(ctx, remote, nickManager, channelManager))

  def write(str: String) = client ! Tcp.Received(ByteString(str))

  def send(str: String) = write(s"$str\r\n")

  def sendAck[T](str: String)(expect: PartialFunction[Any, T]): T = {
    send(str)
    expectMsgPF()(expect)
  }

  def sendExpect(str: String)(expected: String => Boolean) = {
    sendAck(str) {
      case Tcp.Write(x, _) =>
        val got = x.decodeString("utf8")
        expected(got) should be(true)
    }
  }

  def sendExpect(str: String, expected: String) = {
    sendAck(str) {
      case Tcp.Write(x, _) =>
        val got = x.decodeString("utf8")
        got should be(expected)
    }
  }
}
