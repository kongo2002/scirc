package com.kongo2002.scirc

import akka.actor.Actor
import akka.io.Tcp
import akka.util.ByteString

trait SendActor extends Actor {
  def send(msg: ByteString)(client: Client) {
    client.socket ! Tcp.Write(msg)
  }

  def send(msg: String)(client: Client) {
    send(ByteString(msg))(client)
  }
}
