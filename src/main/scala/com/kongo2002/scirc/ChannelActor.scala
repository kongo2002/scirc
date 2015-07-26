package com.kongo2002.scirc

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp
import akka.util.ByteString

import scala.collection.mutable.Map

object ChannelActor {
  def apply(name: String, channelManager: ActorRef, server: ServerContext) =
    new ChannelActor(name, channelManager, server)

  // request messages
  case class UserJoin(nick: String, client: Client)
  case class UserPart(nick: String, client: Client)

  // response messages
  case class ChannelJoined(channel: String, topic: String, names: List[String], client: Client)
}

class ChannelActor(name: String, channelManager: ActorRef, server: ServerContext) extends Actor {
  import ChannelActor._

  var topic = ""
  val members = Map.empty[String, Client]

  def join(nick: String, client: Client) = {
    if (!members.contains(nick)) {
      members += (nick -> client)
      true
    } else {
      false
    }
  }

  def part(nick: String) = {
    if (members.contains(nick)) {
      members -= nick
      true
    } else {
      false
    }
  }

  def send(msg: ByteString)(client: Client) {
    client.socket ! Tcp.Write(msg)
  }

  def toAll(msg: String) {
    val bytes = ByteString(msg)
    members.values.foreach (send(bytes))
  }

  def receive: Receive = {

    case UserJoin(nick, client) =>
      if (join(nick, client)) {
        // notify the client that he successfully joined the channel
        val names = members.keys.toList
        client.client ! ChannelJoined(name, topic, names, client)

        // notify all members of member join
        toAll(s":$nick!$nick@${server.host} JOIN $name\r\n")
      }

    case UserPart(nick, client) =>
      if (part(nick)) {
        // TODO: part notification
      }
  }
}
