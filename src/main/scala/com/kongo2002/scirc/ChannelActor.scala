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
  case class UserPart(nick: String, reason: String, client: Client)

  // response messages
  case class ChannelJoined(channel: String, topic: String, names: List[String], client: Client)
}

class ChannelActor(name: String, channelManager: ActorRef, server: ServerContext)
  extends Actor
  with SendActor {

  import ClientActor._
  import ChannelActor._
  import Response._

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

    case UserPart(nick, reason, client) =>
      if (part(nick)) {
        toAll(s":$nick!$nick@${server.host} PART $name :$reason\r\n")
      }
      // user is not on the requested channel
      else {
        client.client ! Err(ErrorNotOnChannel(name), client)
      }

    case PrivMsg(rec, text, from, client) =>
      // TODO: 'from'
      val msg = ByteString(s":$from PRIVMSG $name :$text\r\n")

      members.foreach { case (nick, cl) =>
        if (nick != from)
          send(msg)(cl)
      }
  }
}
