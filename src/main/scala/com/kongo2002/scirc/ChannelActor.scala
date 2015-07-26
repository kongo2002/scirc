package com.kongo2002.scirc

import akka.actor.{Actor, ActorRef}

import scala.collection.mutable.Map

object ChannelActor {
  def apply(name: String, channelManager: ActorRef) =
    new ChannelActor(name, channelManager)

  // request messages
  case class UserJoin(nick: String, client: Client)
  case class UserPart(nick: String, client: Client)

  // response messages
  case class ChannelJoined(channel: String, topic: String, names: List[String], client: Client)
}

class ChannelActor(name: String, channelManager: ActorRef) extends Actor {
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

  def receive: Receive = {

    case UserJoin(nick, client) =>
      if (join(nick, client)) {
        val names = members.keys.toList
        client.client ! ChannelJoined(name, topic, names, client)
      }

    case UserPart(nick, client) =>
      if (part(nick)) {
        // TODO: part notification
      }
  }
}
