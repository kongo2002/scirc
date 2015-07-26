package com.kongo2002.scirc

import akka.actor.{Actor, ActorRef, Props}

import scala.collection.mutable.Map

object ChannelManager {
  // request messages
  case class ChannelJoin(channel: String, nick: String, client: Client)
}

class ChannelManager extends Actor {

  import ChannelActor._
  import ChannelManager._

  val channels = Map.empty[String, ActorRef]

  // TODO: import persisted/saved channels

  def join(channel: String, nick: String, client: Client) = {
    channels.get(channel) match {
      // channel already exists -> just join
      case Some(c) =>
        c ! UserJoin(nick, client)
      // new channel -> create a new one
      case None =>
        val newChannel = context.actorOf(Props(ChannelActor(channel, self)))
        newChannel ! UserJoin(nick, client)
    }
  }

  def partAll(nick: String, client: Client) = {
    channels.values.foreach { c =>
      c ! UserPart(nick, client)
    }
  }

  def receive: Receive = {
    case ChannelJoin(channel, nick, client) =>
      if (channel == "0")
        partAll(nick, client)
      else
        join(channel, nick, client)
  }
}
