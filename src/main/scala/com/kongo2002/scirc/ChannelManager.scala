package com.kongo2002.scirc

import akka.actor.{Actor, ActorRef, Props}

import scala.collection.mutable.Map

object ChannelManager {
  def apply(server: ServerContext) =
    new ChannelManager(server)

  // request messages
  case class ChannelJoin(channel: String, nick: String, client: Client)
  case class ChannelPart(channel: String, nick: String, reason: String, client: Client)
}

class ChannelManager(server: ServerContext)
  extends Actor
  with SendActor {

  import ChannelActor._
  import ChannelManager._
  import Response._

  val channels = Map.empty[String, ActorRef]

  // TODO: import persisted/saved channels

  def join(channel: String, nick: String, client: Client) = {
    channels.get(channel) match {
      // channel already exists -> just join
      case Some(c) =>
        c ! UserJoin(nick, client)
      // new channel -> create a new one
      case None =>
        val newChannel = context.actorOf(
          Props(ChannelActor(channel, self, server)))

        channels += (channel -> newChannel)
        newChannel ! UserJoin(nick, client)
    }
  }

  def partAll(nick: String, reason: String, client: Client) = {
    channels.values.foreach { c =>
      c ! UserPart(nick, reason, client)
    }
  }

  def receive: Receive = {

    case ChannelJoin(channel, nick, client) =>
      if (channel == "0")
        partAll(nick, "leaving", client)
      else
        join(channel, nick, client)

    case ChannelPart(channel, nick, reason, client) =>
      channels.get(channel) match {
        case Some(c) =>
          c ! UserPart(nick, reason, client)
        case None =>
          client.client ! Err(ErrorNoSuchChannel(channel), client)
      }
  }
}
