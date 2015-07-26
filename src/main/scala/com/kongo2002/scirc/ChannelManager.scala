package com.kongo2002.scirc

import akka.actor.{Actor, ActorRef}

import scala.collection.mutable.Map

object ChannelManager {
  // request messages
  case class ChannelJoin(channel: String, nick: String, rec: ActorRef)

  // response messages
  case class ChannelJoined(channel: String, topic: String, rec: ActorRef)
}

class ChannelManager extends Actor {

  import ChannelManager._

  val channels = Map.empty[String, ChannelInfo]

  // TODO: import persisted/saved channels

  def join(channel: String, nick: String, rec: ActorRef) = {
    channels.get(channel) match {
      // channel already exists -> just join
      case Some(c) =>
        if (c.join(nick, sender))
          sender ! ChannelJoined(channel, c.topic, rec)
      // new channel -> create a new one
      case None =>
        val newChannel = new ChannelInfo(channel)
        newChannel.join(nick, sender)
        sender ! ChannelJoined(channel, newChannel.topic, rec)
    }
  }

  def partAll(nick: String, rec: ActorRef) = {
    channels.values.foreach { c =>
      if (c.part(nick)) {
        // TODO: notify channel
      }
    }
  }

  def receive: Receive = {
    case ChannelJoin(channel, nick, rec) =>
      if (channel == "0")
        partAll(nick, rec)
      else
        join(channel, nick, rec)
  }
}

class ChannelInfo(name: String) {
  var topic = ""
  val members = Map.empty[String, ActorRef]

  def join(nick: String, ref: ActorRef) = {
    if (!members.contains(nick)) {
      members += (nick -> ref)
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
}
