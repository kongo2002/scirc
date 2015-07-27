package com.kongo2002.scirc

import akka.actor.{Actor, ActorRef, ActorLogging}
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
  with ActorLogging
  with SendActor {

  import ClientActor._
  import ChannelActor._
  import NickManager._
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

  def partOf(nick: String) = !members.get(nick).isEmpty

  def receive: Receive = {

    case UserJoin(nick, client) =>
      if (join(nick, client)) {
        // notify the client that he successfully joined the channel
        val names = members.keys.toList
        client.client ! ChannelJoined(name, topic, names, client)

        // notify all members of member join
        toAll(s":${client.ctx.prefix} JOIN $name\r\n")
      }

    case UserPart(nick, reason, client) =>
      if (part(nick)) {
        toAll(s":${client.ctx.prefix} PART $name :$reason\r\n")
      }
      // user is not on the requested channel
      else {
        client.client ! Err(ErrorNotOnChannel(name), client)
      }

    case PrivMsg(rec, text, from, client) =>
      // check if the user is part of the channel
      if (partOf(from)) {
        val msg = ByteString(s":$from PRIVMSG $name :$text\r\n")

        members.foreach { case (nick, cl) =>
          if (nick != client.ctx.nick)
            send(msg)(cl)
        }
      }
      // user is not allowed to send on this channel
      else {
        // TODO: or 401?
        client.client ! Err(ErrorCannotSendToChannel(name), client)
      }

    case ChangeNick(oldNick, newNick, client) =>
      members.get(oldNick) match {
        case Some(c) =>
          val from = s"$oldNick!${client.ctx.user}@${client.ctx.host}"
          val msg = ByteString(s":$from NICK $newNick\r\n")

          // send notification to participating channels first
          members.foreach { case (nick, cl) =>
            if (nick != oldNick)
              send(msg)(cl)
          }

          // change nick in members
          members -= oldNick
          members += (newNick -> c)
        case None =>
      }
  }
}
