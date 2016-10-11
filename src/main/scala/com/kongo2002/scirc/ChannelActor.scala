/* Copyright 2015 Gregor Uhlenheuer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.kongo2002.scirc

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.ByteString
import com.kongo2002.scirc.errors.UnexpectedException

object ChannelActor {
  def props(name: String, channelManager: ActorRef, server: ServerContext): Props =
    Props(new ChannelActor(name, channelManager, server))

  // request messages
  case class UserJoin(nick: String, creator: Boolean, key: String, client: Client)
  case class UserPart(nick: String, reason: String, client: Client)
  case class UserQuit(nick: String, reason: String, client: Client)
  case class UserKick(nick: String, reason: String, client: Client)
  case class SetChannelModes(channel: String, modes: Array[String], client: Client)
  case class GetChannelModes(channel: String, client: Client)
  case class WhoQuery(channel: String, opOnly: Boolean, client: Client)
  case class UserChannels(nick: String, client: Client)
  case class UserInChannel(nick: String)
  case class SetTopic(channel: String, topic: String, client: Client)
  case class GetTopic(channel: String, client: Client)
  case class ChannelTopics(client: Client)
  case class ChannelTopic(client: Client)

  // response messages
  case class ChannelJoined(channel: String, topic: String, created: java.util.Date, names: List[String], client: Client)
  case class ChannelModes(channel: String, modes: String, client: Client)
  case class UserInChannels(channels: List[String], client: Client)
}

class ChannelActor(name: String, channelManager: ActorRef, server: ServerContext)
  extends Actor
  with ActorLogging
  with SendActor {

  import ChannelActor._
  import ClientActor._
  import Modes._
  import NickManager._
  import Response._

  var topic = ""
  var members = Map.empty[String, Client]

  var modes = Modes.ChannelModeSet()

  // TODO: a persisted channel might be created earlier
  val created = java.util.Calendar.getInstance().getTime

  def join(nick: String, client: Client): Boolean = {
    if (!members.contains(nick)) {
      members += (nick -> client)
      true
    } else {
      false
    }
  }

  def part(nick: String): Boolean =
    kick(nick).isDefined

  def kick(nick: String): Option[Client] =
    members.get(nick).map { n =>
      members -= nick
      n
    }

  def sendToAll(msg: String): Unit = {
    val bytes = ByteString(msg)
    members.values.foreach (send(bytes))
  }

  def partOf(nick: String): Boolean =
    members.contains(nick)

  def checkKey(key: String) = {
    modes.getArgs(ChannelKeyMode) match {
      case List(k) if k == key => true
      // no key defined at all
      case Nil => true
      case _ => false
    }
  }

  def handleModeResult(op: ModeOperation, client: Client) = {
    op.t match {
      // list requests
      case ModeOperationType.ListMode =>
        op.mode match {
          case BanMaskMode =>
            op.args foreach { mask =>
              client ! Msg(ReplyBanList(name, mask, client.ctx), client)
            }
            client ! Msg(ReplyEndOfBanList(name, client.ctx), client)
          case ExceptionMaskMode =>
            op.args foreach { mask =>
              client ! Msg(ReplyExceptList(name, mask, client.ctx), client)
            }
            client ! Msg(ReplyEndOfExceptList(name, client.ctx), client)
          case InvitationMaskMode =>
            op.args foreach { mask =>
              client ! Msg(ReplyInviteList(name, mask, client.ctx), client)
            }
            client ! Msg(ReplyEndOfInviteList(name, client.ctx), client)
          case unexpected =>
            throw new UnexpectedException(s"unexpected list mode operation $unexpected")
        }

      // set and unset operations may be handled similarly
      case ModeOperationType.SetMode | ModeOperationType.UnsetMode =>
        val modeStr = toModeString(op)

        sendToAll(s":${client.ctx.prefix} MODE $name $modeStr\r\n")
        log.info(s"MODE operation: $modeStr")
    }
  }

  private def nickName(nick: String) = {
    val isOp = modes.isOp(nick) || modes.isCreator(nick)
    val isVoice = !isOp && modes.isVoice(nick)

    if (isOp) s"@$nick"
    else if (isVoice) s"+$nick"
    else nick
  }

  private def asOperator(client: Client)(func: => Unit) {
    // check if the issuer is part of the channel at all
    if (!members.contains(client.ctx.nick)) {
      client ! Err(ErrorNotOnChannel(name, client.ctx), client)
    }
    // after that check the necessary operator privileges
    else if (!modes.isOp(client.ctx.nick)) {
      client ! Err(ErrorChannelOperatorPrivilegeNeeded(name, client.ctx), client)
    } else {
      func
    }
  }

  def receive: Receive = {

    case UserJoin(nick, creator, key, client) =>
      if (!checkKey(key))
        client ! Err(ErrorBadChannelKey(name, client.ctx), client)
      else if (join(nick, client)) {
        // if this is a freshly created channel set the 'creator' flag
        if (creator) {
          modes = modes.applyModes(List("+Oo", nick, nick))._1
        }

        // notify the client that he successfully joined the channel
        val names = members.keys.map(nickName).toList
        client ! ChannelJoined(name, topic, created, names, client)

        // notify all members of member join
        sendToAll(s":${client.ctx.prefix} JOIN $name\r\n")
      }

    case UserPart(nick, reason, client) =>
      if (part(nick)) {
        // 'nick' is not in the channel anymore
        // that's why the message won't be sent with 'toAll'
        client ! Msg(HostReply(s"PART $name :$reason\r\n", client.ctx), client)

        sendToAll(s":${client.ctx.prefix} PART $name :$reason\r\n")
      }
      // user is not on the requested channel
      else {
        client ! Err(ErrorNotOnChannel(name, client.ctx), client)
      }

    case UserQuit(nick, reason, client) =>
      if (part(nick))
        sendToAll(s":${client.ctx.prefix} QUIT :$reason\r\n")

    case UserKick(nick, reason, client) =>
      asOperator(client) {
        kick(nick) match {
          case Some(user) =>
            val msg = s":${client.ctx.prefix} KICK $name $nick :$reason\r\n"

            // notify kicked user
            send(ByteString(msg))(user)

            // notify rest of the channel
            sendToAll(msg)

          // user is not on the requested channel
          case None =>
            client ! Err(ErrorUserNotInChannel(nick, name, client.ctx), client)
        }
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
        client ! Err(ErrorCannotSendToChannel(name, client.ctx), client)
      }

    case GetChannelModes(channel, client) =>
      client ! ChannelModes(channel, modes.modeString, client)

    case SetChannelModes(channel, args, client) =>
      val (newModes, applied) = modes.applyModes(args)
      modes = newModes
      applied foreach (handleModeResult(_, client))

    case ChangeNick(oldNick, newNick, client) =>
      members.get(oldNick) foreach { nick =>
        val from = s"$oldNick!${client.ctx.user}@${client.ctx.host}"
        val msg = ByteString(s":$from NICK $newNick\r\n")

        // send notification to participating channels first
        members.foreach { case (nick, cl) =>
          if (nick != oldNick)
            send(msg)(cl)
        }

        // change nick in members
        members -= oldNick
        members += (newNick -> client)
      }

    case UserInChannel(nick) =>
      if (members.contains(nick)) {
        // determine user's special role(s)
        val chName =
          if (modes.isOp(nick))
            "@" + name
          else if (modes.isVoice(nick))
            "+" + name
          else
            name

        sender ! ChannelGatherer.JobResult(chName)
      }
      else
        sender ! ChannelGatherer.NoResult

    case GetTopic(channel, client) =>
      client ! Msg(ReplyTopic(name, topic, client.ctx), client)

    case ChannelTopic(client: Client) =>
      // TODO: select visible members only
      val visible = members.size
      sender ! ChannelGatherer.JobResult(ReplyList(name, visible, topic, client.ctx))

    case SetTopic(channel, topc, client) =>
      // operator privilege needed
      asOperator(client) {
        // update topic
        this.topic = topc

        // notify channel members
        sendToAll(s":${client.ctx.prefix} TOPIC $name :$topc\r\n")
      }

    case WhoQuery(channel, opOnly, client) =>
      members.values.foreach { c =>
        // TODO: evaluate 'opOnly'

        val info = Info.UserWhoInfo(c.ctx,
          channel,
          away = false, // TODO: away
          "H" // TODO: modes
        )

        client ! Msg(ReplyWho(channel, info, client.ctx), client)
      }

      // send end of list
      client ! Msg(ReplyEndOfWho(channel, client.ctx), client)
  }
}
