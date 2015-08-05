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

import scala.collection.mutable.Map
import scala.concurrent.duration._
import scala.util.matching.Regex

object ChannelManager {
  def apply(server: ServerContext) =
    new ChannelManager(server)

  // TODO: this regex is not 100% accurate
  val valid = new Regex("""[&#!+][&#!+]*[a-zA-Z0-9]+""")

  def isValidChannel(channel: String): Boolean = {
    channel match {
      case valid(_*) => true
      case _ => false
    }
  }

  // request messages
  case class ChannelJoin(channel: String, nick: String, client: Client)
  case class ChannelQuit(nick: String, reason: String, client: Client)
  case class ChannelPart(channel: String, nick: String, reason: String, client: Client)
}

class ChannelManager(server: ServerContext)
  extends Actor
  with ActorLogging
  with SendActor {

  import ChannelActor._
  import ChannelGatherer._
  import ChannelManager._
  import ClientActor._
  import NickManager._
  import Response._

  val channels = Map.empty[String, ActorRef]

  // TODO: import persisted/saved channels

  private def getChannel(channel: String): Option[ActorRef] = {
    // channel names are case-insensitive
    val ch = channel.toLowerCase
    channels.get(ch)
  }

  def join(channel: String, nick: String, client: Client) = {
    getChannel(channel) match {
      // channel already exists -> just join
      case Some(c) =>
        c ! UserJoin(nick, false, client)
      // new channel -> create a new one
      case None =>
        val channelName = channel.toLowerCase

        if (isValidChannel(channelName)) {
          val newChannel = context.actorOf(
            Props(ChannelActor(channelName, self, server)))

          channels += (channelName -> newChannel)
          newChannel ! UserJoin(nick, true, client)
        } else {
          client.client ! Err(ErrorBadChannelMask(channel), client)
        }
    }
  }

  def partAll(nick: String, reason: String, client: Client) = {
    channels.values.foreach { c =>
      c ! UserPart(nick, reason, client)
    }
  }

  private def withChannel(channel: String, client: Client)(handler: ActorRef => Unit) {
    getChannel(channel) match {
      case Some(c) =>
        handler(c)
      case None =>
        client.client ! Err(ErrorNoSuchChannel(channel), client)
    }
  }

  def receive: Receive = {

    case ChannelJoin(channel, nick, client) =>
      if (channel == "0")
        partAll(nick, "leaving", client)
      else
        join(channel, nick, client)

    case ChannelQuit(nick, reason, client) =>
      channels.values.foreach { c =>
        c ! UserQuit(nick, reason, client)
      }

    case ChannelPart(channel, nick, reason, client) =>
      withChannel(channel, client) { c => c ! UserPart(nick, reason, client) }

    case UserChannels(nick, client) =>
      var userSender = sender

      if (channels.nonEmpty) {
        var gather = context.actorOf(Props(
          ChannelGatherer(channels.values, client, UserInChannel(nick), {
            (cs: List[String], cl: Client) =>
              userSender ! UserInChannels(cs, client)
            })))
      } else {
        // no active channels -> no need to gather anything at all
        userSender ! UserInChannels(List(), client)
      }

    case ChannelTopics(client: Client) =>
      if (channels.nonEmpty) {
        var gather = context.actorOf(Props(
          ChannelGatherer(channels.values, client, ChannelTopic, {
            (cs: List[ReplyList], cl: Client) =>
              cl.client ! Msg(ListResponse(cs :+ ReplyEndOfList), client)
            })))
      } else {
        client.client ! Msg(ReplyEndOfList, client)
      }

    case msg@SetTopic(channel, _, client) =>
      withChannel(channel, client) { c => c forward msg }

    case msg@GetChannelModes(channel, client) =>
      withChannel(channel, client) { c => c forward msg }

    case msg@SetChannelModes(channel, _, client) =>
      withChannel(channel, client) { c => c forward msg }

    case msg@WhoQuery(channel, _, client) =>
      withChannel(channel, client) { c => c forward msg }

    case msg@ChangeNick(_, _, _) =>
      channels.values.foreach { c =>
        c forward msg
      }

    case msg@PrivMsg(rec, _, _, _) =>
      getChannel(rec) match {
        case Some(channel) =>
          channel forward msg
        case None =>
          // do nothing for now
      }
  }
}
