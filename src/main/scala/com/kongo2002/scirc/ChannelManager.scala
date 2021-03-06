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

import akka.actor.{ Actor, ActorLogging, ActorRef, Props }
import kamon.Kamon

import scala.util.matching.Regex

object ChannelManager {
  def props(server: ServerContext): Props =
    Props(new ChannelManager(server))

  // TODO: this regex is not 100% accurate
  val validChannel = new Regex("""[&#!+][&#!+]*[a-zA-Z0-9]+""")

  def isValidChannel(channel: String): Boolean = {
    channel match {
      case validChannel(_*) => true
      case _ => false
    }
  }

  // request messages
  case class ChannelJoin(channel: String, nick: String, key: String, client: Client)
  case class ChannelQuit(nick: String, reason: String, client: Client)
  case class ChannelPart(channel: String, nick: String, reason: String, client: Client)
  case class ChannelKick(channel: String, nick: String, reason: String, client: Client)
}

class ChannelManager(server: ServerContext)
    extends Actor
    with ActorLogging
    with SendActor {

  import ChannelActor._
  import ChannelManager._
  import ClientActor._
  import NickManager._
  import Response._

  var channels = Map.empty[String, ActorRef]
  val channelCounter = Kamon.metrics.minMaxCounter("channels")

  // TODO: import persisted/saved channels

  private def getChannel(channel: String): Option[ActorRef] = {
    // channel names are case-insensitive
    val ch = channel.toLowerCase
    channels.get(ch)
  }

  def join(channel: String, nick: String, key: String, client: Client) = {
    getChannel(channel) match {
      // channel already exists -> just join
      case Some(c) =>
        c ! UserJoin(nick, creator = false, key, client)
      // new channel -> create a new one
      case None =>
        val channelName = channel.toLowerCase

        if (isValidChannel(channelName)) {
          val newChannel = context.actorOf(
            ChannelActor.props(channelName, self, server)
          )

          channels += (channelName -> newChannel)
          channelCounter.increment()
          newChannel ! UserJoin(nick, creator = true, key, client)
        } else {
          client ! Err(ErrorBadChannelMask(channel, client.ctx), client)
        }
    }
  }

  private def partAll(nick: String, reason: String, client: Client) = {
    val msg = UserPart(nick, reason, client)
    channels.values.foreach { channel => channel ! msg }
  }

  private def withChannel(channel: String, client: Client)(handler: ActorRef => Unit) {
    getChannel(channel) match {
      case Some(c) =>
        handler(c)
      case None =>
        client ! Err(ErrorNoSuchChannel(channel, client.ctx), client)
    }
  }

  private def forwardToChannel(channel: String, client: Client, msg: Any) =
    withChannel(channel, client) { c => c forward msg }

  def receive: Receive = {

    case ChannelJoin(channel, nick, key, client) =>
      if (channel == "0")
        partAll(nick, "leaving", client)
      else
        join(channel, nick, key, client)

    case ChannelQuit(nick, reason, client) =>
      val msg = UserQuit(nick, reason, client)
      channels.values.foreach { channel => channel ! msg }

    case ChannelPart(channel, nick, reason, client) =>
      withChannel(channel, client) { c => c ! UserPart(nick, reason, client) }

    case ChannelKick(channel, nick, reason, client) =>
      withChannel(channel, client) { c => c ! UserKick(nick, reason, client) }

    case UserChannels(nick, client) =>
      val userSender = sender

      if (channels.nonEmpty) {
        ChannelGatherer(channels.values, client, UserInChannel(nick), {
          (cs: List[String], cl: Client) =>
            userSender ! UserInChannels(cs, client)
        })
      } else {
        // no active channels -> no need to gather anything at all
        userSender ! UserInChannels(Nil, client)
      }

    case ChannelTopics(client: Client) =>
      if (channels.nonEmpty) {
        ChannelGatherer(channels.values, client, ChannelTopic, {
          (cs: List[ReplyList], cl: Client) =>
            cl ! Msg(ListResponse(cs :+ ReplyEndOfList(client.ctx), client.ctx), client)
        })
      } else {
        client ! Msg(ReplyEndOfList(client.ctx), client)
      }

    case msg @ SetTopic(channel, _, client) =>
      forwardToChannel(channel, client, msg)

    case msg @ GetChannelModes(channel, client) =>
      forwardToChannel(channel, client, msg)

    case msg @ SetChannelModes(channel, _, client) =>
      forwardToChannel(channel, client, msg)

    case msg @ WhoQuery(channel, _, client) =>
      forwardToChannel(channel, client, msg)

    case msg: ChangeNick =>
      channels.values.foreach { _ forward msg }

    case msg @ PrivMsg(rec, _, _, _) =>
      getChannel(rec) foreach { _ forward msg }
  }
}
