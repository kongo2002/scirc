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

import com.kongo2002.scirc.handlers._

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp

import java.net.InetSocketAddress

case class Client(client: ActorRef, socket: ActorRef, ctx: ClientContext)

object ClientActor {
  def apply(server: ServerContext, remote: InetSocketAddress, nickManager: ActorRef, channelManager: ActorRef) =
    new ClientActor(server, remote, nickManager, channelManager)

  case class PrivMsg(recipient: String, text: String, from: String, client: Client)
}

class ClientActor(val server: ServerContext,
    remote: InetSocketAddress,
    val nickManager: ActorRef,
    val channelManager: ActorRef)
  extends CommandProcessor
  with NickHandler
  with PingHandler
  with UserHandler
  with IsonHandler
  with JoinHandler
  with QuitHandler
  with PartHandler
  with WhoIsHandler
  with ModeHandler
  with PrivMsgHandler
  with TopicHandler
  with ListHandler
  with WhoHandler
  with MotdHandler
  with CommandHandler {

  import Commands._
  import ChannelManager._
  import NickManager._
  import Response._

  val remoteHost = remote.getHostString()
  implicit val ctx = ClientContext(server, remoteHost, "")

  def welcome = {
    // TODO: version
    val version = "scirc-0.1"
    val host = server.host
    val nick = ctx.nick
    val df = java.text.DateFormat.getDateInstance()
    val tz = java.util.TimeZone.getTimeZone("UTC")
    val time = df.format(server.created)

    ListResponse(List(
      ReplyWelcome(s"Welcome to the Internet Relay Network $nick!${ctx.user}@$host"),
      ReplyYourHost(s"Your host is $host, running version $version"),
      ReplyCreated(s"This server was created $time"),
      // TODO: ISUPPORT: <http://www.irc.org/tech_docs/005.html>
      // TODO: LUSERS
      ReplyMyInfo(s"$host $version o o"),
      getMotd
      ))
  }

  protected def getConfigString(path: String): String = {
    val config = context.system.settings.config
    val fullPath = s"com.kongo2002.scirc.$path"

    config.getString(fullPath)
  }

  def handle(op: Operation): Response = {
    val client = Client(self, sender, ctx)
    val handler = op.cmd match {
      case PingCmd    => handlePing _
      case NickCmd    => handleNick _
      case QuitCmd    => handleQuit _
      case UserCmd    => handleUser _
      case IsonCmd    => handleIson _
      case JoinCmd    => handleJoin _
      case PartCmd    => handlePart _
      case PrivMsgCmd => handlePrivMsg _
      case WhoIsCmd   => handleWhois _
      case ModeCmd    => handleMode _
      case WhoCmd     => handleWho _
      case TopicCmd   => handleTopic _
      case MotdCmd    => handleMotd _
      case ListCmd    => handleList _
      case PongCmd    => noop _
    }

    handler(op, client)
  }

  def handleCommand(cmd: String): Response = {
    parseCommand(cmd).right.flatMap(handle)
  }

  def handleClose: Receive = {
    case Tcp.PeerClosed =>
      println("Connection closed")

      disconnect(Client(self, sender, ctx), "leaving", true)

    case Tcp.Closed =>
      println("Connection closed")
  }

  def handleReply: Receive = {
    case Err(e, client) => sendMsg(e, sendTo(client))
    case Msg(msg, client) => sendMsg(msg, sendTo(client))
  }

  def receive =
    httpReceive  orElse
    joinReceive  orElse
    userReceive  orElse
    nickReceive  orElse
    modeReceive  orElse
    isonReceive  orElse
    whoisReceive orElse
    handleReply  orElse
    handleClose
}
