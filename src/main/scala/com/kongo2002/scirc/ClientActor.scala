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

import java.net.InetSocketAddress

import akka.actor.{ ActorRef, Props }
import akka.io.Tcp
import com.kongo2002.scirc.handlers._
import com.typesafe.config.Config
import kamon.Kamon

import scala.collection.JavaConverters._

/**
 * Client base case class
 * @param client client actor
 * @param socket underlying socket actor
 * @param ctx client's context information
 */
case class Client(client: ActorRef, socket: ActorRef, ctx: ClientContext) {
  def withContext(ctx: ClientContext): Client = copy(ctx = ctx)
  def !(msg: Any)(implicit sender: ActorRef) = client ! msg
}

/**
 * Client actor's companion object
 */
object ClientActor {
  def props(server: ServerContext, remote: InetSocketAddress, nickManager: ActorRef, channelManager: ActorRef) =
    Props(new ClientActor(server, remote, nickManager, channelManager))

  case class PrivMsg(recipient: String, text: String, from: String, client: Client)
}

/**
 * Main client actor class
 * @param server server context
 * @param remote remote address
 * @param nickManager associated nick manager
 * @param channelManager associated channel manager
 */
class ClientActor(
  val server: ServerContext,
  remote: InetSocketAddress,
  val nickManager: ActorRef,
  val channelManager: ActorRef
)
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
    with OperHandler
    with KickHandler
    with MotdHandler
    with CommandHandler
    with HasClientMetrics {

  import Commands._
  import Response._

  val remoteHost = remote.getHostString

  var ctx = ClientContext(server, remoteHost, "")
  val metricsName = self.toString
  val metrics = Kamon.metrics.entity(ClientMetrics, metricsName)

  def welcome = {
    val host = server.host
    val nick = ctx.nick
    val df = java.text.DateFormat.getDateInstance
    val tz = java.util.TimeZone.getTimeZone("UTC")
    df.setTimeZone(tz)
    val time = df.format(server.created)

    ListResponse(List(
      ReplyWelcome(s"Welcome to the Internet Relay Network $nick!${ctx.user}@$host", ctx),
      ReplyYourHost(s"Your host is $host, running version ${server.version}", ctx),
      ReplyCreated(s"This server was created $time", ctx),
      // TODO: ISUPPORT: <http://www.irc.org/tech_docs/005.html>
      // TODO: LUSERS
      ReplyMyInfo(s"$host ${server.version} o o", ctx),
      getMotd
    ), ctx)
  }

  protected def getConfig[T](getter: (Config, String) => T)(path: String): T = {
    val config = context.system.settings.config
    val fullPath = s"com.kongo2002.scirc.$path"

    getter(config, fullPath)
  }

  protected def getConfigString(path: String): String = {
    getConfig { (cfg, p) => cfg.getString(p) }(path)
  }

  protected def getConfigStringList(path: String): List[String] = {
    getConfig { (cfg, p) => cfg.getStringList(p).asScala.toList }(path)
  }

  def handle(op: Operation): Response = {
    val client = Client(self, sender, ctx)

    op.cmd match {
      case IsonCmd => handleIson(op, client)
      case JoinCmd => handleJoin(op, client)
      case KickCmd => handleKick(op, client)
      case ListCmd => handleList(op, client)
      case ModeCmd => handleMode(op, client)
      case MotdCmd => handleMotd(op, client)
      case NickCmd => handleNick(op, client)
      case OperCmd => handleOper(op, client)
      case PartCmd => handlePart(op, client)
      case PingCmd => handlePing(op, client)
      case PongCmd => noop(op, client)
      case PrivMsgCmd => handlePrivMsg(op, client)
      case QuitCmd => handleQuit(op, client)
      case TopicCmd => handleTopic(op, client)
      case UserCmd => handleUser(op, client)
      case WhoCmd => handleWho(op, client)
      case WhoIsCmd => handleWhois(op, client)
    }
  }

  def handleCommand(cmd: String): Response = {
    parseCommand(cmd, ctx).right.flatMap(handle)
  }

  def handleClose: Receive = {
    case Tcp.PeerClosed =>
      log.debug("Connection closed")

      disconnect(Client(self, sender, ctx), "leaving", isClosed = true)

    case Tcp.Closed =>
      log.debug("Connection closed")
  }

  def handleReply: Receive = {
    case Err(e, client) => sendMsg(e, sendTo(client))
    case Msg(msg, client) => sendMsg(msg, sendTo(client))
  }

  def receive =
    httpReceive orElse
      joinReceive orElse
      userReceive orElse
      nickReceive orElse
      modeReceive orElse
      isonReceive orElse
      whoisReceive orElse
      handleReply orElse
      handleClose

  override def postStop(): Unit = {
    ClientMetrics.removeEntity(metricsName)
    super.postStop()
  }
}
