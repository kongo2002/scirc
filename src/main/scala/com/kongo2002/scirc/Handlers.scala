package com.kongo2002.scirc

import akka.actor.Actor
import akka.util.ByteString

import Response._

object Handlers {

  abstract trait BaseHandler {
    this: ClientActor =>

    val empty = Right(EmptyResponse)

    def success(response: String) = Right(StringResponse(response))

    def noop(op: Operation, client: Client): Response = empty

    def hostReply(reply: String) =
      success(s":${server.host} $reply")
  }

  trait NickHandler extends BaseHandler {
    this: ClientActor =>

    import NickManager._

    def handleNick(op: Operation, client: Client): Response = {
      val newNick = op.get(0)
      val nick = ctx.nick

      if (nick != newNick) {
        val isNew = nick == ""

        if (isNew)
          nickManager ! RegisterNick(newNick, client)
        else
          nickManager ! ChangeNick(nick, newNick, client)
      }

      empty
    }

    def nickReceive: Receive = {
      case NickAck(newNick, client) =>
        ctx.nick = newNick
      case NickErr(err, client) =>
        sendError(StringError(err), sendTo(client))
    }
  }

  trait UserHandler extends BaseHandler {
    this: ClientActor =>

    def handleUser(op: Operation, client: Client): Response = {
      ctx.user = op.get(0)
      ctx.realname = op.get(3)
      ctx.modes = op.getInt(1).getOrElse(0)

      // TODO: version
      val version = "scirc-0.1"
      val host = server.host
      val nick = ctx.nick
      val df = java.text.DateFormat.getDateInstance()
      val tz = java.util.TimeZone.getTimeZone("UTC")
      val time = df.format(server.created)

      Right(ListResponse(List(
        ReplyWelcome(s"Welcome to the Internet Relay Network $nick!${ctx.user}@$host"),
        ReplyYourHost(s"Your host is $host, running version $version"),
        ReplyCreated(s"This server was created $time"),
        // TODO: modes
        ReplyMyInfo(s"$host $version o o")
        )))
    }
  }

  trait JoinHandler extends BaseHandler {
    this: ClientActor =>

    import ChannelActor._
    import ChannelManager._

    def handleJoin(op: Operation, client: Client): Response = {
      // TODO: handle multiple channels
      // TODO: handle keys
      var channel = op.get(0)
      channelManager ! ChannelJoin(channel, ctx.nick, client)
      empty
    }

    def joinReceive: Receive = {
      case ChannelJoined(ch, topic, client) =>
        topic match {
          case "" => sendResponse(ReplyNoTopic(ch), sendTo(client))
          case _ => sendResponse(ReplyTopic(ch, topic), sendTo(client))
        }
    }
  }

  trait IsonHandler extends BaseHandler {
    this: ClientActor =>

    import NickManager._

    def handleIson(op: Operation, client: Client): Response = {
      nickManager ! OnlineNicks(op.args.toList, client)
      empty
    }

    def isonReceive: Receive = {
      case NicksOnline(ns, rec) =>
        sendResponse(ReplyIson(ns.mkString(" ")), (sendTo(rec)))
    }
  }

  trait PingHandler extends BaseHandler {
    this: ClientActor =>

    def handlePing(op: Operation, client: Client): Response = {
      val host = server.host
      hostReply(s"PONG $host :$host")
    }
  }

  trait QuitHandler extends BaseHandler {
    this: ClientActor =>

    import ChannelManager._
    import NickManager._

    def disconnect(client: Client) {
      // terminate itself
      context stop self

      // unregister nick
      channelManager ! ChannelJoin("0", ctx.nick, client)
      nickManager ! DisconnectNick(ctx.nick)
    }

    def handleQuit(op: Operation, client: Client): Response = {
      val msg = op.get(0, "leaving")

      disconnect(client)

      Left(StringError(s"QUIT :$msg"))
    }
  }

}

abstract trait CommandHandler {
  def handleCommand(cmd: String): Response
  implicit val ctx: ClientContext
}

