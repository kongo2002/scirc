package com.kongo2002.scirc

import akka.actor.Actor
import akka.util.{ByteString, Timeout}
import akka.pattern.ask

import scala.concurrent.duration._

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
        val oldNick = ctx.nick
        val isNew = oldNick == ""

        // update to new nick
        ctx.nick = newNick

        // if this is a new nick this is probably the registration phase
        // but it could be a second try caused by a duplicate nick name
        if (isNew)
          self ! Registered(client)
        else {
          // notify client itself
          val msg = s":$oldNick!${ctx.user}@${ctx.host} NICK $newNick"
          sendResponse(StringResponse(msg), sendTo(client))

          // notify channels
          channelManager ! ChangeNick(oldNick, newNick, client)
        }

      case NickErr(err, client) =>
        sendError(err, sendTo(client))
    }
  }

  trait WhoIsHandler extends BaseHandler {
    this: ClientActor =>

    import NickManager._

    def args(xs: Array[String]) = xs.toList match {
      case List(target, mask) => (target, mask.split(","))
      case mask :: _ => ("", mask.split(","))
    }

    def handleWhois(op: Operation, client: Client): Response = {
      // TODO: respect target
      val (target, mask) = args(op.args)

      nickManager ! WhoIsNicks(mask, client)
      empty
    }

    def whoisReceive: Receive = {
      case WhoIs(client: Client) =>
        val nick = ctx.nick

        val reply = ListResponse(List(
          ReplyWhoIsUser(nick, ctx.user, ctx.host, ctx.realname),
          ReplyWhoIsServer(nick, ctx.ctx.host, ctx.ctx.host),

          // TODO: channel list
          ReplyEndOfWhoIsList(nick)
        ))

        sendResponse(reply, sendTo(client))
    }
  }

  trait PrivMsgHandler extends BaseHandler {
    this: ClientActor =>

    import ClientActor._

    def handlePrivMsg(op: Operation, client: Client): Response = {
      val rec = op.get(0)
      val text = op.get(1)

      if (rec == "")
        Left(ErrorNoRecipient("PRIVMSG"))
      else if (text == "")
        Left(ErrorNoTextToSend)
      else {
        channelManager ! PrivMsg(rec, text, ctx.nick, client)
        empty
      }
    }
  }

  trait UserHandler extends BaseHandler {
    this: ClientActor =>

    import NickManager._

    def handleUser(op: Operation, client: Client): Response = {
      ctx.user = op.get(0)
      ctx.realname = op.get(3)
      ctx.modes = op.getInt(1).getOrElse(0)

      self ! Registered(client)

      empty
    }

    def userReceive: Receive = {
      case Registered(client) =>
        if (!ctx.isRegistered && ctx.nick != "" && ctx.user != "") {
          ctx.isRegistered = true
          sendResponse(welcome, sendTo(client))
        }
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
      case ChannelJoined(ch, topic, names, client) =>
        topic match {
          case "" => sendResponse(ReplyNoTopic(ch), sendTo(client))
          case _ => sendResponse(ReplyTopic(ch, topic), sendTo(client))
        }

        // send list of names
        val nameStr = names.mkString(" ")
        sendResponse(ReplyChannelNames(ch, nameStr), sendTo(client))
        sendResponse(ReplyEndOfNames(ch), sendTo(client))
    }
  }

  trait PartHandler extends BaseHandler {
    this: ClientActor =>

    import ChannelManager._

    def handlePart(op: Operation, client: Client): Response = {
      var channel = op.get(0)
      var reason = op.get(1, ctx.nick)

      channelManager ! ChannelPart(channel, ctx.nick, reason, client)
      empty
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

