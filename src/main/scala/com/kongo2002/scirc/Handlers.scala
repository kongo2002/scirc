package com.kongo2002.scirc

import akka.actor.Actor
import akka.util.ByteString

import Response._

object Handlers {

  abstract trait BaseHandler {
    this: ClientActor =>

    val empty = Right(EmptyResponse)

    def success(response: String) = Right(StringResponse(response))

    def noop(op: Operation): Response = empty

    def hostReply(reply: String) =
      success(s":${server.host} $reply")
  }

  trait NickHandler extends BaseHandler {
    this: ClientActor =>

    import NickManager._

    def handleNick(op: Operation): Response = {
      val newNick = op.get(0)
      val nick = ctx.nick

      if (nick != newNick) {
        val isNew = nick == ""

        if (isNew)
          nickManager ! RegisterNick(newNick, sender)
        else
          nickManager ! ChangeNick(nick, newNick, sender)
      }

      empty
    }

    def nickReceive: Receive = {
      case NickAck(newNick, rec) =>
        ctx.nick = newNick
      case NickErr(err, rec) =>
        sendError(StringError(err), sendTo(rec))
    }
  }

  trait UserHandler extends BaseHandler {
    this: ClientActor =>

    def handleUser(op: Operation): Response = {
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

  trait PingHandler extends BaseHandler {
    this: ClientActor =>

    def handlePing(op: Operation): Response = {
      val host = server.host
      hostReply(s"PONG $host :$host")
    }
  }

  trait QuitHandler extends BaseHandler {
    this: ClientActor =>

    import NickManager._

    def handleQuit(op: Operation): Response = {
      val msg = op.get(0, "leaving")

      // terminate itself
      context stop self

      // unregister nick
      nickManager ! DisconnectNick(ctx.nick)

      Left(StringError(s"QUIT :$msg"))
    }
  }

}

abstract trait CommandHandler {
  def handleCommand(cmd: String): Response
  implicit val ctx: ClientContext
}

