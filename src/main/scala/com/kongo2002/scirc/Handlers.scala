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
      success(s":$host $reply")
  }

  trait NickHandler extends BaseHandler {
    this: ClientActor =>

    def handleNick(op: Operation): Response = {
      val newNick = op.get(0)
      if (nick != newNick) {
        nick = newNick

        // TODO
        success("OK")
      }
      else
        empty
    }
  }

  trait PingHandler extends BaseHandler {
    this: ClientActor =>

    def handlePing(op: Operation): Response = {
      hostReply(s"PONG $host :$host")
    }
  }

  trait QuitHandler extends BaseHandler {
    this: ClientActor =>

    def handleQuit(op: Operation): Response = {
      val msg = op.get(0, "Leaving.")

      // send quit
      context stop self

      Left(StringError(s"QUIT :$msg"))
    }
  }

}

trait CommandHandler {
  def handleCommand(cmd: String): Response
}

