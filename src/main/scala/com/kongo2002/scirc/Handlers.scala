package com.kongo2002.scirc

import akka.actor.Actor
import akka.util.ByteString

import Response._

object Handlers {

  abstract trait BaseHandler {
    def success(response: String) = Right(StringResponse(response))
    def noop(op: Operation): Response = Right(EmptyResponse)
  }

  trait NickHandler extends BaseHandler {
    this: ClientActor =>

    def handleNick(op: Operation): Response = {
      Right(EmptyResponse)
    }
  }

  trait PingHandler extends BaseHandler {
    this: ClientActor =>

    def handlePing(op: Operation): Response = {
      Right(StringResponse(s":$host PONG $host :$host"))
    }
  }

}

trait CommandHandler {
  def handleCommand(cmd: String): Response
}

