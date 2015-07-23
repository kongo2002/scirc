package com.kongo2002.scirc

import akka.actor.Actor
import akka.util.ByteString

import Response._

object Handlers {

  trait Handler {
    def handle(op: Operation): Response
  }

  def makeHandler(handler: Operation => Response) =
    new Handler {
      def handle(op:Operation) = handler(op)
    }

  val noop = makeHandler { _ => Right(EmptyResponse) }

  val ping = makeHandler { _ => Right(StringResponse("PONG")) }
  val pong = noop
  val nick = noop
}

trait CommandHandler {
  this: Actor =>

  import Commands._
  import Handlers._

  def handle(op: Operation): Response = {
    op.cmd match {
      case PingCmd => ping.handle(op)
      case PongCmd => pong.handle(op)
      case NickCmd => nick.handle(op)
    }
  }

  def handleCommand(cmd: String): Response = {
    parseCommand(cmd).right.flatMap(handle)
  }
}

