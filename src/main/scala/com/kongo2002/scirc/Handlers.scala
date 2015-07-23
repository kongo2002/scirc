package com.kongo2002.scirc

import akka.actor.Actor
import akka.util.ByteString

import scala.util.{Try, Success, Failure}

object Handlers {
  trait Handler {
    def handle(op: Operation): Either[String, String]
  }

  def makeHandler(handler: Operation => Either[String, String]) =
    new Handler {
      def handle(op:Operation) = handler(op)
    }

  val noop = makeHandler { _ => Right("OK") }
  val ping = makeHandler { _ => Right("PONG") }
  val nick = noop
}

trait CommandHandler {
  this: Actor =>

  import Commands._
  import Handlers._

  def handle(op: Operation): Either[String, String] = {
    op.cmd match {
      case PingCmd => ping.handle(op)
      case NickCmd => nick.handle(op)
    }
  }

  def handleCommand(cmd: String): Either[String, String] = {
    parseCommand(cmd).right.flatMap(handle)
  }
}

