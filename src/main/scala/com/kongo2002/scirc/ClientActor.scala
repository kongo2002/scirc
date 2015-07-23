package com.kongo2002.scirc

import akka.actor.Actor

import Handlers._

object ClientActor {
  def apply(host: String) = new ClientActor(host)
}

class ClientActor(val host: String)
 extends CommandActor
 with NickHandler
 with PingHandler
 with CommandHandler {

  import Commands._
  import Response._

  var nick = "unknown"

  def handle(op: Operation): Response = {
    op.cmd match {
      case PingCmd => handlePing(op)
      case NickCmd => handleNick(op)
      case PongCmd => noop(op)
    }
  }

  def handleCommand(cmd: String): Response = {
    parseCommand(cmd).right.flatMap(handle)
  }

  // use HTTP handling of the CommandActor for now
  def receive = httpReceive
}
