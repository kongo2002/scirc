package com.kongo2002.scirc

import akka.actor.{Actor, ActorRef}

import Handlers._

object ClientActor {
  def apply(server: ServerContext, nickManager: ActorRef) =
    new ClientActor(server, nickManager)
}

class ClientActor(val server: ServerContext, val nickManager: ActorRef)
  extends CommandActor
  with NickHandler
  with PingHandler
  with UserHandler
  with QuitHandler
  with CommandHandler {

  import Commands._
  import Response._

  implicit val ctx = ClientContext(server, "")

  def handle(op: Operation): Response = {
    op.cmd match {
      case PingCmd => handlePing(op)
      case NickCmd => handleNick(op)
      case QuitCmd => handleQuit(op)
      case UserCmd => handleUser(op)
      case PongCmd => noop(op)
    }
  }

  def handleCommand(cmd: String): Response = {
    parseCommand(cmd).right.flatMap(handle)
  }

  // use HTTP handling of the CommandActor for now
  def receive = httpReceive orElse nickReceive
}
