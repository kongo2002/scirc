package com.kongo2002.scirc

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp

import Handlers._

case class Client(client: ActorRef, socket: ActorRef)

object ClientActor {
  def apply(server: ServerContext, nickManager: ActorRef, channelManager: ActorRef) =
    new ClientActor(server, nickManager, channelManager)
}

class ClientActor(val server: ServerContext,
    val nickManager: ActorRef,
    val channelManager: ActorRef)
  extends CommandActor
  with NickHandler
  with PingHandler
  with UserHandler
  with IsonHandler
  with JoinHandler
  with QuitHandler
  with PartHandler
  with PrivMsgHandler
  with CommandHandler {

  import Commands._
  import ChannelManager._
  import NickManager._
  import Response._

  implicit val ctx = ClientContext(server, "")

  def handle(op: Operation): Response = {
    val client = Client(self, sender)
    val handler = op.cmd match {
      case PingCmd    => handlePing _
      case NickCmd    => handleNick _
      case QuitCmd    => handleQuit _
      case UserCmd    => handleUser _
      case IsonCmd    => handleIson _
      case JoinCmd    => handleJoin _
      case PartCmd    => handlePart _
      case PrivMsgCmd => handlePrivMsg _
      case PongCmd    => noop _
    }

    handler(op, client)
  }

  def handleCommand(cmd: String): Response = {
    parseCommand(cmd).right.flatMap(handle)
  }

  def handleClose: Receive = {
    case Tcp.PeerClosed =>
      println("Connection closed")

      disconnect(Client(self, sender))
  }

  def handleError: Receive = {
    case Err(e, client) => sendError(e, sendTo(client))
  }

  def receive =
    httpReceive orElse
    nickReceive orElse
    isonReceive orElse
    joinReceive orElse
    handleError orElse
    handleClose
}
