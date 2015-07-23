package com.kongo2002.scirc

import akka.actor.Actor
import akka.util.ByteString

import scala.util.{Try, Success, Failure}

abstract class Command(cmd: String)
case object PingCmd extends Command("PING")

object Commands {
  val cmds: Map[String, Command] = Map("PING" -> PingCmd)

  def parseCommand(cmd: String): Option[Command] = cmds.get(cmd)
}

trait CommandHandler {
  this: Actor =>

  import Commands._

  def handle(cmd: Command): String = {
    cmd match {
      case PingCmd => "PONG"
    }
  }

  def handleCommand(cmd: String): Try[String] = {
    parseCommand(cmd) match {
      case Some(c) =>
        Try(handle(c))
      case None =>
        Failure(new Error(s"Command '$cmd' does not exist"))
    }
  }
}
