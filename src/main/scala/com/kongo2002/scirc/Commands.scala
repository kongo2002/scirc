package com.kongo2002.scirc

import akka.actor.Actor
import akka.util.ByteString

case class Operation(cmd: Command, args: Array[String])

abstract class Command(cmd: String, numArgs: Int) {
  def validate(input: Array[String]) = input.size >= numArgs
}

abstract class NoArgCommand(cmd: String) extends Command(cmd, 0)
abstract class OneArgCommand(cmd: String) extends Command(cmd, 1)

object Commands {
  import Response._

  case object PingCmd extends OneArgCommand("PING")
  case object PongCmd extends NoArgCommand("PONG")
  case object NickCmd extends OneArgCommand("NICK")

  val cmds: Map[String, Command] = Map(
    "PING" -> PingCmd,
    "PONG" -> PongCmd,
    "NICK" -> NickCmd
  )

  def parseCommand(input: String): Either[ErrorResponse, Operation] = {
    val parts = input.split(' ').filter(_ != "")

    if (parts.size < 1)
      Left(StringError("invalid command given"))
    else {
      cmds.get(parts(0)) match {
        case Some(cmd) =>
          val args = parts.drop(1)

          if (cmd.validate(args))
            Right(Operation(cmd, args))
          else
            Left(StringError("insufficient arguments"))
        case None =>
          Left(StringError("unknown command"))
      }
    }
  }
}
