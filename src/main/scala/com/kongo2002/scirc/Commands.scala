package com.kongo2002.scirc

import akka.actor.Actor
import akka.util.ByteString

import scala.util.{Try, Success, Failure}

case class Operation(cmd: Command, args: Array[String])

abstract class Command(cmd: String, numArgs: Int) {
  def validate(input: Array[String]) = input.size >= numArgs
}

abstract class OneArgCommand(cmd: String)
  extends Command(cmd, 1)

case object PingCmd extends OneArgCommand("PING")

object Commands {
  val cmds: Map[String, Command] = Map("PING" -> PingCmd)

  def parseCommand(input: String): Either[String, Operation] = {
    val parts = input.split(' ').filter(_ != "")

    if (parts.size < 1)
      Left("invalid command given")
    else {
      cmds.get(parts(0)) match {
        case Some(cmd) =>
          val args = parts.drop(1)

          if (cmd.validate(args))
            Right(Operation(cmd, args))
          else
            Left("insufficient arguments")
        case None =>
          Left("unknown command")
      }
    }
  }
}

trait CommandHandler {
  this: Actor =>

  import Commands._

  def handle(op: Operation): String = {
    op.cmd match {
      case PingCmd => "PONG"
    }
  }

  def handleCommand(cmd: String): Either[String, String] = {
    parseCommand(cmd) match {
      case Right(c) =>
        Try(handle(c)) match {
          case Success(response) => Right(response)
          case Failure(err) => Left(err.toString)
        }
      case Left(s) => Left(s)
    }
  }
}
