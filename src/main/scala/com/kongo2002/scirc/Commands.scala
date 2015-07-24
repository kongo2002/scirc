package com.kongo2002.scirc

import akka.actor.Actor
import akka.util.ByteString

case class Operation(cmd: Command, args: Array[String]) {
  def get(idx: Int): String = get(idx, "")
  def get(idx: Int, default: String): String = {
    if (idx < args.size) args(idx) else default
  }
}

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
  case object QuitCmd extends NoArgCommand("QUIT")

  val cmds: Map[String, Command] = Map(
    "PING" -> PingCmd,
    "PONG" -> PongCmd,
    "NICK" -> NickCmd,
    "QUIT" -> QuitCmd
  )

  def getArguments(args: List[String]): Array[String] = args match {
    case Nil => Array()
    case args :: _ =>
      if (args.size > 0 && args(0) == ':')
        Array(args.drop(1))
      else {
        Utils.splitFirst(args, " :") match {
          case List(fst, rest) =>
            fst.split(" ") :+ rest
          case _ => args.split(" ")
        }
      }
  }

  def parseCommand(input: String): Either[ErrorResponse, Operation] = {
    Utils.splitFirst(input, " ") match {
      case cmd :: rest =>
        cmds.get(cmd) match {
          case Some(c) =>
            val arguments = getArguments(rest)
            if (c.validate(arguments))
              Right(Operation(c, arguments))
            else
              Left(StringError("insufficient arguments"))
          case None =>
            Left(StringError("unknown command"))
        }
      case _ =>
        Left(StringError("invalid command given"))
    }
  }
}
