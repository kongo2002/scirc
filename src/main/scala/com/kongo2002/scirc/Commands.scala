/* Copyright 2015 Gregor Uhlenheuer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.kongo2002.scirc

/**
 * Base operation class that encapsulates all commands
 * @param cmd command name
 * @param args command arguments
 */
case class Operation(cmd: Command, args: Array[String]) {
  def get(idx: Int): String = get(idx, "")
  def get(idx: Int, default: String): String = {
    if (idx < args.length) args(idx) else default
  }
  def getInt(idx: Int): Option[Int] = {
    try {
      Some(get(idx).toInt)
    } catch {
      case e: NumberFormatException => None
    }
  }
}

sealed abstract class Command(cmd: String, numArgs: Int) {
  def validate(input: Array[String]) = input.length >= numArgs
}

sealed abstract class NoArgCommand(cmd: String) extends Command(cmd, 0)
sealed abstract class OneArgCommand(cmd: String) extends Command(cmd, 1)

object Commands {
  import Response._

  case object PingCmd extends OneArgCommand("PING")
  case object PongCmd extends NoArgCommand("PONG")
  case object NickCmd extends OneArgCommand("NICK")
  case object QuitCmd extends NoArgCommand("QUIT")
  case object IsonCmd extends OneArgCommand("ISON")
  case object JoinCmd extends OneArgCommand("JOIN")
  case object PartCmd extends OneArgCommand("PART")
  case object PrivMsgCmd extends NoArgCommand("PRIVMSG")
  case object WhoIsCmd extends OneArgCommand("WHOIS")
  case object ModeCmd extends OneArgCommand("MODE")
  case object WhoCmd extends OneArgCommand("WHO")
  case object TopicCmd extends OneArgCommand("TOPIC")
  case object MotdCmd extends NoArgCommand("MOTD")
  case object ListCmd extends NoArgCommand("LIST")
  case object KickCmd extends Command("KICK", 2)
  case object OperCmd extends Command("OPER", 2)
  case object UserCmd extends Command("USER", 4)

  val cmds: Map[String, Command] = Map(
    "ISON" -> IsonCmd,
    "JOIN" -> JoinCmd,
    "KICK" -> KickCmd,
    "LIST" -> ListCmd,
    "MODE" -> ModeCmd,
    "MOTD" -> MotdCmd,
    "NICK" -> NickCmd,
    "OPER" -> OperCmd,
    "PART" -> PartCmd,
    "PING" -> PingCmd,
    "PONG" -> PongCmd,
    "PRIVMSG" -> PrivMsgCmd,
    "QUIT" -> QuitCmd,
    "TOPIC" -> TopicCmd,
    "USER" -> UserCmd,
    "WHO" -> WhoCmd,
    "WHOIS" -> WhoIsCmd
  )

  def getArguments(args: List[String]): Array[String] = args match {
    case Nil => Array()
    case as :: _ =>
      if (as.nonEmpty && as(0) == ':')
        Array(as.drop(1))
      else {
        Utils.splitFirst(as, " :") match {
          case List(fst, rest) =>
            fst.split(" ") :+ rest
          case _ => as.split(" ")
        }
      }
  }

  def parseCommand(input: String, ctx: ClientContext): Either[ErrorResponse, Operation] = {
    Utils.splitFirst(input, " ") match {
      case cmd :: rest =>
        cmds.get(cmd) match {
          case Some(c) =>
            val arguments = getArguments(rest)
            if (c.validate(arguments))
              Right(Operation(c, arguments))
            else
              Left(ErrorNeedMoreParams(cmd, ctx))
          case None =>
            Left(StringError("unknown command", ctx))
        }
      case _ =>
        Left(StringError("invalid command given", ctx))
    }
  }
}
