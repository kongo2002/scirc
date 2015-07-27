package com.kongo2002.scirc

import akka.actor.Actor
import akka.io.Tcp
import akka.util.ByteString

object Response {
  // error message
  case class Err(e: ErrorResponse, client: Client)

  // general response
  type Response = Either[ErrorResponse, SuccessResponse]

  // numeric replies
  abstract class NumericReply(code: Int, msg: String)
    extends SuccessResponse {

    def getMessage(implicit ctx: ClientContext) = {
      val host = ctx.ctx.host
      val nick = ctx.nick match {
        case "" => "*"
        case n => n
      }

      ":%s %03d %s %s".format(host, code, nick, msg)
    }
  }

  // abstract errors
  abstract trait ErrorResponse
  abstract class ErrorNumericReply(code: Int, msg: String)
    extends NumericReply(code, msg)
    with ErrorResponse

  case class StringError(msg: String) extends ErrorNumericReply(0, msg)

  case class ErrorNoSuchNick(nick: String)
    extends ErrorNumericReply(401, s"$nick :No such nick/channel")

  case class ErrorNoSuchChannel(channel: String)
    extends ErrorNumericReply(403, s"$channel :No such channel")

  case class ErrorCannotSendToChannel(channel: String)
    extends ErrorNumericReply(404, s"$channel :Cannot send to channel")

  case class ErrorNoRecipient(cmd: String)
    extends ErrorNumericReply(411, s":No recipient given ($cmd)")

  case object ErrorNoTextToSend
    extends ErrorNumericReply(412, ":No text to send")

  case class ErrorNickAlreadyInUse(nick: String)
    extends ErrorNumericReply(433, s"$nick :Nickname is already in use")

  case class ErrorNotOnChannel(channel: String)
    extends ErrorNumericReply(442, s"$channel :You're not on that channel")

  case class ErrorNeedMoreParams(cmd: String)
    extends ErrorNumericReply(461, s"$cmd :Not enough parameters")

  case object ErrorUsersDontMatch
    extends ErrorNumericReply(502, ":Cannot change mode for other users")

  // success types
  abstract trait SuccessResponse
  case object EmptyResponse              extends SuccessResponse
  case class StringResponse(msg: String) extends SuccessResponse
  case class ListResponse(rs: List[SuccessResponse]) extends SuccessResponse

  abstract class SuccessNumericReply(code: Int, msg: String)
    extends NumericReply(code, msg)
    with SuccessResponse

  case class ReplyWelcome(msg: String)
    extends SuccessNumericReply(1, msg)

  case class ReplyYourHost(msg: String)
    extends SuccessNumericReply(2, msg)

  case class ReplyCreated(msg: String)
    extends SuccessNumericReply(3, msg)

  case class ReplyMyInfo(msg: String)
    extends SuccessNumericReply(4, msg)

  case class ReplyUserModeIs(modes: Modes.ModeSet)
    extends SuccessNumericReply(221, modes.modeString)

  case class ReplyIson(msg: String)
    extends SuccessNumericReply(303, msg)

  case class ReplyWhoIsUser(nick: String, user: String, host: String, realname: String)
    extends SuccessNumericReply(311, s"$nick $user $host * :$realname")

  case class ReplyWhoIsServer(nick: String, server: String, info: String)
    extends SuccessNumericReply(312, s"$nick $server :$info")

  case class ReplyEndOfWhoIsList(nick: String)
    extends SuccessNumericReply(318, s"$nick :End of WHOIS list")

  case class ReplyNoTopic(channel: String)
    extends SuccessNumericReply(331, s"$channel :No topic is set")

  case class ReplyTopic(channel: String, topic: String)
    extends SuccessNumericReply(332, s"$channel :$topic")

  // TODO: channel type
  case class ReplyChannelNames(channel: String, names: String)
    extends SuccessNumericReply(353, s"= $channel :$names")

  case class ReplyEndOfNames(channel: String)
    extends SuccessNumericReply(366, s"$channel :End of NAMES list")
}
