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

object Response {

  // ERROR MESSAGE
  case class Err(e: ErrorResponse, client: Client)

  // SUCCESS MESSAGE
  case class Msg(msg: SuccessResponse, client: Client)

  // GENERAL RESPONSE
  type Response = Either[ErrorResponse, SuccessResponse]

  trait Reply {
    val crlf = "\r\n"
    val hasReply = true
    def ctx: ClientContext
    def getMessage: String
  }

  case class HostReply(msg: String, ctx: ClientContext) extends SuccessResponse {
    override def getMessage =
      s":${ctx.prefix} $msg$crlf"
  }

  // NUMERIC REPLIES
  abstract class NumericReply(code: Int, msg: String, ctx: ClientContext) extends Reply {
    override def getMessage = {
      val host = ctx.ctx.host
      val nick = ctx.nick match {
        case "" => "*"
        case n => n
      }

      ":%s %03d %s %s\r\n".format(host, code, nick, msg)
    }
  }

  // ABSTRACT ERRORS
  sealed trait ErrorResponse extends Reply
  sealed abstract class ErrorNumericReply(code: Int, msg: String, ctx: ClientContext)
    extends NumericReply(code, msg, ctx)
    with ErrorResponse

  case class StringError(msg: String, ctx: ClientContext) extends ErrorResponse {
    override def getMessage = s"ERROR :$msg$crlf"
  }

  case class ErrorNoSuchNick(nick: String, ctx: ClientContext)
    extends ErrorNumericReply(401, s"$nick :No such nick/channel", ctx)

  case class ErrorNoSuchChannel(channel: String, ctx: ClientContext)
    extends ErrorNumericReply(403, s"$channel :No such channel", ctx)

  case class ErrorCannotSendToChannel(channel: String, ctx: ClientContext)
    extends ErrorNumericReply(404, s"$channel :Cannot send to channel", ctx)

  case class ErrorNoRecipient(cmd: String, ctx: ClientContext)
    extends ErrorNumericReply(411, s":No recipient given ($cmd)", ctx)

  case class ErrorNoTextToSend(ctx: ClientContext)
    extends ErrorNumericReply(412, ":No text to send", ctx)

  case class ErrorNoMotd(ctx: ClientContext)
    extends ErrorNumericReply(422, ":MOTD File is missing", ctx)

  case class ErrorErroneousNick(nick: String, ctx: ClientContext)
    extends ErrorNumericReply(432, s"$nick :Erroneous nickname", ctx)

  case class ErrorNickAlreadyInUse(nick: String, ctx: ClientContext)
    extends ErrorNumericReply(433, s"$nick :Nickname is already in use", ctx)

  case class ErrorUserNotInChannel(nick: String, channel: String, ctx: ClientContext)
    extends ErrorNumericReply(441, s"$nick $channel :They aren't on that channel", ctx)

  case class ErrorNotOnChannel(channel: String, ctx: ClientContext)
    extends ErrorNumericReply(442, s"$channel :You're not on that channel", ctx)

  case class ErrorNeedMoreParams(cmd: String, ctx: ClientContext)
    extends ErrorNumericReply(461, s"$cmd :Not enough parameters", ctx)

  case class ErrorPasswordMismatch(ctx: ClientContext)
    extends ErrorNumericReply(464, ":Password incorrect", ctx)

  case class ErrorBadChannelKey(channel: String, ctx: ClientContext)
    extends ErrorNumericReply(475, s"$channel :Cannot join channel (+k)", ctx)

  case class ErrorBadChannelMask(channel: String, ctx: ClientContext)
    extends ErrorNumericReply(476, s"$channel :Bad Channel Mask", ctx)

  case class ErrorChannelOperatorPrivilegeNeeded(channel: String, ctx: ClientContext)
    extends ErrorNumericReply(482, s"$channel :You're not channel operator", ctx)

  case class ErrorUsersDontMatch(ctx: ClientContext)
    extends ErrorNumericReply(502, ":Cannot change mode for other users", ctx)

  // SUCCESS TYPES

  trait SuccessResponse extends Reply

  case class EmptyResponse(ctx: ClientContext) extends SuccessResponse {
    override val hasReply = false
    override def getMessage = ""
  }

  case class StringResponse(msg: String, ctx: ClientContext) extends SuccessResponse {
    override def getMessage = msg + crlf
  }

  case class ListResponse(rs: List[Reply], ctx: ClientContext) extends SuccessResponse {
    override def getMessage = {
      rs.filter(_.hasReply).map(response => response.getMessage).mkString("")
    }
  }

  abstract class SuccessNumericReply(code: Int, msg: String, ctx: ClientContext)
    extends NumericReply(code, msg, ctx)
    with SuccessResponse

  case class ReplyWelcome(msg: String, ctx: ClientContext)
    extends SuccessNumericReply(1, msg, ctx)

  case class ReplyYourHost(msg: String, ctx: ClientContext)
    extends SuccessNumericReply(2, msg, ctx)

  case class ReplyCreated(msg: String, ctx: ClientContext)
    extends SuccessNumericReply(3, msg, ctx)

  case class ReplyMyInfo(msg: String, ctx: ClientContext)
    extends SuccessNumericReply(4, msg, ctx)

  case class ReplyUserModeIs(modes: Modes.ModeSet, ctx: ClientContext)
    extends SuccessNumericReply(221, modes.modeString, ctx)

  case class ReplyIson(msg: String, ctx: ClientContext)
    extends SuccessNumericReply(303, msg, ctx)

  case class ReplyWhoIsUser(ctx: ClientContext)
    extends SuccessNumericReply(311, s"${ctx.nick} ${ctx.user} ${ctx.host} * :${ctx.realname}", ctx)

  case class ReplyWhoIsServer(nick: String, server: String, info: String, ctx: ClientContext)
    extends SuccessNumericReply(312, s"$nick $server :$info", ctx)

  case class ReplyEndOfWho(name: String, ctx: ClientContext)
    extends SuccessNumericReply(315, s"$name :End of WHO list", ctx)

  case class ReplyEndOfWhoIsList(nick: String, ctx: ClientContext)
    extends SuccessNumericReply(318, s"$nick :End of WHOIS list", ctx)

  case class ReplyWhoIsChannels(nick: String, channels: List[String], ctx: ClientContext)
    extends SuccessNumericReply(319, s"$nick :${channels.mkString(" ")}", ctx)

  case class ReplyList(channel: String, visible: Int, topic: String, ctx: ClientContext)
    extends SuccessNumericReply(322, s"$channel $visible :$topic", ctx)

  case class ReplyEndOfList(ctx: ClientContext)
    extends SuccessNumericReply(323, ":End of LIST", ctx)

  // TODO: mode parameters
  case class ReplyChannelModeIs(channel: String, modes: String, ctx: ClientContext)
    extends SuccessNumericReply(324, s"$channel $modes", ctx)

  case class ReplyChannelCreation(channel: String, creation: java.util.Date, ctx: ClientContext)
    extends SuccessNumericReply(329, s"$channel ${creation.getTime / 1000}", ctx)

  case class ReplyNoTopic(channel: String, ctx: ClientContext)
    extends SuccessNumericReply(331, s"$channel :No topic is set", ctx)

  case class ReplyTopic(channel: String, topic: String, ctx: ClientContext)
    extends SuccessNumericReply(332, s"$channel :$topic", ctx)

  case class ReplyInviteList(channel: String, mask: String, ctx: ClientContext)
    extends SuccessNumericReply(346, s"$channel $mask", ctx)

  case class ReplyEndOfInviteList(channel: String, ctx: ClientContext)
    extends SuccessNumericReply(347, s"$channel :End of channel invite list", ctx)

  case class ReplyExceptList(channel: String, mask: String, ctx: ClientContext)
    extends SuccessNumericReply(348, s"$channel $mask", ctx)

  case class ReplyEndOfExceptList(channel: String, ctx: ClientContext)
    extends SuccessNumericReply(349, s"$channel :End of channel exception list", ctx)

  case class ReplyWho(channel: String, info: Info.UserWhoInfo, ctx: ClientContext)
    extends SuccessNumericReply(
      352,
      s"$channel ${info.ctx.user} ${info.ctx.host} ${info.ctx.ctx.host} ${info.ctx.nick} ${info.modes} :${info.ctx.hops} ${info.ctx.realname}", ctx
    )

  // TODO: channel type
  case class ReplyChannelNames(channel: String, names: String, ctx: ClientContext)
    extends SuccessNumericReply(353, s"= $channel :$names", ctx)

  case class ReplyEndOfNames(channel: String, ctx: ClientContext)
    extends SuccessNumericReply(366, s"$channel :End of NAMES list", ctx)

  case class ReplyBanList(channel: String, mask: String, ctx: ClientContext)
    extends SuccessNumericReply(367, s"$channel $mask", ctx)

  case class ReplyEndOfBanList(channel: String, ctx: ClientContext)
    extends SuccessNumericReply(368, s"$channel :End of channel ban list", ctx)

  case class ReplyMotd(line: String, ctx: ClientContext)
    extends SuccessNumericReply(372, s":- ${line.take(80)}", ctx)

  case class ReplyStartMotd(server: String, ctx: ClientContext)
    extends SuccessNumericReply(375, s":- $server Message of the day - ", ctx)

  case class ReplyEndOfMotd(ctx: ClientContext)
    extends SuccessNumericReply(376, ":End of MOTD command", ctx)

  case class ReplyYouAreOperator(ctx: ClientContext)
    extends SuccessNumericReply(381, ":You are now an IRC operator", ctx)
}
