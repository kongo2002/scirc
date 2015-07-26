package com.kongo2002.scirc

object Response {
  // general response
  type Response = Either[ErrorResponse, SuccessResponse]

  // numeric replies
  abstract class NumericReply(code: Int, msg: String)
    extends SuccessResponse {

    def getMessage(implicit ctx: ClientContext) = {
      "%03d %s %s".format(code, ctx.user, msg)
    }
  }

  // abstract errors
  abstract trait ErrorResponse
  abstract class ErrorNumericReply(code: Int, msg: String)
    extends NumericReply(code, msg)
    with ErrorResponse

  case class StringError(msg: String) extends ErrorNumericReply(0, msg)

  case class ErrorNeedMoreParams(cmd: String)
    extends ErrorNumericReply(461, s"$cmd :Not enough parameters")

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

  case class ReplyIson(msg: String)
    extends SuccessNumericReply(303, msg)
}
