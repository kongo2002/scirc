package com.kongo2002.scirc

object Response {
  // general response
  type Response = Either[ErrorResponse, SuccessResponse]

  // abstract errors
  abstract class ErrorResponse(code: Int, msg: String)
  case class StringError(msg: String) extends ErrorResponse(0, msg)

  // success types
  abstract trait SuccessResponse
  case object EmptyResponse              extends SuccessResponse
  case class StringResponse(msg: String) extends SuccessResponse
}
