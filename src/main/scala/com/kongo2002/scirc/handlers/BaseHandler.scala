package com.kongo2002.scirc.handlers

import com.kongo2002.scirc._
import com.kongo2002.scirc.Response._

abstract trait BaseHandler {
  this: ClientActor =>

  val empty = Right(EmptyResponse)

  def success(response: String) = Right(StringResponse(response))

  def noop(op: Operation, client: Client): Response = empty

  def hostReply(reply: String) =
    success(s":${server.host} $reply")
}
