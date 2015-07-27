package com.kongo2002.scirc.handlers

import com.kongo2002.scirc._
import com.kongo2002.scirc.Response._

trait PingHandler extends BaseHandler {
  this: ClientActor =>

  def handlePing(op: Operation, client: Client): Response = {
    val host = server.host
    hostReply(s"PONG $host :$host")
  }
}
