package com.kongo2002.scirc.handlers

import com.kongo2002.scirc._
import com.kongo2002.scirc.Response._

trait UserHandler extends BaseHandler {
  this: ClientActor =>

  import NickManager._

  def handleUser(op: Operation, client: Client): Response = {
    ctx.user = op.get(0)
    ctx.realname = op.get(3)

    // TODO: set default mask
    val modeBitMask = op.getInt(1).getOrElse(0)

    self ! Registered(client)

    empty
  }

  def userReceive: Receive = {
    case Registered(client) =>
      if (!ctx.isRegistered && ctx.nick != "" && ctx.user != "") {
        ctx.isRegistered = true
        sendResponse(welcome, sendTo(client))
      }
  }
}
