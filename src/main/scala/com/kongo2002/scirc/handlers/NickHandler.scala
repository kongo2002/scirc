package com.kongo2002.scirc.handlers

import com.kongo2002.scirc._
import com.kongo2002.scirc.Response._

trait NickHandler extends BaseHandler {
  this: ClientActor =>

  import NickManager._

  def handleNick(op: Operation, client: Client): Response = {
    val newNick = op.get(0)
    val nick = ctx.nick

    if (nick != newNick) {
      val isNew = nick == ""

      if (isNew)
        nickManager ! RegisterNick(newNick, client)
      else
        nickManager ! ChangeNick(nick, newNick, client)
    }

    empty
  }

  def nickReceive: Receive = {
    case NickAck(newNick, client) =>
      val oldNick = ctx.nick
      val isNew = oldNick == ""

      // update to new nick
      ctx.nick = newNick

      // if this is a new nick this is probably the registration phase
      // but it could be a second try caused by a duplicate nick name
      if (isNew)
        self ! Registered(client)
      else {
        // notify client itself
        val msg = s":$oldNick!${ctx.user}@${ctx.host} NICK $newNick"
        sendResponse(StringResponse(msg), sendTo(client))

        // notify channels
        channelManager ! ChangeNick(oldNick, newNick, client)
      }

    case NickErr(err, client) =>
      sendError(err, sendTo(client))
  }
}
