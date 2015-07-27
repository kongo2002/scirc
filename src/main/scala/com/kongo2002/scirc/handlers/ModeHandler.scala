package com.kongo2002.scirc.handlers

import com.kongo2002.scirc._
import com.kongo2002.scirc.Response._

trait ModeHandler extends BaseHandler {
  this: ClientActor =>

  import ChannelActor._

  private def nickMode(nick: String, op: Operation, client: Client): Response = {
    if (nick == ctx.nick) {
      val mode = op.get(1)

      // set modes if given
      if (mode != "") {
        log.debug(s"${ctx.nick}: MODE request '$mode'")

        if (ctx.modes.applyMode(mode))
          log.debug(s"${ctx.nick}: new MODE set '${ctx.modes.modeString}'")
      }

      Right(ReplyUserModeIs(ctx.modes))
    }
    else
      Left(ErrorUsersDontMatch)
  }

  private def channelMode(channel: String, op: Operation, client: Client): Response = {
    channelManager ! GetChannelModes(channel, client)
    empty
  }

  def handleMode(op: Operation, client: Client): Response = {
    val target = op.get(0)

    // check whether it is a channel mode query
    if (ChannelManager.isValidChannel(target))
      channelMode(target, op, client)
    // or a client/nick query
    else
      nickMode(target, op, client)
  }

  def modeReceive: Receive = {
    case ChannelModes(channel, modes, client) =>
      sendResponse(ReplyChannelModeIs(channel, modes), sendTo(client))
  }
}
