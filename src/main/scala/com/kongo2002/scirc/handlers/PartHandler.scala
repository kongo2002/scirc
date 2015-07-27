package com.kongo2002.scirc.handlers

import com.kongo2002.scirc._
import com.kongo2002.scirc.Response._

trait PartHandler extends BaseHandler {
  this: ClientActor =>

  import ChannelManager._

  def handlePart(op: Operation, client: Client): Response = {
    var channel = op.get(0)
    var reason = op.get(1, ctx.nick)

    channelManager ! ChannelPart(channel, ctx.nick, reason, client)
    empty
  }
}
