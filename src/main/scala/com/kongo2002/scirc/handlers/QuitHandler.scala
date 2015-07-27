package com.kongo2002.scirc.handlers

import com.kongo2002.scirc._
import com.kongo2002.scirc.Response._

trait QuitHandler extends BaseHandler {
  this: ClientActor =>

  import ChannelManager._
  import NickManager._

  def disconnect(client: Client) {
    // terminate itself
    context stop self

    // unregister nick
    channelManager ! ChannelJoin("0", ctx.nick, client)
    nickManager ! DisconnectNick(ctx.nick)
  }

  def handleQuit(op: Operation, client: Client): Response = {
    val msg = op.get(0, "leaving")

    disconnect(client)

    Left(StringError(s"QUIT :$msg"))
  }
}
