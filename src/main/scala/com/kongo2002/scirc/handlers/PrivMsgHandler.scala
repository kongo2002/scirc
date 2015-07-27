package com.kongo2002.scirc.handlers

import com.kongo2002.scirc._
import com.kongo2002.scirc.Response._

trait PrivMsgHandler extends BaseHandler {
  this: ClientActor =>

  import ClientActor._

  def handlePrivMsg(op: Operation, client: Client): Response = {
    val rec = op.get(0)
    val text = op.get(1)

    if (rec == "")
      Left(ErrorNoRecipient("PRIVMSG"))
    else if (text == "")
      Left(ErrorNoTextToSend)
    else {
      channelManager ! PrivMsg(rec, text, ctx.nick, client)
      empty
    }
  }
}
