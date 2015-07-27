package com.kongo2002.scirc.handlers

import com.kongo2002.scirc._
import com.kongo2002.scirc.Response._

trait WhoIsHandler extends BaseHandler {
  this: ClientActor =>

  import NickManager._

  def args(xs: Array[String]) = xs.toList match {
    case List(target, mask) => (target, mask.split(","))
    case mask :: _ => ("", mask.split(","))
  }

  def handleWhois(op: Operation, client: Client): Response = {
    // TODO: respect target
    val (target, mask) = args(op.args)

    nickManager ! WhoIsNicks(mask, client)
    empty
  }

  def whoisReceive: Receive = {
    case WhoIs(client: Client) =>
      val nick = ctx.nick

      val reply = ListResponse(List(
        ReplyWhoIsUser(nick, ctx.user, ctx.host, ctx.realname),
        ReplyWhoIsServer(nick, ctx.ctx.host, ctx.ctx.host),

        // TODO: channel list
        ReplyEndOfWhoIsList(nick)
      ))

      sendResponse(reply, sendTo(client))
  }
}
