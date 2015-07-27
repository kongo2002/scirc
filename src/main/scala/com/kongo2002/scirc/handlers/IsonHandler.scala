package com.kongo2002.scirc.handlers

import com.kongo2002.scirc._
import com.kongo2002.scirc.Response._

trait IsonHandler extends BaseHandler {
  this: ClientActor =>

  import NickManager._

  def handleIson(op: Operation, client: Client): Response = {
    nickManager ! OnlineNicks(op.args.toList, client)
    empty
  }

  def isonReceive: Receive = {
    case NicksOnline(ns, rec) =>
      sendResponse(ReplyIson(ns.mkString(" ")), (sendTo(rec)))
  }
}
