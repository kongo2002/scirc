package com.kongo2002.scirc.handlers

import com.kongo2002.scirc._
import com.kongo2002.scirc.Response._

trait JoinHandler extends BaseHandler {
  this: ClientActor =>

  import ChannelActor._
  import ChannelManager._

  def handleJoin(op: Operation, client: Client): Response = {
    // TODO: handle multiple channels
    // TODO: handle keys
    var channel = op.get(0)
    channelManager ! ChannelJoin(channel, ctx.nick, client)
    empty
  }

  def joinReceive: Receive = {
    case ChannelJoined(ch, topic, created, names, client) =>
      topic match {
        case "" => sendResponse(ReplyNoTopic(ch), sendTo(client))
        case _ => sendResponse(ReplyTopic(ch, topic), sendTo(client))
      }

      // send list of names
      // TODO: split into multiple messages if necessary
      val nameStr = names.mkString(" ")
      sendResponse(ReplyChannelNames(ch, nameStr), sendTo(client))
      sendResponse(ReplyEndOfNames(ch), sendTo(client))

      // send channel creation date
      sendResponse(ReplyChannelCreation(ch, created), sendTo(client))
  }
}
