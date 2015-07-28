package com.kongo2002.scirc.handlers

import com.kongo2002.scirc._
import com.kongo2002.scirc.Response._

trait WhoHandler extends BaseHandler {
  this: ClientActor =>

  import ChannelActor._

  private def channelWho(mask: String, opOnly: Boolean, client: Client) = {
    channelManager ! WhoQuery(mask, opOnly, client)
    empty
  }

  private def userWho(mask: String, opOnly: Boolean, client: Client) = {
    empty
  }

  def handleWho(op: Operation, client: Client): Response = {
    val mask = op.get(0)
    val opOnly = op.get(1) == "o"

    if (ChannelManager.isValidChannel(mask)) {
      channelWho(mask, opOnly, client)
    } else {
      userWho(mask, opOnly, client)
    }
  }
}
