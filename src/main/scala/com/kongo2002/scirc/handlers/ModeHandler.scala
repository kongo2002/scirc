/* Copyright 2015 Gregor Uhlenheuer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.kongo2002.scirc.handlers

import com.kongo2002.scirc._
import com.kongo2002.scirc.Response._

trait ModeHandler extends BaseHandler {
  this: ClientActor =>

  import ChannelActor._

  private def nickMode(nick: String, op: Operation): Response = {
    if (nick == ctx.nick) {
      // set modes if given
      if (op.args.size > 1) {
        val modes = op.args.drop(1)
        if (ctx.modes.applyModes(modes))
          log.debug(s"${ctx.nick}: new MODE set '${ctx.modes.modeString}'")
      }

      // I am not sure how to reply here. According to the the RFC 2811
      // I understand to send a numeric reply
      // but the practice of other servers appears to be different...
      Right(ListResponse(List(
        ReplyUserModeIs(ctx.modes),
        HostReply(s"MODE ${ctx.nick} ${ctx.modes.modeString}"))))
    }
    else
      Left(ErrorUsersDontMatch)
  }

  private def channelMode(channel: String, op: Operation, client: Client): Response = {
    if (op.args.size > 1) {
      val args = op.args.drop(1)
      channelManager ! SetChannelModes(channel, args, client)
    } else {
      channelManager ! GetChannelModes(channel, client)
    }
    empty
  }

  def handleMode(op: Operation, client: Client): Response = {
    val target = op.get(0)

    // check whether it is a channel mode query
    if (ChannelManager.isValidChannel(target))
      channelMode(target, op, client)
    // or a client/nick query
    else
      nickMode(target, op)
  }

  def modeReceive: Receive = {
    case ChannelModes(channel, modes, client) =>
      sendResponse(ReplyChannelModeIs(channel, modes), sendTo(client))
  }
}
