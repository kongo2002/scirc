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

trait NickHandler extends BaseHandler {
  this: ClientActor =>

  import NickManager._

  /**
   * Process the 'NICK' operation
   * @param op operation to process
   * @param client client information
   * @return [[com.kongo2002.scirc.Response]] type
   */
  def handleNick(op: Operation, client: Client): Response = {
    val newNick = op.get(0)
    val nick = ctx.nick

    if (nick != newNick) {
      val isNew = nick.isEmpty

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
        sendMsg(StringResponse(msg), sendTo(client))

        // notify channels
        channelManager ! ChangeNick(oldNick, newNick, client)
      }

    case NickErr(err, client) =>
      sendMsg(err, sendTo(client))
  }
}
