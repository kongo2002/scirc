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

trait JoinHandler extends BaseHandler {
  this: ClientActor =>

  import ChannelActor._
  import ChannelManager._

  def handleJoin(op: Operation, client: Client): Response = {
    val channels = op.get(0).split(",")
    val numChannels = channels.size
    val keys = op.get(1).split(",") ++ Iterator.fill(numChannels)("")

    channels.zip(keys).foreach {
      case (channel, key) =>
        channelManager ! ChannelJoin(channel, ctx.nick, key, client)
    }

    empty
  }

  def joinReceive: Receive = {
    case ChannelJoined(ch, topic, created, names, client) =>
      topic match {
        case "" => sendMsg(ReplyNoTopic(ch, ctx), sendTo(client))
        case _ => sendMsg(ReplyTopic(ch, topic, ctx), sendTo(client))
      }

      // send list of names
      // TODO: split into multiple messages if necessary
      val nameStr = names.mkString(" ")
      sendMsg(ReplyChannelNames(ch, nameStr, ctx), sendTo(client))
      sendMsg(ReplyEndOfNames(ch, ctx), sendTo(client))

      // send channel creation date
      sendMsg(ReplyChannelCreation(ch, created, ctx), sendTo(client))
  }
}
