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

trait KickHandler extends BaseHandler {
  this: ClientActor =>

  import ChannelManager._

  def handleKick(op: Operation, client: Client): Response = {
    // TODO: handle multiple users and/or channels

    var channel = op.get(0)
    var nick = op.get(1)

    // default reason is the nick of the user issuing the KICK
    var reason = op.get(2, ctx.nick)

    channelManager ! ChannelKick(channel, nick, reason, client)
    empty
  }
}
