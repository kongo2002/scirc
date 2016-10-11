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

trait WhoHandler extends BaseHandler {
  this: ClientActor =>

  import ChannelActor._

  private def channelWho(mask: String, opOnly: Boolean, client: Client) = {
    channelManager ! WhoQuery(mask, opOnly, client)
    empty
  }

  private def userWho(mask: String, opOnly: Boolean, client: Client) = {
    // TODO
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
