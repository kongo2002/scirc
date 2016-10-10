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

trait PrivMsgHandler extends BaseHandler {
  this: ClientActor =>

  import ClientActor._

  def handlePrivMsg(op: Operation, client: Client): Response = {
    val rec = op.get(0)
    val text = op.get(1)

    if (rec == "")
      Left(ErrorNoRecipient("PRIVMSG", ctx))
    else if (text == "")
      Left(ErrorNoTextToSend(ctx))
    else {
      channelManager ! PrivMsg(rec, text, ctx.nick, client)
      metrics.foreach(_.privateMessageCounter.increment())
      empty
    }
  }
}
