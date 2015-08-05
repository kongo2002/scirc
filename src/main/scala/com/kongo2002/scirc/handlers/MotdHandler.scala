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

trait MotdHandler extends BaseHandler {
  this: ClientActor =>

  def readMotd: Option[List[String]] = {
    try {
      val source = io.Source.fromFile("motd.txt")
      val lines = source.getLines.toList

      source.close

      if (lines.nonEmpty)
        Some(lines)
      else
        None
    } catch {
      case _: Throwable => None
    }
  }

  def getMotd: Reply = {
    readMotd match {
      case Some(motd) =>
        val lines = List(ReplyStartMotd(server.host)) ++
          motd.map(ReplyMotd) :+
          ReplyEndOfMotd

        ListResponse(lines)
      case None =>
        ErrorNoMotd
    }
  }

  def handleMotd(op: Operation, client: Client): Response = {
    sendMsg(getMotd, sendTo(client))
    empty
  }
}
