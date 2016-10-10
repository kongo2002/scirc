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

package com.kongo2002.scirc

import com.kongo2002.scirc.Modes.UserModeSet

/**
 * Server context information
 *
 * @param host server host name
 * @param modes server modes
 */
case class ServerContext(host: String, modes: Int) {
  val created = java.util.Calendar.getInstance().getTime
}

/**
 * Client context information
 * @param ctx server context
 * @param host client's host name
 * @param nick client's nick name
 */
case class ClientContext(ctx: ServerContext, host: String, nick: String, user: String = "", realname: String = "", isRegistered: Boolean = false, hops: Int = 0, modes: UserModeSet = UserModeSet()) {
  def prefix: String = s"$nick!$user@$host"
  def registerable: Boolean = !isRegistered && user.nonEmpty && nick.nonEmpty

  def withNick(nick: String): ClientContext = copy(nick = nick)
  def withModes(modes: UserModeSet): ClientContext = copy(modes = modes)
  def register: ClientContext = copy(isRegistered = true)
}
