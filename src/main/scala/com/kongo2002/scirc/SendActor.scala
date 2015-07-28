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

import akka.actor.Actor
import akka.io.Tcp
import akka.util.ByteString

trait SendActor extends Actor {
  def send(msg: ByteString)(client: Client) {
    client.socket ! Tcp.Write(msg)
  }

  def send(msg: String)(client: Client) {
    send(ByteString(msg))(client)
  }
}
