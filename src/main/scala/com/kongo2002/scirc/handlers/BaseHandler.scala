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

trait BaseHandler {
  this: ClientActor =>

  /** successful empty response */
  val empty = Right(EmptyResponse(ctx))

  /** generate a successful string response */
  def success(response: String) = Right(StringResponse(response, ctx))

  /** no-operation handler with an empty response (@see [[empty]]) */
  def noop(op: Operation, client: Client): Response = empty

  /** generate a host reply (i.e. ':some.host reply') */
  def hostReply(reply: String) =
    success(s":${server.host} $reply")
}
