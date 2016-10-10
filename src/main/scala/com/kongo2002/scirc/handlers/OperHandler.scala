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

trait OperHandler extends BaseHandler {
  this: ClientActor =>

  case class Operator(name: String, password: String)

  def getOperators: List[Operator] = {
    def toOp(input: String) = {
      Utils.splitFirst(input, ":") match {
        case List(u, p) => List(Operator(u, p))
        case _ => Nil
      }
    }

    getConfigStringList("operators").flatMap(toOp)
  }

  def validAuth(name: String, password: String) = {
    getOperators.exists {
      case Operator(n, p) if n == name && p == password => true
      case _ => false
    }
  }

  def handleOper(op: Operation, client: Client): Response = {
    val name = op.get(0)
    val password = op.get(1)

    // check if the current user is an operator already
    if (ctx.modes.isSet(Modes.OperatorMode))
      Right(ReplyYouAreOperator(ctx))
    // check user + password
    else if (validAuth(name, password)) {
      ctx.modes.applyModes(List("+o"))

      // send an extra MODE message that reflects the change
      Right(ListResponse(List(
        StringResponse(s"MODE ${ctx.nick} :+o", ctx),
        ReplyYouAreOperator(ctx)), ctx))
    }
    else
      Left(ErrorPasswordMismatch(ctx))
  }
}
