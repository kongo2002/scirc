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

import akka.actor.{ Actor, ActorLogging }
import akka.event.LoggingReceive
import akka.io.Tcp
import akka.util.ByteString

import Response._

trait CommandHandler extends HasClientMetrics {
  def handleCommand(cmd: String): Response
}

trait CommandProcessor extends Actor with ActorLogging {
  this: CommandHandler =>

  val crlf = "\r\n"
  val buffer = new StringBuilder

  private def parseBuffer: List[String] = {
    var idx = 0

    def next: Option[String] = {
      val to = buffer.indexOf(crlf, idx)
      if (to > 0) {
        val cmd = buffer.slice(idx, to)
        idx = to + crlf.length
        Some(cmd.stripLineEnd)
      } // empty line
      else if (to == 0) {
        buffer.delete(0, crlf.length)
        Some("")
      } else {
        None
      }
    }

    def parse: List[String] = {
      next match {
        case Some("") => parse
        case Some(cmd) => cmd :: parse
        case None =>
          // nothing more to match (so far)
          // remove processed contents
          buffer.delete(0, idx)
          Nil
      }
    }

    parse
  }

  def sendMsg(msg: Reply, sendFunc: String => Unit): Unit = {
    if (msg.hasReply)
      sendFunc(msg.getMessage)
  }

  def send(msg: String): Unit = {
    val bs = ByteString(msg)
    sender ! Tcp.Write(bs)

    metrics.sentBytesCounter.increment(bs.length)
    log.debug(s"<<< $msg")
  }

  def sendTo(to: Client)(msg: String): Unit = {
    val bs = ByteString(msg)
    to.socket ! Tcp.Write(bs)

    metrics.sentBytesCounter.increment(bs.length)
    log.debug(s"<<< $msg")
  }

  def process(cmd: String): Unit = {
    log.debug(s">>> $cmd")

    handleCommand(cmd) match {
      case Right(res) => sendMsg(res, send)
      case Left(e) =>
        // TODO: log error?
        sendMsg(e, send)
    }
  }

  def httpReceive: Receive = LoggingReceive {
    case Tcp.Received(bs) =>
      val str = bs.utf8String
      buffer.append(str)

      val cmds = parseBuffer
      cmds foreach process
  }
}
