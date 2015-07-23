package com.kongo2002.scirc

import akka.actor.Actor
import akka.event.LoggingReceive
import akka.io.{IO, Tcp}
import akka.util.ByteString

import scala.util.{Try, Success, Failure}

class Client extends Actor
  with CommandHandler {

  val crlf = "\r\n"
  val buffer = new StringBuilder

  private def parseBuffer: List[String] = {
    var idx = 0

    def next: Option[String] = {
      val to = buffer.indexOf(crlf, idx)
      if (to > 0) {
        val cmd = buffer.slice(idx, to)
        idx = to + crlf.size
        Some(cmd.stripLineEnd)
      }
      else {
        None
      }
    }

    def parse: List[String] = {
      next match {
        case None => Nil
        case Some(cmd) => cmd :: parse
      }
    }

    parse
  }

  def writeResponse(cmd: String) = {
    val res = handleCommand(cmd) match {
      case Success(res) => res + crlf
      case Failure(x) => x.toString + crlf
    }

    sender ! Tcp.Write(ByteString(res))
  }

  def receive: Receive = LoggingReceive {
    case Tcp.Received(bs) =>
      val str = bs.utf8String
      buffer.append(str)

      val cmds = parseBuffer
      cmds foreach writeResponse

    case Tcp.PeerClosed =>
      println("Connection closed")
      context stop self
  }
}
