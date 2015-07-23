package com.kongo2002.scirc

import akka.actor.Actor
import akka.event.LoggingReceive
import akka.io.{IO, Tcp}
import akka.util.ByteString

import scala.util.{Try, Success, Failure}

import Response._

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

  def errorResponse(e: ErrorResponse) = e match {
    case StringError(err) => err + crlf
  }

  def writeResponse(res: SuccessResponse) = res match {
    case EmptyResponse => None
    case StringResponse(str) => Some(str + crlf)
  }

  def process(cmd: String) = {
    def send(x: String) = sender ! Tcp.Write(ByteString(x))

    val res = handleCommand(cmd) match {
      case Right(res) => writeResponse(res) map (send _)
      case Left(e) => send(errorResponse(e))
    }
  }

  def receive: Receive = LoggingReceive {
    case Tcp.Received(bs) =>
      val str = bs.utf8String
      buffer.append(str)

      val cmds = parseBuffer
      cmds foreach process

    case Tcp.PeerClosed =>
      println("Connection closed")
      context stop self
  }
}
