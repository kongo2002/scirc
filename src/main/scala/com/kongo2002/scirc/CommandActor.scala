package com.kongo2002.scirc

import akka.actor.{Actor, ActorRef}
import akka.event.LoggingReceive
import akka.io.{IO, Tcp}
import akka.util.ByteString

import scala.util.{Try, Success, Failure}

import Response._

abstract trait CommandActor extends Actor {
  this: CommandHandler =>

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
      // empty line
      else if (to == 0) {
        buffer.delete(0, crlf.size)
        Some("")
      }
      else {
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

  def errorResponse(e: ErrorResponse) = e match {
    case StringError(err) => err + crlf
    case x: ErrorNumericReply => x.getMessage + crlf
  }

  def writeResponse(res: SuccessResponse): Option[String] = res match {
    case EmptyResponse => None
    case StringResponse(str) => Some(str + crlf)
    case ListResponse(rs) =>
      Some(rs.map(writeResponse(_).getOrElse("")).mkString(""))
    case x: SuccessNumericReply => Some(x.getMessage + crlf)
  }

  def sendError(e: ErrorResponse, sendFunc: String => Unit) = {
    sendFunc(errorResponse(e))
  }

  def sendResponse(res: SuccessResponse, sendFunc: String => Unit) = {
    writeResponse(res) map (sendFunc)
  }

  def send(x: String) = sender ! Tcp.Write(ByteString(x))

  def sendTo(to: Client)(x: String) = to.socket ! Tcp.Write(ByteString(x))

  def process(cmd: String) = {
    val res = handleCommand(cmd) match {
      case Right(res) => sendResponse(res, send)
      case Left(e) => sendError(e, send)
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
