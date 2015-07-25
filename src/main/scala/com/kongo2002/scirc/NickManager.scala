package com.kongo2002.scirc

import akka.actor.{Actor, ActorRef}

import scala.collection.immutable.HashMap

object NickManager {
  case class RegisterNick(nick: String, rec: ActorRef)
  case class ChangeNick(from: String, to: String, rec: ActorRef)
  case class NickCount(rec: ActorRef)

  case class NickAck(newNick: String, rec: ActorRef)
  case class NickErr(error: String, rec: ActorRef)
  case class Nicks(count: Int, rec: ActorRef)
}

class NickManager extends Actor {
  var nicks = new HashMap[String, ActorRef]

  import NickManager._

  def receive: Receive = {

    case ChangeNick(from, to, rec) =>
      nicks.get(from) match {
        case Some(ref) =>
          val exists = !nicks.get(to).isEmpty

          if (exists)
            sender ! NickErr("already in use", rec)
          else if (sender == ref) {
            nicks = nicks - from + ((to, sender))
            sender ! NickAck(to, rec)
          }
          else
            sender ! NickErr("invalid user", rec)
        case None =>
          sender ! NickErr("user does not exist", rec)
      }

    case RegisterNick(nick, rec) =>
      val exists = !nicks.get(nick).isEmpty

      if (exists)
        sender ! NickErr("already in use", rec)
      else {
        nicks = nicks + ((nick, sender))
        sender ! NickAck(nick, rec)
      }

    case NickCount(rec) =>
      sender ! Nicks(nicks.size, rec)
  }
}