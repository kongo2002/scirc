package com.kongo2002.scirc

import akka.actor.{Actor, ActorRef}

import scala.collection.immutable.HashMap

object NickManager {
  // requests
  case class RegisterNick(nick: String, rec: ActorRef)
  case class ChangeNick(from: String, to: String, rec: ActorRef)
  case class NickCount(rec: ActorRef)
  case class DisconnectNick(nick: String)
  case class OnlineNicks(nicks: List[String], rec: ActorRef)

  // responses
  case class NickAck(newNick: String, rec: ActorRef)
  case class NickErr(error: String, rec: ActorRef)
  case class Nicks(count: Int, rec: ActorRef)
  case class NicksOnline(nicks: List[String], rec: ActorRef)
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

    case OnlineNicks(ns, rec) =>
      val online = ns.foldLeft(List[String]()) { (xs: List[String], x: String) =>
        nicks.get(x) match {
          case Some(nick) => x :: xs
          case None => xs
        }
      }

      if (!online.isEmpty)
        sender ! NicksOnline(online, rec)

    case DisconnectNick(nick) =>
      nicks = nicks - nick

    case NickCount(rec) =>
      sender ! Nicks(nicks.size, rec)
  }
}
