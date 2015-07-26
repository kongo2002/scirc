package com.kongo2002.scirc

import akka.actor.{Actor, ActorRef}

import scala.collection.mutable.Map

import Response._

object NickManager {
  // requests
  case class RegisterNick(nick: String, client: Client)
  case class ChangeNick(from: String, to: String, client: Client)
  case class NickCount(client: Client)
  case class DisconnectNick(nick: String)
  case class OnlineNicks(nicks: List[String], client: Client)

  // responses
  case class NickAck(newNick: String, client: Client)
  case class NickErr(error: ErrorResponse, client: Client)
  case class Nicks(count: Int, client: Client)
  case class NicksOnline(nicks: List[String], client: Client)
}

class NickManager extends Actor {
  var nicks = Map.empty[String, ActorRef]

  import NickManager._

  def receive: Receive = {

    case ChangeNick(from, to, client) =>
      nicks.get(from) match {
        case Some(ref) =>
          val exists = !nicks.get(to).isEmpty

          if (exists)
            sender ! NickErr(ErrorNickAlreadyInUse(to), client)
          else if (sender == ref) {
            nicks -= from += (to -> sender)
            sender ! NickAck(to, client)
          }
          else
            sender ! NickErr(StringError("invalid user"), client)
        case None =>
          sender ! NickErr(StringError("user does not exist"), client)
      }

    case RegisterNick(nick, client) =>
      val exists = !nicks.get(nick).isEmpty

      if (exists)
        sender ! NickErr(ErrorNickAlreadyInUse(nick), client)
      else {
        nicks += (nick -> sender)
        sender ! NickAck(nick, client)
      }

    case OnlineNicks(ns, client) =>
      val online = ns.foldLeft(List[String]()) { (xs: List[String], x: String) =>
        nicks.get(x) match {
          case Some(nick) => x :: xs
          case None => xs
        }
      }

      if (!online.isEmpty)
        sender ! NicksOnline(online, client)

    case DisconnectNick(nick) =>
      nicks -= nick

    case NickCount(client) =>
      sender ! Nicks(nicks.size, client)
  }
}
