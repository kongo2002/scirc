package com.kongo2002.scirc

import akka.actor.{Actor, ActorRef, ActorLogging}

import scala.collection.mutable.Map
import scala.util.matching.Regex

import Response._

object NickManager {
  // TODO: this regex is not 100% accurate
  val valid = new Regex("""[^&#!+][-_a-zA-Z0-9]+""")

  def isValidNick(nick: String): Boolean = {
    nick match {
      case valid(_*) => true
      case _ => false
    }
  }

  // requests
  case class RegisterNick(nick: String, client: Client)
  case class ChangeNick(from: String, to: String, client: Client)
  case class NickCount(client: Client)
  case class DisconnectNick(nick: String)
  case class OnlineNicks(nicks: List[String], client: Client)
  case class Registered(client: Client)
  case class WhoIs(client: Client)
  case class WhoIsNicks(nicks: Array[String], client: Client)

  // responses
  case class NickAck(newNick: String, client: Client)
  case class NickErr(error: ErrorResponse, client: Client)
  case class Nicks(count: Int, client: Client)
  case class NicksOnline(nicks: List[String], client: Client)
}

class NickManager extends Actor with ActorLogging with SendActor {
  var nicks = Map.empty[String, ActorRef]

  import NickManager._

  def whois(nick: String, client: Client) = {
    nicks.get(nick) match {
      case Some(ref) =>
        // request WHOIS information of respective client
        ref ! WhoIs(client)
      case None =>
        // specified nick does not exist
        sender ! Err(ErrorNoSuchNick(nick), client)
    }
  }

  def receive: Receive = {

    case ChangeNick(from, to, client) =>
      nicks.get(from) match {
        case Some(ref) =>
          val exists = !nicks.get(to).isEmpty

          // nick already exists
          if (exists)
            sender ! NickErr(ErrorNickAlreadyInUse(to), client)
          // 'free' nick
          else if (sender == ref) {
            // validate nickname
            if (isValidNick(to)) {
              nicks -= from += (to -> sender)
              sender ! NickAck(to, client)

              log.info(s"changed nick from '$from' to '$to'")
            } else {
              sender ! NickErr(ErrorErroneousNick(to), client)
            }
          }
          else
            // TODO: use correct numeric reply
            sender ! NickErr(StringError("invalid user"), client)
        case None =>
          // TODO: use correct numeric reply
          sender ! NickErr(StringError("user does not exist"), client)
      }

    case RegisterNick(nick, client) =>
      val exists = !nicks.get(nick).isEmpty

      if (exists)
        sender ! NickErr(ErrorNickAlreadyInUse(nick), client)
      else {
        // validate nickname
        if (isValidNick(nick)) {
          nicks += (nick -> sender)
          sender ! NickAck(nick, client)

          log.info(s"registered nick '$nick'")
        } else {
          sender ! NickErr(ErrorErroneousNick(nick), client)
        }
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

    case WhoIsNicks(ns, client) =>
      ns.foreach (n => whois(n, client))

    case DisconnectNick(nick) =>
      nicks -= nick

    case NickCount(client) =>
      sender ! Nicks(nicks.size, client)
  }
}
