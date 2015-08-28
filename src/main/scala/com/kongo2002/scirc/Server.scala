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

import akka.actor.{Actor, ActorSystem, Props}
import akka.event.LoggingReceive
import akka.io.{IO, Tcp}

import scala.concurrent.duration._

import java.net.{InetSocketAddress, URI}

class Server(listen: URI, hostname: String) extends Actor {
  val host = listen.getHost
  val port = listen.getPort
  val ctx = ServerContext(hostname, 0)

  var nickManager = context.system.deadLetters
  var channelManager = context.system.deadLetters

  IO(Tcp)(context.system) ! Tcp.Bind(self, new InetSocketAddress(host, port))

  override def preStart() = {
    nickManager = context.actorOf(Props[NickManager], "nickmanager")
    channelManager = context.actorOf(Props(ChannelManager(ctx)), "channelmanager")
  }

  def receive: Receive = LoggingReceive {
    case Tcp.Connected(rem, _) =>
      sender ! Tcp.Register(context.actorOf(
        Props(ClientActor(ctx, rem, nickManager, channelManager))))
  }
}

object Server {
  def apply(port: Int, hostname: String) = {
    val uri = new URI(s"http://127.0.0.1:$port")
    new Server(uri, hostname)
  }

  def getPort(str: Array[String]) = {
    str.toList match {
      case h :: _ => h.toInt
      case _ => 6667
    }
  }

  def main(args: Array[String]) = {
    // TODO: configuration
    val hostname = "localhost"
    val port = getPort(args)

    val system = ActorSystem("scirc")
    val server = system.actorOf(Props(Server(port, hostname)), "server")

    println(s"scirc running on port $port")

    system.awaitTermination(Duration.Inf)
  }
}
