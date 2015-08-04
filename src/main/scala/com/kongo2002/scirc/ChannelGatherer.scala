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

import akka.actor.{Actor, ActorLogging, ActorRef}

import scala.concurrent.duration._

object ChannelGatherer {
  sealed abstract trait GathererResult
  case class JobResult[T](result: T) extends GathererResult
  case object NoResult extends GathererResult

  def apply[T, R](channels: Iterable[ActorRef],
      client: Client,
      message: T,
      finisher: (List[R], Client) => Unit,
      timeout: FiniteDuration = 1.seconds) =
    new ChannelGatherer[T, R](channels, client, message, finisher, timeout)
}

class ChannelGatherer[T, R](channels: Iterable[ActorRef],
    client: Client,
    message: T,
    finisher: (List[R], Client) => Unit,
    timeout: FiniteDuration = 1.seconds)
  extends Actor with ActorLogging {
  import ChannelGatherer._

  implicit val ec = context.dispatcher

  private var results = List.empty[R]
  private var processed = 0

  case object GatherTimeout

  // send request to all given channels
  val total = channels.foldLeft(0) { case (cnt, x) =>
    x ! message
    cnt + 1
  }

  val scheduler = context.system.scheduler
  val gatherTimeout = scheduler.scheduleOnce(timeout, self, GatherTimeout)

  private def sendResults {
    finisher(results, client)
    context stop self
  }

  private def check {
    if (processed >= total) {
      gatherTimeout.cancel
      sendResults
    }
  }

  def receive: Receive = {
    case NoResult =>
      processed += 1

      check

    case GatherTimeout =>
      log.debug(s"ChannelGatherer timed out after $timeout")
      sendResults

    case r: JobResult[R] =>
      processed += 1
      results = r.result :: results

      check
  }
}
