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

object Modes {
  import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}

  sealed abstract class IrcMode(val chr: Char, val acceptArg: Boolean = false)
  sealed abstract class ArgIrcMode(chr: Char)
    extends IrcMode(chr, true)

  object ModeOperationType extends Enumeration {
    type ModeOperationType = Value
    val SetMode, UnsetMode, ListMode = Value
  }
  import ModeOperationType._

  case class ModeOperation(t: ModeOperationType, mode: IrcMode, args: List[String])

  // GENERAL USER MODES

  case object AwayMode            extends IrcMode('a')
  case object InvisibleMode       extends IrcMode('i')
  case object WallOpsMode         extends IrcMode('w')
  case object RestrictedMode      extends IrcMode('r')
  case object ServerRecipientMode extends IrcMode('s')

  val userModes = toMap(Seq(
    AwayMode,
    InvisibleMode,
    WallOpsMode,
    RestrictedMode,
    ServerRecipientMode
  ))

  // USER CHANNEL MODES

  case object OperatorMode        extends IrcMode('o')
  // 'channel creator' flag
  case object LocalOperatorMode   extends IrcMode('O')
  case object VoiceMode           extends IrcMode('v')

  // CHANNEL RELATED MODES

  case object AnonymousMode         extends IrcMode('a')
  case object InviteOnlyMode        extends IrcMode('i')
  case object ModeratedMode         extends IrcMode('m')
  case object NoOutsideMessageMode  extends IrcMode('n')
  case object QuietMode             extends IrcMode('q')
  case object PrivateMode           extends IrcMode('p')
  case object SecretMode            extends IrcMode('s')
  case object ServerReopMode        extends IrcMode('r')
  case object OperatorTopicOnlyMode extends IrcMode('t')
  case object ChannelKeyMode        extends ArgIrcMode('k')
  case object UserLimitMode         extends ArgIrcMode('l')
  case object BanMaskMode           extends ArgIrcMode('b')
  case object ExceptionMaskMode     extends ArgIrcMode('e')
  case object InvitationMaskMode    extends ArgIrcMode('I')

  val channelModes = toMap(Seq(
    // user related
    OperatorMode,
    LocalOperatorMode,
    VoiceMode,
    // channel related
    AnonymousMode,
    InviteOnlyMode,
    ModeratedMode,
    NoOutsideMessageMode,
    QuietMode,
    PrivateMode,
    SecretMode,
    ServerReopMode,
    OperatorTopicOnlyMode,
    ChannelKeyMode,
    UserLimitMode,
    BanMaskMode,
    ExceptionMaskMode,
    InvitationMaskMode
  ))

  private def toMap(modes: Seq[IrcMode]): Map[Char, IrcMode] = {
    modes.foldLeft(Map.empty[Char, IrcMode]) { (map, x) =>
      map + ((x.chr, x))
    }
  }

  abstract class ModeSet extends HashMap[IrcMode, HashSet[String]] {
    val modes: Map[Char, IrcMode]

    private def aggregateOps[A, B](seq: Seq[A])(func: A => Option[B]): List[B] = {
      // we have to invoke 'func' for *every* item
      seq.foldLeft(List[B]()) { (acc, x) =>
        func(x) match {
          case Some(value) => value +: acc
          case None => acc
        }
      }
    }

    // TODO: not sure if we have to filter for 'non-argument' modes
    def modeString: String =
      "+" + keys.map(_.chr).mkString

    def setMode(mode: IrcMode, arg: String): Boolean = {
      if (arg != "") {
        get(mode) match {
          case Some(v) => update(mode, v += arg)
          case None =>
            var single = new HashSet[String]
            single += arg
            update(mode, single)
        }
      } else
        update(mode, new HashSet[String])

      true
    }

    def unsetMode(mode: IrcMode, arg: String): Boolean = {
      if (arg != "") {
        get(mode) match {
          case Some(v) => update(mode, v -= arg); true
          case None => false
        }
      } else {
        remove(mode).isDefined
      }
    }

    def isSet(chr: Char): Boolean = exists { case (k, _) => k.chr == chr }

    def isSet(mode: IrcMode): Boolean = contains(mode)

    def getArgs(mode: IrcMode): List[String] = get(mode) match {
      case Some(x) => x.toList
      case None => List()
    }

    private def toOp(op: ModeOperationType, mode: IrcMode, arg: String)
      (func: (IrcMode, String) => Boolean): Option[ModeOperation] = {
      if (func(mode, arg)) {
        val value = if (arg != "") List(arg) else List()
        Some(ModeOperation(op, mode, value))
      }
      else
        None
    }

    def applyModes(arguments: Seq[String]): List[ModeOperation] = {
      val parser = new ModeParser(this, arguments)
      parser.parse match {
        case Nil => List()
        case xs => aggregateOps(xs) {
          case (SetMode, mode, arg)  => toOp(SetMode, mode, arg)(setMode)
          case (UnsetMode, mode, arg) => toOp(UnsetMode, mode, arg)(unsetMode)
          case (ListMode, mode, _) =>
            Some(ModeOperation(ListMode, mode, getArgs(mode)))
        }
      }
    }
  }

  class UserModeSet extends ModeSet {
    val modes = userModes
  }

  class ChannelModeSet extends ModeSet {
    val modes = channelModes
  }

  class ModeParser(modes: ModeSet, arguments: Seq[String]) {
    private var set = SetMode
    private var mode: List[Char] = Nil

    private val args = new ArrayBuffer[String]
    args.appendAll(arguments)

    def parse: List[(ModeOperationType, IrcMode, String)] = parseMode match {
      case None => Nil
      case Some(ms) =>
        // extract next character
        val res = ms match {
          case '+' :: m :: ms =>
            set = SetMode
            mode = ms
            m
          case '-' :: m :: ms =>
            set = UnsetMode
            mode = ms
            m
          case m :: ms =>
            mode = ms
            m
        }
        // lookup mode and attach (optional) arguments
        attachModeArgs(set, res) match {
          case Some(x) => x :: parse
          case None => parse
        }
    }

    private def attachModeArgs(set: ModeOperationType, chr: Char) = {
      // lookup mode (use available modes of the given set)
      modes.modes.get(chr) match {
        case Some(mode) =>
          val (arg, opType) =
            if (mode.acceptArg) {
              if (args.size > 0)
                (args.remove(0), set)
              else
                ("", ListMode)
            }
            else ("", set)
          Some((opType, mode, arg))
        case None => None
      }
    }

    private def parseMode: Option[List[Char]] = mode match {
      // no mode set yet
      case Nil =>
        // arguments left?
        if (args.size > 0) {
          mode = args(0).toList
          args.remove(0)
          Some(mode)
        } else None
      // mode was already selected -> choose the rest
      case xs =>
        Some(xs)
    }
  }
}
