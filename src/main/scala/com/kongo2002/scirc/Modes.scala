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

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}

object Modes {

  // argument type enumeration
  object ModeArgumentType extends Enumeration {
    type ModeArgumentType = Value
    val NoArg, OneArg, NArgs = Value
  }
  import ModeArgumentType._

  // mode operation enumeration
  object ModeOperationType extends Enumeration {
    type ModeOperationType = Value
    val SetMode, UnsetMode, ListMode = Value
  }
  import ModeOperationType._

  sealed abstract class IrcMode(
    val chr: Char,
    val arg: ModeArgumentType = NoArg,
    val internal: Boolean = false
  )

  case class ModeOperation(t: ModeOperationType, mode: IrcMode, args: List[String])

  // GENERAL USER MODES

  case object AwayMode            extends IrcMode('a')
  case object InvisibleMode       extends IrcMode('i')
  case object WallOpsMode         extends IrcMode('w')
  case object RestrictedMode      extends IrcMode('r')
  case object ServerRecipientMode extends IrcMode('s')
  case object OperatorMode        extends IrcMode('o')

  val userModes = toMap(Seq(
    AwayMode,
    InvisibleMode,
    WallOpsMode,
    RestrictedMode,
    ServerRecipientMode,
    OperatorMode
  ))

  // USER CHANNEL MODES

  case object ChannelOperatorMode   extends IrcMode('o', NArgs)
  // 'channel creator' flag
  case object VoiceMode             extends IrcMode('v', NArgs)
  case object LocalOperatorMode     extends IrcMode('O', OneArg, true)

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
  case object ChannelKeyMode        extends IrcMode('k', OneArg)
  case object UserLimitMode         extends IrcMode('l', OneArg)
  case object BanMaskMode           extends IrcMode('b', NArgs)
  case object ExceptionMaskMode     extends IrcMode('e', NArgs)
  case object InvitationMaskMode    extends IrcMode('I', NArgs)

  val channelModes = toMap(Seq(
    // user related
    ChannelOperatorMode,
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

  private def toMap(modes: Seq[IrcMode]): Map[Char, IrcMode] =
    modes.foldLeft(Map.empty[Char, IrcMode]) { (map, x) =>
      map + ((x.chr, x))
    }

  def toModeString(op: ModeOperation): String = {
    def sign = if (op.t == UnsetMode) '-' else '+'

    if (!op.mode.internal) {
      op.mode.arg match {
        case OneArg | NArgs if op.args.nonEmpty =>
          val arg = op.args.head
          s"$sign${op.mode.chr} $arg"
        case OneArg | NoArg => s"$sign${op.mode.chr}"
      }
    } else ""
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

    def modeString: String = {
      val (modes, args) = foldLeft(("", List[String]())) { (acc, x) =>
        val (ms, as) = acc
        val (key, value) = x

        if (!key.internal) {
          key.arg match {
            // do not include list modes in mode string
            case NArgs => acc
            case NoArg => (ms + key.chr, as)
            case OneArg => (ms + key.chr, as ++ value.toList)
          }
        } else acc
      }

      if (args.nonEmpty)
        "+" + modes + " " + args.mkString(" ")
      else
        "+" + modes
    }

    def singleSet(value: String) = new HashSet[String] += value

    def setMode(mode: IrcMode, arg: String): Boolean = {
      if (arg != "") {
        get(mode) match {
          case Some(v) =>
            // at this point we have to make sure that 1-argument modes
            // only store up to one argument at a time
            if (mode.arg == OneArg)
              update(mode, singleSet(arg))
            else
              update(mode, v += arg)
          case None =>
            update(mode, singleSet(arg))
        }
      } else
        update(mode, new HashSet[String])

      true
    }

    def unsetMode(mode: IrcMode, arg: String): Boolean = {
      if (arg != "") {
        get(mode) match {
          case Some(v) =>
            val newArgs = v -= arg

            // remove mode completely if the arguments are now empty
            if (newArgs.isEmpty)
              remove(mode)
            else
              update(mode, newArgs)

            true
          case None => false
        }
      } else {
        remove(mode).isDefined
      }
    }

    def isSet(chr: Char): Boolean = exists { case (k, _) => k.chr == chr }

    def isSet(mode: IrcMode): Boolean = contains(mode)

    def containsArg(mode: IrcMode, arg: String): Boolean = {
      get(mode) match {
        case Some(args) => args.contains(arg)
        case None => false
      }
    }

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

    def isOp(nick: String) = containsArg(ChannelOperatorMode, nick)
    def isCreator(nick: String) = containsArg(LocalOperatorMode, nick)
    def isVoice(nick: String) = containsArg(VoiceMode, nick)
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
        // no-argument mode
        case Some(mode) if mode.arg == NoArg =>
          Some((set, mode, ""))
        // one or N argument mode
        case Some(mode) =>
          if (args.nonEmpty)
            Some((set, mode, args.remove(0)))
          else if (mode.arg == NArgs)
            Some((ListMode, mode, ""))
          else if (set == UnsetMode)
            Some((set, mode, ""))
          // this is a 'one argument' mode without an argument
          else
            None
        // mode does not exist at all
        case None => None
      }
    }

    private def parseMode: Option[List[Char]] = mode match {
      // no mode set yet
      case Nil =>
        // arguments left?
        if (args.nonEmpty) {
          mode = args.head.toList
          args.remove(0)
          Some(mode)
        } else None
      // mode was already selected -> choose the rest
      case xs =>
        Some(xs)
    }
  }
}
