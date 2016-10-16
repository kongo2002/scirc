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

import scala.collection.mutable.ArrayBuffer

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

  case object AwayMode extends IrcMode('a')
  case object InvisibleMode extends IrcMode('i')
  case object WallOpsMode extends IrcMode('w')
  case object RestrictedMode extends IrcMode('r')
  case object ServerRecipientMode extends IrcMode('s')
  case object OperatorMode extends IrcMode('o')

  val userModes = toMap(Seq(
    AwayMode,
    InvisibleMode,
    WallOpsMode,
    RestrictedMode,
    ServerRecipientMode,
    OperatorMode
  ))

  // USER CHANNEL MODES

  case object ChannelOperatorMode extends IrcMode('o', NArgs)
  // 'channel creator' flag
  case object VoiceMode extends IrcMode('v', NArgs)
  case object LocalOperatorMode extends IrcMode('O', OneArg, true)

  // CHANNEL RELATED MODES

  case object AnonymousMode extends IrcMode('a')
  case object InviteOnlyMode extends IrcMode('i')
  case object ModeratedMode extends IrcMode('m')
  case object NoOutsideMessageMode extends IrcMode('n')
  case object QuietMode extends IrcMode('q')
  case object PrivateMode extends IrcMode('p')
  case object SecretMode extends IrcMode('s')
  case object ServerReopMode extends IrcMode('r')
  case object OperatorTopicOnlyMode extends IrcMode('t')
  case object ChannelKeyMode extends IrcMode('k', OneArg)
  case object UserLimitMode extends IrcMode('l', OneArg)
  case object BanMaskMode extends IrcMode('b', NArgs)
  case object ExceptionMaskMode extends IrcMode('e', NArgs)
  case object InvitationMaskMode extends IrcMode('I', NArgs)

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

  type ModeSetEntries = Map[IrcMode, Set[String]]

  case class ModeSet(values: ModeSetEntries = Map.empty) {
    private def update(mode: IrcMode, args: Set[String]): (ModeSet, Boolean) =
      copy(values + (mode -> args)) -> true

    private def remove(mode: IrcMode): (ModeSet, Boolean) = {
      if (values.contains(mode))
        copy(values - mode) -> true
      else
        this -> false
    }

    def modeString: String = {
      val (modes, args) = values.foldLeft(("", List.empty[String])) { (acc, x) =>
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

    def setMode(mode: IrcMode, arg: String): (ModeSet, Boolean) = {
      if (arg.nonEmpty) {
        values.get(mode) match {
          case Some(v) =>
            // at this point we have to make sure that 1-argument modes
            // only store up to one argument at a time
            if (mode.arg == OneArg)
              update(mode, Set(arg))
            else
              update(mode, v + arg)
          case None =>
            update(mode, Set(arg))
        }
      } else
        update(mode, Set.empty[String])
    }

    def unsetMode(mode: IrcMode, arg: String): (ModeSet, Boolean) = {
      if (arg.nonEmpty) {
        values.get(mode) match {
          case Some(v) =>
            val newArgs = v - arg

            // remove mode completely if the arguments are now empty
            if (newArgs.isEmpty)
              remove(mode)
            else
              update(mode, newArgs)
          case None =>
            this -> false
        }
      } else {
        remove(mode)
      }
    }

    def isSet(chr: Char): Boolean = values.exists { case (k, _) => k.chr == chr }

    def isSet(mode: IrcMode): Boolean = values.contains(mode)

    def containsArg(mode: IrcMode, arg: String): Boolean =
      values.get(mode).exists(_.contains(arg))

    def getArgs(mode: IrcMode): List[String] = values.get(mode) match {
      case Some(x) => x.toList
      case None => Nil
    }

    def aggregateOps[A, B](seq: Seq[A])(func: (ModeSet, A) => (ModeSet, Option[B])): (ModeSet, List[B]) = {
      // we have to invoke 'func' for *every* item
      seq.foldLeft((this, List.empty[B])) {
        case ((set0, acc), x) =>
          func(set0, x) match {
            case (set, Some(value)) => (set, value +: acc)
            case (set, None) => (set, acc)
          }
      }
    }
  }

  trait ModeSetContainer {
    type ModeType <: ModeSetContainer
    def modes: Map[Char, IrcMode]
    def values: ModeSet

    def modeString: String = values.modeString
    def size: Int = values.values.size
    def isSet(chr: Char): Boolean = values.isSet(chr)
    def isSet(mode: IrcMode): Boolean = values.isSet(mode)
    def getArgs(mode: IrcMode): List[String] = values.getArgs(mode)

    protected def update(updated: ModeSet): ModeType

    private def toOp(op: ModeOperationType, mode: IrcMode, arg: String)(func: (IrcMode, String) => (ModeSet, Boolean)): (ModeSet, Option[ModeOperation]) = {
      val (result, modified) = func(mode, arg)
      if (modified) {
        val value = if (arg.nonEmpty) List(arg) else Nil
        (result, Some(ModeOperation(op, mode, value)))
      } else
        (result, None)
    }

    private def applyParser(parser: ModeParser): (ModeSet, List[ModeOperation]) = {
      parser.parse match {
        case Nil => (values, Nil)
        case xs => values.aggregateOps(xs) {
          case (set, (SetMode, mode, arg)) => toOp(SetMode, mode, arg)(set.setMode)
          case (set, (UnsetMode, mode, arg)) => toOp(UnsetMode, mode, arg)(set.unsetMode)
          case (set, (ListMode, mode, _)) =>
            (set, Some(ModeOperation(ListMode, mode, set.getArgs(mode))))
        }
      }
    }

    def applyModes(arguments: Seq[String]): (ModeType, List[ModeOperation]) = {
      val parser = new ModeParser(this, arguments)
      val (updated, operations) = applyParser(parser)

      update(updated) -> operations
    }
  }

  case class UserModeSet(values: ModeSet = ModeSet()) extends ModeSetContainer {
    override type ModeType = UserModeSet
    override def modes = userModes
    override def update(updated: ModeSet): UserModeSet = copy(updated)
  }

  case class ChannelModeSet(values: ModeSet = ModeSet()) extends ModeSetContainer {
    override type ModeType = ChannelModeSet
    override def modes = channelModes
    override def update(updated: ModeSet): ChannelModeSet = copy(updated)

    def isOp(nick: String) = values.containsArg(ChannelOperatorMode, nick)
    def isCreator(nick: String) = values.containsArg(LocalOperatorMode, nick)
    def isVoice(nick: String) = values.containsArg(VoiceMode, nick)
  }

  class ModeParser(modes: ModeSetContainer, arguments: Seq[String]) {
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
