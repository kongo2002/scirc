package com.kongo2002.scirc

object Modes {
  import scala.collection.mutable.HashSet

  sealed abstract class IrcMode(val chr: Char)

  // USER MODES

  case object AwayMode            extends IrcMode('a')
  case object InvisibleMode       extends IrcMode('i')
  case object WallOpsMode         extends IrcMode('w')
  case object RestrictedMode      extends IrcMode('r')
  case object OperatorMode        extends IrcMode('o')
  // 'channel creator' flag
  case object LocalOperatorMode   extends IrcMode('O')
  case object ServerRecipientMode extends IrcMode('s')
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
  case object ChannelKeyMode        extends IrcMode('k')
  case object UserLimitMode         extends IrcMode('l')
  case object BanMaskMode           extends IrcMode('b')
  case object ExceptionMaskMode     extends IrcMode('e')
  case object InvitationMaskMode    extends IrcMode('I')

  private def toMap(modes: Seq[IrcMode]): Map[Char, IrcMode] = {
    modes.foldLeft(Map.empty[Char, IrcMode]) { (map, x) =>
      map + ((x.chr, x))
    }
  }

  val userModes = toMap(Seq(
    AwayMode,
    InvisibleMode,
    WallOpsMode,
    RestrictedMode,
    OperatorMode,
    LocalOperatorMode,
    VoiceMode,
    ServerRecipientMode
  ))

  val channelModes = toMap(Seq(
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

  abstract class ModeSet extends HashSet[IrcMode] {
    val modes: Map[Char, IrcMode]

    private def modifyMode(func: IrcMode => Boolean, chr: Char): Boolean = {
      modes.get(chr) match {
        case Some(mode) => func(mode)
        case None => false
      }
    }

    private def anyChange[A](seq: Seq[A], func: A => Boolean): Boolean = {
      seq.foldLeft(false) { (acc, x) => func(x) || acc }
    }

    def modeString: String = map(_.chr).mkString

    def setMode(chr: Char): Boolean = modifyMode(add(_), chr)

    def unsetMode(chr: Char): Boolean = modifyMode(remove(_), chr)

    def applyMode(mode: String): Boolean = mode.toSeq match {
      case Seq('+', m@_*) => anyChange(m, setMode _)
      case Seq('-', m@_*) => anyChange(m, unsetMode _)
      case _ => false
    }
  }

  class UserModeSet extends ModeSet {
    val modes = userModes
  }

  class ChannelModeSet extends ModeSet {
    val modes = channelModes
  }
}
