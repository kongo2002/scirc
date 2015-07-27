package com.kongo2002.scirc

object Modes {
  import scala.collection.mutable.HashSet

  sealed abstract class IrcMode(val chr: Char)

  case object AwayMode            extends IrcMode('a')
  case object InvisibleMode       extends IrcMode('i')
  case object WallOpsMode         extends IrcMode('w')
  case object RestrictedMode      extends IrcMode('r')
  case object OperatorMode        extends IrcMode('o')
  case object LocalOperatorMode   extends IrcMode('O')
  case object ServerRecipientMode extends IrcMode('s')

  val modes = Seq(
    AwayMode,
    InvisibleMode,
    WallOpsMode,
    RestrictedMode,
    OperatorMode,
    LocalOperatorMode,
    ServerRecipientMode)
      .foldLeft(Map.empty[Char, IrcMode]) { (map, x) =>
        map + ((x.chr, x))
      }

  def getMode(chr: Char) = modes.get _

  class ModeSet extends HashSet[IrcMode] {

    private def modifyMode(func: IrcMode => Boolean, chr: Char): Boolean = {
      modes.get(chr) match {
        case Some(mode) => func(mode)
        case None => false
      }
    }

    def modeString: String = map(_.chr).mkString

    def setMode(chr: Char): Boolean = modifyMode(add(_), chr)

    def unsetMode(chr: Char): Boolean = modifyMode(remove(_), chr)

    def applyMode(mode: String) = mode.toSeq match {
      case Seq('+', m@_*) => m.foreach(setMode _)
      case Seq('-', m@_*) => m.foreach(unsetMode _)
      case _ =>
    }
  }
}
