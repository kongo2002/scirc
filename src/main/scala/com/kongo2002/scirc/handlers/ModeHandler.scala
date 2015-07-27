package com.kongo2002.scirc.handlers

import com.kongo2002.scirc._
import com.kongo2002.scirc.Response._

trait ModeHandler extends BaseHandler {
  this: ClientActor =>

  def handleMode(op: Operation, client: Client): Response = {
    val numArgs = op.args.size

    val target = op.get(0)
    val mode = op.get(1)

    if (target == ctx.nick) {
      // set modes if given
      if (mode != "") {
        log.debug(s"${ctx.nick}: MODE request '$mode'")

        if (ctx.modes.applyMode(mode))
          log.debug(s"${ctx.nick}: new MODE set '${ctx.modes.modeString}'")
      }

      Right(ReplyUserModeIs(ctx.modes))
    }
    else
      Left(ErrorUsersDontMatch)
  }
}
