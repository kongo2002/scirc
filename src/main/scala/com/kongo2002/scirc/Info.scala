package com.kongo2002.scirc

object Info {
  case class UserWhoInfo(ctx: ClientContext,
    channel: String,
    away: Boolean,
    modes: String)
}
