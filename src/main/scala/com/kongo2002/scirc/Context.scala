package com.kongo2002.scirc

case class ServerContext(host: String, modes: Int) {
  val created = java.util.Calendar.getInstance().getTime()
}

case class ClientContext(ctx: ServerContext, host: String, var nick: String) {
  var user = ""
  var realname = ""
  var isRegistered = false

  val modes = new Modes.ModeSet

  def prefix = s"$nick!$user@$host"
}
