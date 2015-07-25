package com.kongo2002.scirc

case class ServerContext(host: String, modes: Int) {
  val created = java.util.Calendar.getInstance().getTime()
}

case class ClientContext(ctx: ServerContext, var nick: String) {
  var user = ""
  var realname = ""
  var modes = 0
}
