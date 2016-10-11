package com.kongo2002.scirc.errors

class UnexpectedException(msg: String)
  extends RuntimeException(s"captain! we've been hit: $msg")
