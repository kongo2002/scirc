package com.kongo2002.scirc

object Utils {
  def splitFirst(input: String, at: String): List[String] = {
    var idx = input.indexOf(at)
    if (idx < 0)
      List(input)
    else {
      val (fst, snd) = input.splitAt(idx)
      List(fst, snd.drop(at.size))
    }
  }
}
