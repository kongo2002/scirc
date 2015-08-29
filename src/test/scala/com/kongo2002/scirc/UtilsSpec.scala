package com.kongo2002.scirc

import org.scalatest.{FlatSpec, Matchers}

class UtilsSpec extends FlatSpec with Matchers {

  import Utils._

  "splitFirst" should "work on empty string" in {
    splitFirst("", "some") should be (List(""))
  }

  it should "work on empty split string" in {
    splitFirst("some", "") should be (List("some"))
  }

  it should "split correctly" in {
    splitFirst("foobarfoo", "bar") should be (List("foo", "foo"))
  }

  it should "split correctly at end of string" in {
    splitFirst("foobar", "bar") should be (List("foo"))
  }

  it should "split correctly at start of string" in {
    splitFirst("barfoo", "bar") should be (List("foo"))
  }

  it should "not touch on unmatched" in {
    splitFirst("foo bar", "ham") should be (List("foo bar"))
  }
}
