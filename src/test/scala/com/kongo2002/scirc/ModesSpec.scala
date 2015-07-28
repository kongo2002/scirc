package com.kongo2002.scirc

import org.scalatest.{FlatSpec, Matchers}

class ModesSpec extends FlatSpec with Matchers {
  import Modes._

  def user(x: String*) = new ModeParser(new UserModeSet, x).parse
  def channel(x: String*) = new ModeParser(new ChannelModeSet, x).parse

  def userSet(x: String*) = {
    val set = new UserModeSet
    set.applyModes(x)
    set
  }

  def channelSet(x: String*) = {
    val set = new ChannelModeSet
    set.applyModes(x)
    set
  }

  "ModeParser" should "parse no arguments" in {
    user() should be (List())
  }

  it should "parse one argument with sign" in {
    user("+i").size should be (1)
  }

  it should "parse one argument without sign" in {
    user("i").size should be (1)
  }

  it should "parse multiple arguments with sign" in {
    user("+ia").size should be (2)
  }

  it should "parse multiple arguments without sign" in {
    user("ia").size should be (2)
  }

  it should "parse invisible mode" in {
    user("i") should be (List((true, InvisibleMode, "")))
  }

  it should "parse positive sign" in {
    user("+i") should be (List((true, InvisibleMode, "")))
  }

  it should "parse negative sign" in {
    user("-i") should be (List((false, InvisibleMode, "")))
  }

  it should "parse mode with argument" in {
    channel("b", "foo") should be (List((true, BanMaskMode, "foo")))
  }

  it should "parse mode and ignore invalid argument" in {
    channel("m", "foo") should be (List((true, ModeratedMode, "")))
  }

  it should "parse multiple modes with arguments" in {
    channel("bI", "foo", "bar") should be
      (List((true, BanMaskMode, "foo"), (true, InvitationMaskMode, "bar")))
  }

  it should "parse multiple modes with arguments with different signs" in {
    channel("+b-I", "foo", "bar") should be
      (List((true, BanMaskMode, "foo"), (false, InvitationMaskMode, "bar")))
  }

  it should "parse multiple modes with arguments with different signs in another order" in {
    channel("+b", "foo", "-I", "bar") should be
      (List((true, BanMaskMode, "foo"), (false, InvitationMaskMode, "bar")))
  }

  "UserModeSet" should "apply a mode" in {
    userSet("+i").size should be (1)
  }

  it should "apply the invisible mode" in {
    val set = userSet("i")
    set.isSet('i') should be (true)
    set.isSet(InvisibleMode) should be (true)
  }

  it should "ignore unknown mode" in {
    userSet("+x").size should be (0)
  }

  it should "ignore channel modes" in {
    userSet("+I").isSet('I') should be (false)
  }

  "ChannelModeSet" should "apply ban list mode" in {
    channelSet("+b").isSet('b') should be (true)
  }

  it should "apply ban list mode with argument" in {
    channelSet("+b", "foo").isSet('b') should be (true)
    channelSet("+b", "foo").size should be (1)
  }

  it should "get ban list arguments" in {
    val set = channelSet("+b", "foo")
    set.applyModes(List("b", "bar"))

    set.getArgs(BanMaskMode) should be (List("bar", "foo"))
  }

  it should "remove ban list arguments" in {
    val set = channelSet("+b", "foo")
    set.applyModes(List("b", "bar"))

    set.getArgs(BanMaskMode) should be (List("bar", "foo"))

    set.applyModes(List("-b", "foo"))

    set.getArgs(BanMaskMode) should be (List("bar"))
  }
}
