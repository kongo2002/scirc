/* Copyright 2015 Gregor Uhlenheuer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.kongo2002.scirc

import org.scalatest.{ FlatSpec, Matchers }

class ModesSpec extends FlatSpec with Matchers {
  import Modes._
  import ModeOperationType._

  def user(x: String*) = new ModeParser(UserModeSet(), x).parse
  def channel(x: String*) = new ModeParser(ChannelModeSet(), x).parse

  def userSet(x: String*) =
    UserModeSet().applyModes(x)._1

  def channelSet(x: String*) =
    ChannelModeSet().applyModes(x)._1

  "ModeParser" should "parse no arguments" in {
    user() should be(List())
  }

  it should "parse one argument with sign" in {
    user("+i").size should be(1)
  }

  it should "parse one argument without sign" in {
    user("i").size should be(1)
  }

  it should "parse multiple arguments with sign" in {
    user("+ia").size should be(2)
  }

  it should "parse multiple arguments without sign" in {
    user("ia").size should be(2)
  }

  it should "parse invisible mode" in {
    user("i") should be(List((SetMode, InvisibleMode, "")))
  }

  it should "parse positive sign" in {
    user("+i") should be(List((SetMode, InvisibleMode, "")))
  }

  it should "parse negative sign" in {
    user("-i") should be(List((UnsetMode, InvisibleMode, "")))
  }

  it should "parse mode with argument" in {
    channel("b", "foo") should be(List((SetMode, BanMaskMode, "foo")))
  }

  it should "parse mode and ignore invalid argument" in {
    channel("m", "XXX") should be(List((SetMode, ModeratedMode, "")))
  }

  it should "parse multiple modes with arguments" in {
    channel("bI", "foo", "bar") should be
    List((SetMode, BanMaskMode, "foo"), (true, InvitationMaskMode, "bar"))
  }

  it should "parse multiple modes with arguments with different signs" in {
    channel("+b-I", "foo", "bar") should be
    List((SetMode, BanMaskMode, "foo"), (UnsetMode, InvitationMaskMode, "bar"))
  }

  it should "parse multiple modes with arguments with different signs in another order" in {
    channel("+b", "foo", "-I", "bar") should be
    List((SetMode, BanMaskMode, "foo"), (UnsetMode, InvitationMaskMode, "bar"))
  }

  "UserModeSet" should "apply a mode" in {
    userSet("+i").size should be(1)
  }

  it should "apply the invisible mode" in {
    val set = userSet("i")
    set.isSet('i') should be(true)
    set.isSet(InvisibleMode) should be(true)
  }

  it should "ignore unknown mode" in {
    userSet("+x").size should be(0)
  }

  it should "ignore channel modes" in {
    userSet("+I").isSet('I') should be(false)
  }

  "ChannelModeSet" should "not apply ban list mode without argument" in {
    channelSet("+b").isSet('b') should be(false)
  }

  it should "apply ban list mode with argument" in {
    channelSet("+b", "foo").isSet('b') should be(true)
    channelSet("+b", "foo").size should be(1)
  }

  it should "get ban list arguments" in {
    val set = channelSet("+b", "foo")
      .applyModes(List("b", "bar"))._1

    set.getArgs(BanMaskMode).sorted should be(List("bar", "foo"))
  }

  it should "remove ban list arguments" in {
    val set0 = channelSet("+b", "foo")
      .applyModes(List("b", "bar"))._1

    set0.getArgs(BanMaskMode).sorted should be(List("bar", "foo"))

    val set = set0.applyModes(List("-b", "foo"))._1

    set.getArgs(BanMaskMode) should be(List("bar"))
  }

  it should "remove channel key #1" in {
    val set0 = channelSet("+k", "secret")

    set0.getArgs(ChannelKeyMode) should be(List("secret"))

    val set = set0.applyModes(List("-k"))._1

    set.isSet(ChannelKeyMode) should be(false)
    set.getArgs(ChannelKeyMode) should be(Nil)
    set.modeString should be("+")

  }

  it should "remove channel key #2" in {
    val set0 = channelSet("+k", "secret")

    set0.getArgs(ChannelKeyMode) should be(List("secret"))

    val set = set0.applyModes(List("-k", "secret"))._1

    set.isSet(ChannelKeyMode) should be(false)
    set.getArgs(ChannelKeyMode) should be(Nil)
    set.modeString should be("+")

  }

  it should "output a basic mode string" in {
    val set = channelSet("+a")

    set.modeString should be("+a")
  }

  it should "output a mode string with arguments" in {
    val set = channelSet("+l", "100")

    set.modeString should be("+l 100")
  }

  it should "output a mode string and ignore list modes" in {
    val set = channelSet("+lb", "100", "bar")

    set.modeString should be("+l 100")
  }

  it should "not output internal modes" in {
    val set = channelSet("+lO", "100", "foo")

    set.getArgs(LocalOperatorMode) should be(List("foo"))
    set.modeString should be("+l 100")
  }

  it should "replace single argument modes" in {
    val set0 = channelSet("+l", "100")

    set0.modeString should be("+l 100")

    val set = set0.applyModes(List("+l", "200"))._1

    set.modeString should be("+l 200")
    set.getArgs(UserLimitMode) should be(List("200"))
  }
}
