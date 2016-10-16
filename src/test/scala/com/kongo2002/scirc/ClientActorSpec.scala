package com.kongo2002.scirc

import org.scalatest.WordSpecLike

class ClientActorSpec extends TestClient("ClientActorSpec")
    with WordSpecLike {

  "ClientActor" should {
    "reject unknown commands" in {
      sendExpect("FOO", "ERROR :unknown command\r\n")
    }

    "accept commands" in {
      sendExpect("PING localhost", ":test.localhost PONG test.localhost :test.localhost\r\n")
    }
  }
}
