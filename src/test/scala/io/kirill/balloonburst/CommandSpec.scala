package io.kirill.balloonburst

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CommandSpec extends AnyWordSpec with Matchers {

  "A Command::fromString" should {

    "return right with a command" in {
      Command.fromString("INFLATE") must be (Right(Command.Inflate))
      Command.fromString("BANK") must be (Right(Command.Bank))
    }

    "return left when input is unexpected command" in {
      Command.fromString("FOO") must be (Left("unexpected command FOO"))
    }
  }
}
