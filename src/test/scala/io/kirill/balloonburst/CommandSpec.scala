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

  "A Command::run" should {

    "increment score and return remaining balloons when BANK used" in {
      val balloons = List(Balloon(1), Balloon(2))

      Command.run(Command.Bank, balloons, Score(2)) must be ((List(Balloon(2)), Score(3)))
    }

    "return remaining balloons and current score when balloon bursted with INFLATE" in {
      val balloons = List(Balloon(0), Balloon(2))

      Command.run(Command.Inflate, balloons, Score(2)) must be ((List(Balloon(2)), Score(2)))
    }

    "return balloons with the first inflated and current score when balloon has enough inflates with INFLATE" in {
      val balloons = List(Balloon(1), Balloon(2))

      Command.run(Command.Inflate, balloons, Score(2)) must be ((List(Balloon(0), Balloon(2)), Score(2)))
    }
  }
}
