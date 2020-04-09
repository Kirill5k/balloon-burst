package io.kirill.balloonburst

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BalloonSpec extends AnyWordSpec with Matchers {

  "A Balloon::inflate" should {

    "return option with a balloon when it has enough inflates left" in {
      val balloon = Balloon(5, 0)

      balloon.inflate() must be (Some(Balloon(5, 1)))
    }

    "return empty option when it has no inflates left" in {
      val balloon = Balloon(0, 0)

      balloon.inflate() must be (None)
    }
  }

  "A Balloon::fromString" should {

    "return right with a seq of balloons" in {
      Balloon.fromString("3 4 2") must be (Right(List(Balloon(3, 0), Balloon(4, 0), Balloon(2, 0))))
    }

    "return left when input string contains illegal characters" in {
      Balloon.fromString("3 foo 2") must be (Left("""invalid input string: For input string: "foo""""))
    }

    "return left when input string is empty" in {
      Balloon.fromString("") must be (Left("""invalid input string: For input string: """""))
    }

    "return left when input string has no numbers" in {
      Balloon.fromString(" ") must be (Left("invalid input string: is empty or contains balloons with negative inflates"))
    }

    "return left when input string contains negative numbers" in {
      Balloon.fromString("3 -1 2") must be (Left("invalid input string: is empty or contains balloons with negative inflates"))
    }
  }
}
