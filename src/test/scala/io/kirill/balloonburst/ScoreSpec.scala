package io.kirill.balloonburst

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ScoreSpec extends AnyWordSpec with Matchers {

  "A Score::increment" should {

    "increase score by 1" in {
      val score = Score(2)

      score.increment() must be (Score(3))
    }
  }
}
