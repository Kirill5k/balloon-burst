package io.kirill.balloonburst

import io.kirill.balloonburst.Command._

object Application extends App {

  def runCommand(command: Command, currentBalloons: Seq[Balloon], currentScore: Score): (Seq[Balloon], Score) = {
    command match {
      case Bank => (currentBalloons.tail, currentScore)
      case Inflate =>
        currentBalloons.head.inflate() match {
          case Some(balloon) => (balloon +: currentBalloons.tail, currentScore)
          case None => (currentBalloons.tail, currentScore.increment())
        }
    }
  }

  val balloonsString = scala.io.StdIn.readLine
  val balloons = Balloon.fromString(balloonsString)
}
