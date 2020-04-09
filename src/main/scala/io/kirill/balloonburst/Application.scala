package io.kirill.balloonburst

object Application extends App {

  def readBalloons(): Either[String, Seq[Balloon]] = {
    val balloonsString = scala.io.StdIn.readLine
    Balloon.fromString(balloonsString)
  }

  def readCommands(currentBalloons: Seq[Balloon], currentScore: Score): Either[String, Score] = {
    if (currentBalloons.isEmpty) Right(currentScore)
    else
      Command.fromString(scala.io.StdIn.readLine).flatMap { command =>
        val (updateBalloons, updatedScore) = Command.run(command, currentBalloons, currentScore)
        if (command == Command.Inflate && updateBalloons.size != currentBalloons.size) {
          println("BURST")
        }
        readCommands(updateBalloons, updatedScore)
      }
  }

  val finalScore = for {
    balloons <- readBalloons()
    score <- readCommands(balloons, Score(0))
  } yield score

  finalScore match {
    case Right(score) => println(s"SCORE: ${score.value}")
    case Left(error) => println(s"ERROR: $error")
  }
}
