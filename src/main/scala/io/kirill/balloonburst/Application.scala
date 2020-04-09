package io.kirill.balloonburst

import io.kirill.balloonburst.Command.{Bank, Inflate, _}

object Application extends App {

  def readBalloons(): Either[String, Seq[Balloon]] = {
    val balloonsString = scala.io.StdIn.readLine
    Balloon.fromString(balloonsString)
  }

  def readCommand(): Either[String, Command] = {
    val commandString = scala.io.StdIn.readLine
    Command.fromString(commandString)
  }

  def readCommands(currentBalloons: Seq[Balloon], currentScore: Score): Either[String, Score] = {
    if (currentBalloons.isEmpty) Right(currentScore)
    else
      readCommand().flatMap { command =>
        val (updateBalloons, updatedScore) = Command.run(command, currentBalloons, currentScore)
        readCommands(updateBalloons, updatedScore)
      }
  }

  val finalScore = for {
    balloons <- readBalloons()
    score <- readCommands(balloons, Score(0))
  } yield score

  finalScore match {
    case Right(score) => println(s"SCORE :${score.value}")
    case Left(error) => println(s"ERROR: $error")
  }
}
