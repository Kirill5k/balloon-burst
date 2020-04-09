package io.kirill.balloonburst

import scala.util.{Failure, Success, Try}

final case class Balloon(initialInflates: Int, currentInflates: Int) {

  def inflate(): Option[Balloon] = {
    val remainingInflates = initialInflates - currentInflates - 1
    if (remainingInflates >= 0) Some(Balloon(initialInflates, currentInflates + 1)) else None
  }
}

object Balloon {
  def fromString(inputLine: String): Either[String, Seq[Balloon]] =
    (for {
      stringsArrays <- Try(inputLine.split(" "))
      numbersArray <- Try(stringsArrays.map(_.toInt))
      balloons <- if (numbersArray.nonEmpty && numbersArray.forall(_ >= 0)) Success(numbersArray.map(i => Balloon(i, 0)).toList)
                  else Failure(new IllegalArgumentException("is empty or contains balloons with negative inflates"))
    } yield balloons).toEither.left.map(e => s"invalid input string: ${e.getMessage}")
}

final case class Score(value: Int)

sealed trait Command
object Command {
  case object Inflate extends Command
  case object Bank extends Command

  def fromString(command: String): Either[String, Command] = command match {
    case "INFLATE" => Right(Inflate)
    case "BANK" => Right(Bank)
    case c => Left(s"unexpected command $c")
  }

  def run(command: Command, balloons: Seq[Balloon], score: Score): (Seq[Balloon], Score) = command match {
    case Bank =>
      (balloons.tail, Score(score.value + balloons.head.currentInflates))
    case Inflate =>
      balloons.head.inflate() match {
        case Some(balloon) =>
          (balloon +: balloons.tail, score)
        case None =>
          (balloons.tail, score)
      }
  }
}

