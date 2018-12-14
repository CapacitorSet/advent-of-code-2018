import scala.annotation.tailrec
import scala.io.Source

object Application {
	var numPlayers = 0
	var highestMarble = 0

	// Return a new circle, where the "current marble" is the head.
	@tailrec
	def iterate(circle: Vector[Int], marble: Int, scores: Map[Int, Long]): (Vector[Int], Int, Map[Int, Long]) =
		if (marble > highestMarble) {
			(circle, marble, scores)
		} else if (marble % 23 == 0) {
			val droppedMarble = circle(circle.length - 7)
			val currentPlayer = marble % numPlayers
			val newScore = scores(currentPlayer) + marble + droppedMarble
			val newCircle = circle.drop(circle.length - 6) ++ circle.take(circle.length - 7)
			iterate(newCircle, marble + 1, scores + (currentPlayer -> newScore))
		} else {
			val newCircle: Vector[Int] = circle.drop(2).+:(marble) ++ circle.take(2)
			iterate(newCircle, marble + 1, scores)
		}

	def main(args: Array[String]): Unit = {
		val format = raw"(\d+) players; last marble is worth (\d+) points".r
		val format(numPlayersStr, highestMarbleStr) = Source.fromFile("input").getLines.toList.head
		numPlayers = numPlayersStr.toInt
		highestMarble = highestMarbleStr.toInt * 100
		val initScores = (0 to numPlayers).map((_, 0L)).toMap
		val (_, _, scores) = iterate(Vector(0), 1, initScores)
		val (_, out) = scores.maxBy(_._2)
		println(out)
	}
}