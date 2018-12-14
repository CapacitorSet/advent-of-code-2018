import scala.annotation.tailrec
import scala.io.Source

object Application {
	var velocities = Vector[(Int, Int)]()
	@tailrec
	def iterate(points: Vector[(Int, Int)], minX: Int, minY: Int): (Vector[(Int, Int)], Int, Int) = {
		val newPoints = points.zip(velocities).map {
			case ((x, y), (dx, dy)) => (x + dx, y + dy)
		}
		val newMinX = newPoints.map(_._1).min
		val newMinY = newPoints.map(_._2).min
		if (newMinX < minX || newMinY < minY)
			(points, minX, minY)
		else
			iterate(newPoints, newMinX, newMinY)
	}
	def main(args: Array[String]): Unit = {
		val format = "position=< ?([0-9-]+),  ?([0-9-]+)> velocity=< ?([0-9-]+),  ?([0-9-]+)>".r
		val (points, _velocities) = Source.fromFile("input").getLines
			.map {
				case format(x, y, vx, vy) => ((x.toInt, y.toInt), (vx.toInt, vy.toInt))
			}.toVector.unzip
		velocities = _velocities
		val minX = points.map(_._1).min
		val minY = points.map(_._2).min
		val (newPoints, newMinX, newMinY) = iterate(points, minX, minY)
		val newMaxX = newPoints.map(_._1).max
		val newMaxY = newPoints.map(_._2).max
		for (y <- newMinY to newMaxY) {
			for (x <- newMinX to newMaxX)
				print(if (newPoints contains ((x, y))) '#' else ' ')
			println()
		}
	}
}