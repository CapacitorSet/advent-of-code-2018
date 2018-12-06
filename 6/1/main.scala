import scala.io.Source

object Application {
	def main(args: Array[String]): Unit = {
		val format = raw"(\d+), (\d+)".r()
		val locations: List[(Int, Int)] = Source.fromFile("input").getLines
			.map { case format(x, y) => (x.toInt, y.toInt) }
    		.toList
		val (xs, ys) = locations.unzip
		val (minX, maxX) = (xs.min, xs.max)
		val (minY, maxY) = (ys.min, ys.max)
		val out = (for (x <- minX to maxX; y <- minY to maxY) yield (x, y))
			// group points by the closest location
			.groupBy { case (x, y) =>
				val distances: Map[Int, List[(Int, Int)]] = locations.groupBy { case (locX, locY) => Math.abs(locX - x) + Math.abs(locY - y) }
				val mins = distances.minBy(_._1)._2
				if (mins.size == 1) mins.head else (-1, -1)
			}
			// skip points in the middle
    		.filterKeys { _ != (-1, -1) }
			// skip locations with infinite areas
			.filterKeys { case (x, y) => x != minX && x != maxX && y != minY && y != maxY }
    		.values.map(_.size)
			.max
		println(out)
	}
}