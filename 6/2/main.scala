import scala.io.Source

object Application {
	def main(args: Array[String]): Unit = {
		val format = raw"(\d+), (\d+)".r()
		val locations: List[(Int, Int)] = Source.fromFile("input").getLines
			.map { case format(x, y) => (x.toInt, y.toInt) }
    		.toList
		val (xs, ys) = locations.unzip
		val out = (for (x <- xs.min to xs.max; y <- ys.min to ys.max) yield (x, y))
			.count { case (x, y) =>
				locations.map { case (locX, locY) => Math.abs(locX - x) + Math.abs(locY - y) }
    			.sum < 10000
			}
		println(out)
	}
}