import java.awt.Rectangle
import scala.io.Source

object Application {
	def main(args: Array[String]): Unit = {
		val format = raw"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)".r()
		val claims: Iterator[Rectangle] = Source.fromFile("input").getLines
			.map { case format(id, x, y, dx, dy) => new Rectangle(x.toInt, y.toInt, dx.toInt, dy.toInt) }
		// For each pair of claims,
		val out = claims.toList.combinations(2)
			// intersect them
			.map(pair => pair(0).intersection(pair(1)))
			// get inner points
			.flatMap(rect => for {x <- rect.x until rect.x + rect.width; y <- rect.y until rect.y + rect.height} yield (x, y))
			// count the number of unique points
			.toSet.size
		println(out)
	}
}