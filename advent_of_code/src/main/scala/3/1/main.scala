import java.awt.Rectangle
import scala.io.Source

object Application31 {
	def rectToPoints(rect: Rectangle) =
		for {x <- rect.x until rect.x + rect.width
			 y <- rect.y until rect.y + rect.height}
			yield (x, y)

	def main(args: Array[String]): Unit = {
		val format = raw"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)".r()
		val claims: Iterator[Rectangle] = Source.fromFile("input").getLines
			.map { case format(_, x, y, dx, dy) => new Rectangle(x.toInt, y.toInt, dx.toInt, dy.toInt) }
		// For each pair of claims,
		val out = claims.toList.combinations(2)
			// intersect them
			.map { case List(left, right) => left intersection right }
			// get inner points
			.flatMap(rectToPoints)
			// count the number of unique points
			.toSet.size
		println(out)
	}
}