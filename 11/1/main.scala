object Application {
	val gridSN = 5177 // input
	def powerOf(x: Int, y: Int): Int = ((((x + 10) * y + gridSN) * (x + 10) / 100) % 10) - 5
	// (x, y) are top-left coordinates.
	def powerOfSquare(x: Int, y: Int, size: Int): Int =
		(for (dx <- 0 to size; dy <- 0 to size) yield powerOf(x + dx, y + dy)).sum
	def main(args: Array[String]): Unit = {
		val cellScores: Seq[((Int, Int), Int)] = for (x <- 1 to 299; y <- 1 to 299)
			yield (x, y) -> powerOfSquare(x, y, 3)
		val out = cellScores.maxBy(_._2)._1
		println(out._1, out._2)
	}
}