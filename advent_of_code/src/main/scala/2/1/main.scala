import scala.io.Source

object Application21 {
	def main(args: Array[String]): Unit = {
		val ids = Source.fromFile("input").getLines
		val freqData: Array[Set[Int]] = ids.map(
			_.toList // Individual letters
				.groupBy(identity).values // Array of occurrences
				.map(_.length).toSet // Set of number-of-occurrences
		).toArray
		val twos = freqData count(_ contains 2)
		val threes = freqData count(_ contains 3)
		val out = twos * threes
		println(out)
	}
}