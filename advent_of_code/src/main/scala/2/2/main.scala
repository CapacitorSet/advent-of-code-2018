import scala.io.Source

object Application22 {
	def main(args: Array[String]): Unit = {
		val ids = Source.fromFile("input").getLines.toList
		val originalLength = ids(0).length

		val pairs = ids.combinations(2)
		// Drops mismatching characters
		val orderedIntersect: (String, String) => String
			= (a, b) => a.zip(b).flatMap(
				pair => if (pair._1 == pair._2) Some(pair._1) else None
			).mkString
		val out = pairs
			.map(list => orderedIntersect(list(0), list(1)))
			// Find the one pair where only one letter differs
			.find(_.length == originalLength - 1)

		println(out.getOrElse("No such ID."))
	}
}