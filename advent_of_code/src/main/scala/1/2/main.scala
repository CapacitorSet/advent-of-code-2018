import scala.io.Source

object Application12 {
	def main(args: Array[String]): Unit = {
		val freqs = Source.fromFile("input").getLines
			.map(_.toInt)
			.toArray
		var sum = 0
		var sumSet = Set(0)
		while (true) {
			for (freq <- freqs) {
				sum += freq
				if (sumSet(sum)) {
					println(sum)
					return
				} else sumSet += sum;
			}
		}
	}
}