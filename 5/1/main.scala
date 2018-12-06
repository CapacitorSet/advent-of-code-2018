import scala.annotation.tailrec
import scala.io.Source

object Application {
	@tailrec
	def score(input: String): Int = {
		val output = ('A' to 'Z').foldLeft(input)((str, ch) =>
			str
				.replace(s"$ch${ch.toLower}", "")
				.replace(s"${ch.toLower}$ch", "")
		)
		if (input.length == output.length)
			input.length
		else
			score(output)
	}
	def main(args: Array[String]): Unit = {
		val polymer = Source.fromFile("input").mkString.replace("\n", "")
		println(score(polymer))
	}
}