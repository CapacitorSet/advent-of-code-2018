import scala.annotation.tailrec
import scala.io.Source

object Application {
	@tailrec
	def score(input: String): Int = {
		val output = ('A' to 'Z').foldLeft(input)((str, ch) =>
			str
				.replace(new StringBuilder().append(ch).append(ch.toLower), "")
				.replace(new StringBuilder().append(ch.toLower).append(ch), "")
		)
		if (input.length == output.length)
			input.length
		else
			score(output)
	}
	def main(args: Array[String]): Unit = {
		val polymer = Source.fromFile("input").mkString.replace("\n", "")
		val out = ('A' to 'Z').map(ch => score(
			polymer
				.replace(String.valueOf(ch), "")
				.replace(String.valueOf(ch.toLower), "")
		)).min
		println(out)
	}
}