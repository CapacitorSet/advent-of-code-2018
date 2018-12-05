import scala.io.Source

object Application11 {
	def main(args: Array[String]): Unit = {
		val out = Source.fromFile("input").getLines
			.map(_.toInt)
			.sum
		println(out)
	}
}