import scala.io.Source

object Application {
	def main(args: Array[String]): Unit = {
		val out = Source.fromFile("input").getLines
			.map(_.toInt)
			.sum
		println(out)
	}
}