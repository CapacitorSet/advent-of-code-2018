import scala.io.Source

case class Node(nodes: Vector[Node], metadata: Vector[Int]) {
	def flatten: Vector[Node] = this.nodes.flatMap(_.flatten).+:(this)
}

object Application {
	def parse(input: Iterator[Int]): (Iterator[Int], Node) = {
		val numChildren = input.next
		val numMetadata = input.next
		val children: Vector[Node] = (1 to numChildren).scanLeft((input, Node(Vector[Node](), Vector[Int]()))){
			case ((iterator, _), _) => parse(iterator)
		}.drop(1).map(_._2).toVector
		val metadata = input.take(numMetadata)
		// Consume metadata
		for (_ <- 1 to numMetadata) input.next
		(input, Node(children, metadata.toVector))
	}
	def main(args: Array[String]): Unit = {
		val input: Array[Int] = Source.fromFile("input").getLines.toList.head.split(" ").map(_.toInt)
		val (_, root) = parse(input.iterator)
		val nodes = root.flatten
		val out = nodes.flatMap(_.metadata).sum
		println(out)
	}
}