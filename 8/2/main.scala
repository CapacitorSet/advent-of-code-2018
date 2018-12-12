import scala.io.Source

case class Node(nodes: Vector[Node], metadata: Vector[Int]) {
	def score: Int =
		if (nodes.isEmpty) metadata.sum
		else metadata
			// metadata is 1-indexed
			.map(_ - 1)
			// drop metadata that doesn't have a matching node
			.filter(nodes.indices contains)
    		.map(nodes(_).score)
    		.sum
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
		val out = root.score
		println(out)
	}
}