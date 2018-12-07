import scala.annotation.tailrec
import scala.collection.parallel.ParIterable
import scala.io.Source

object Application {
	@tailrec
	def walk(path: List[String], conditionsMap: Map[String, List[String]]): List[String] = {
		val candidates: Iterable[String] = conditionsMap
			// Nodes that can be reached
			.filter(_._2.forall(path contains))
			// Extract the actual nodes
			.keys
			// ... except those that we already visited.
			.filterNot(path contains _)
		if (candidates.isEmpty)
			path
		else
			walk(path :+ candidates.min, conditionsMap)
	}
	def main(args: Array[String]): Unit = {
		val format = raw"Step (\w) must be finished before step (\w) can begin\.".r()
		val conditions: List[(String, String)] = Source.fromFile("input").getLines
			.map { case format(prev, next) => (prev, next) }
    		.toList
		// Map a point to the list of conditions that need to be satisfied
		val conditionsMap: Map[String, List[String]] = conditions.groupBy(_._2).mapValues(_.map(_._1))
		// Root points have a dummy ("") condition. Dirty hack, but eh
		val dummyMap = conditions.map(_._1).distinct.filterNot(conditionsMap contains).map((_, List(""))).toMap
		val out = walk(List(""), conditionsMap ++ dummyMap).mkString
		println(out)
	}
}