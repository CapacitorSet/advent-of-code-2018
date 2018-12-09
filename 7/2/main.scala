import scala.annotation.tailrec
import scala.io.Source

case class Requirement(prev: String, next: String)
case class Worker(node: String, minutesBusy: Int) {
	def idle() = node == "" && minutesBusy == 0
}

object Application {
	@tailrec
	def walk(timer: Int, workerPool: List[Worker], path: Set[String], conditionsMap: Map[String, List[String]]): Int = {
		val candidates: List[String] = conditionsMap
			// Nodes that can be reached
			.filter(_._2.forall(path contains)).keys
			// ... except those that are already handled by a worker
			.filterNot(node => workerPool.exists(_.node == node))
			// ... except those that we already visited.
			.filterNot(path contains)
			.toList.sorted
		val (newPool, _) = workerPool.foldLeft((List[Worker](), candidates)){
			case ((pool, candidates), worker) =>
				// The worker is busy, or no candidates are available.
				if (worker.minutesBusy != 0 || candidates.isEmpty)
					(pool :+ worker, candidates)
				else {
					val cand = candidates.head
					val duration = 60 + (cand.toCharArray.head.toInt - 64)
					(pool :+ Worker(cand, duration), candidates.drop(1))
				}
		}
		if (candidates.isEmpty && newPool.forall(_.idle))
			timer
		else {
			// Elapse time until a worker frees.
			val elapsed: Int = newPool.filter(!_.idle).map(_.minutesBusy).min
			val newTimer = timer + elapsed
			val newPath = path ++ newPool.filter(_.minutesBusy == elapsed).map(_.node)
			val elapsedPool = newPool.map(worker => {
				val newMins = worker.minutesBusy - elapsed
				// Do not elapse time for idle workers
				if (worker.node == "")
					worker
				// If the worker is done, unassign the node
				else if (newMins == 0)
					Worker("", 0)
				else
					Worker(worker.node, newMins)
			})
			walk(newTimer, elapsedPool, newPath, conditionsMap)
		}
	}
	def main(args: Array[String]): Unit = {
		val format = raw"Step (\w) must be finished before step (\w) can begin\.".r()
		val requirements: List[Requirement] = Source.fromFile("input").getLines
			.map { case format(prev, next) => Requirement(prev, next) }
			.toList
		// Map a point to the list of requirements that need to be satisfied
		val requirementsMap: Map[String, List[String]] = requirements.groupBy(_.next).mapValues(_.map(_.prev))
		// Root points have a dummy ("") condition. Dirty hack, but eh
		val dummyMap = requirements.map(_.prev).distinct.filterNot(requirementsMap contains).map((_, List(""))).toMap
		val workerPool = List(Worker("", 0), Worker("", 0), Worker("", 0), Worker("", 0), Worker("", 0))
		val out = walk(0, workerPool, Set(""), requirementsMap ++ dummyMap)
		println(out)
	}
}