import scala.io.Source

object Application {
	def main(args: Array[String]): Unit = {
		val formatBegin = raw"\[....-\d\d-\d\d \d\d:\d\d\] Guard #(\d+) begins shift".r()
		val formatAsleep = raw"\[....-\d\d-\d\d 00:(\d\d)\] falls asleep".r()
		val formatWakeup = raw"\[....-\d\d-\d\d 00:(\d\d)\] wakes up".r()
		// The events are in random chronological order, but a simple sort will fix that
		val inputEvents = Source.fromFile("input").getLines.toList.sorted
		var currentGuardId = 0
		val BEGIN = 0
		val ASLEEP = 1
		val WAKEUP = 2
		val normalizedEvents = for (event <- inputEvents)
			yield event match {
				case formatBegin(guardId) =>
					currentGuardId = guardId.toInt
					(currentGuardId, 0, BEGIN)
				case formatAsleep(minute) =>
					(currentGuardId, minute.toInt, ASLEEP)
				case formatWakeup(minute) =>
					(currentGuardId, minute.toInt, WAKEUP)
			}
		val guardData: Map[Int, Iterator[(Int, Int)]] = normalizedEvents
			// Map guard IDs to the respective asleep/wakeup events
			.filter(_._3 != BEGIN).groupBy(_._1)
			// Extract (asleep, wakeup) pairs
    		.mapValues(
				_.sliding(2, 2).map { case List((_, tsAsleep, _), (_, tsWakeup, _)) => (tsAsleep, tsWakeup) }
			)
		val freqPairs = for (minute <- 0 to 59;
			 guard <- guardData.keys)
			yield (minute, guard)
		val out = freqPairs
			.maxBy{ case (minute, guard) =>
				guardData(guard).count {
					case (tsAsleep, tsWakeup) => (tsAsleep until tsWakeup) contains minute
				}
			}

		println(out._1 * out._2)
	}
}