import scala.io.Source

sealed trait Action
case object BEGIN extends Action
case object ASLEEP extends Action
case object WAKEUP extends Action

case class FileEntry(id:Int,minute:Int, action:Action)

object Application41{
	def main(args: Array[String]): Unit = {
		val formatBegin = raw"\[....-(\d\d)-(\d\d) \d\d:\d\d\] Guard #(\d+) begins shift".r()
		val formatAsleep = raw"\[....-(\d\d)-(\d\d) 00:(\d\d)\] falls asleep".r()
		val formatWakeup = raw"\[....-(\d\d)-(\d\d) 00:(\d\d)\] wakes up".r()
		// The events are in random chronological order, but a simple sort will fix that
		val inputEvents = Source.fromFile(args(0)).getLines.toList.sorted
		var currentGuardId = 0
		val normalizedEvents = for (event <- inputEvents)
			yield event match {
				case formatBegin(month, day, guardId) => {
					currentGuardId = guardId.toInt
					FileEntry(currentGuardId,0, BEGIN)
				}
				case formatAsleep(month, day, minute) =>
					FileEntry(currentGuardId, minute.toInt, ASLEEP)
				case formatWakeup(month, day, minute) =>
					FileEntry(currentGuardId, minute.toInt, WAKEUP)
			}
		val guardData: Map[Int, Iterator[(Int, Int)]] = normalizedEvents
			// Map guard IDs to the respective asleep/wakeup events
			.filter(_.action != BEGIN).groupBy(_.id)
			// Extract (asleep, wakeup) pairs
    		.mapValues(
				_.sliding(2, 2).map { case List(asleep,wakeup) => (asleep.minute, wakeup.minute) }
			)
		val guardMostAsleep = guardData
			.mapValues {
				// How long was the guard asleep?
				_.map { case (tsAsleep, tsWakeup) => tsWakeup - tsAsleep }
				.sum
			}
    		.maxBy(_._2)._1
		val thisGuardPairs = guardData(guardMostAsleep).toList
		val bestMinute = (0 to 59)
			.maxBy(minute => thisGuardPairs
				.count { case (tsAsleep, tsWakeup) => (tsAsleep until tsWakeup) contains minute }
			)

		println(guardMostAsleep * bestMinute)
	}
}