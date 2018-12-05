package main

import scala.io.Source
import scala.util.matching.Regex

sealed trait Action

case object BEGIN extends Action

case object ASLEEP extends Action

case object WAKEUP extends Action

case class FileEntry(id: Int, minute: Int, action: Action)

object Application41 {
  val formatBegin:Regex = raw"\[....-(\d\d)-(\d\d) \d\d:\d\d\] Guard #(\d+) begins shift".r()
  val formatAsleep: Regex = raw"\[....-(\d\d)-(\d\d) 00:(\d\d)\] falls asleep".r()
  val formatWakeup:Regex = raw"\[....-(\d\d)-(\d\d) 00:(\d\d)\] wakes up".r()

  def parse_event(event: String, providedGuardID: Int): FileEntry =
    event match {
      case formatBegin(month, day, guardID) =>
        FileEntry(guardID.toInt, 0, BEGIN)

      case formatAsleep(month, day, minute) =>
        FileEntry(providedGuardID, minute.toInt, ASLEEP)
      case formatWakeup(month, day, minute) =>
        FileEntry(providedGuardID, minute.toInt, WAKEUP)
    }

  def parse_events(inputEvents: List[String], outputEvents:List[FileEntry]=List(), guardID:Int=0):List[FileEntry] = {
      inputEvents match {
        case head::tail=> {
          val event=parse_event(head,guardID)
          parse_events(tail,event::outputEvents,event.id)
        }
        case Nil => outputEvents.reverse
      }
  }

  def main(args: Array[String]): Unit = {
    // The events are in random chronological order, but a simple sort will fix that
    val inputEvents = Source.fromFile(args(0)).getLines.toList.sorted
    val normalizedEvents = parse_events(inputEvents)
    val guardData: Map[Int, Iterator[(Int, Int)]] = normalizedEvents
      // Map guard IDs to the respective asleep/wakeup events
      .filter(_.action != BEGIN).groupBy(_.id)
      // Extract (asleep, wakeup) pairs
      .mapValues(
      _.sliding(2, 2).map { case List(asleep, wakeup) => (asleep.minute, wakeup.minute) }
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