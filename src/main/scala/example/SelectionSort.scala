package example

import scala.collection.immutable.ListMap

object SelectionSort extends App {

  def selectionSort(map: Map[String, Int], acc: Map[String, Int]): Map[String, Int] = map match {
    case m if m.isEmpty => acc
    case m =>
      val maxValue = m.values.max
      val maxValueEntries = m.filter(entry => entry._2 == maxValue)
      val otherEntries = m.filterNot(entry => entry._2 == maxValue)
      selectionSort(otherEntries, acc ++ maxValueEntries)
  }

  val musicPlayCount = Map(
    "RADIOHEAD" -> 156,
    "KISHORE KUMAR" -> 141,
    "THE BLACK KEYS" -> 35,
    "NEUTRAL MILK HOTEL" -> 94,
    "BECK" -> 88,
    "THE STROKES" -> 61,
    "WILCO" -> 111
  )
  println(selectionSort(musicPlayCount, ListMap.empty))
}
