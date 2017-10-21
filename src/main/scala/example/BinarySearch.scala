package example

object BinarySearch extends App {
  val target = 10
  val list = (1 to 10).toList

  def binarySearch(list: List[Int], target: Int): Option[Int] = {
    def go(indexedList: List[(Int, Int)]): Option[Int] = indexedList match {
      case l if l.isEmpty => None
      case l if l.size == 2 =>
        if (l.head._1 == target) Some(l.head._2)
        else if (l.last._1 == target) Some(l.last._2)
        else None
      case l =>
        val high = l.length - 1
        val low = 0
        val midIndex = (low + high) / 2
        if (l(midIndex)._1 < target)
          go(l.slice(midIndex, high + 1))
        else if (l(midIndex)._1 > target)
          go(l.slice(0, midIndex))
        else
          Some(l(midIndex)._2)

    }

    go(list.zipWithIndex)
  }

  println(binarySearch(List(1,2,3,4,5,6,7,8,9,10,11), 11))
}
