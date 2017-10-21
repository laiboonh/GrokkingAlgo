package example

object QuickSort extends App {

  def quickSort(list: List[Int]): List[Int] = list match {
    case Nil => List()
    case hd :: Nil => List(hd)
    case l@hd1 :: hd2 :: Nil => if (hd1 > hd2) hd2 :: hd1 :: Nil else l
    case l =>
      val pivot = l.head
      val (lessThan, greaterThan) = l.tail.foldLeft((List[Int](), List[Int]())) { (acc, item) =>
        if (item > pivot) (acc._1, acc._2 :+ item)
        else (acc._1 :+ item, acc._2)
      }
      (quickSort(lessThan) :+ pivot) ++ quickSort(greaterThan)
  }

  println(quickSort(List(1, 9, 3, 4, 5, 2, 8, 7, 0, 11, 4, 6, 5)))

}
