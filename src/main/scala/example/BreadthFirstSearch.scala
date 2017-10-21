package example

import scala.collection.immutable.Map

object BreadthFirstSearch extends App {

  case class Person(name: String, isMangoSeller: Boolean = false)

  def breadthFirstSearch(
    map: Map[Person, List[Person]],
    queue: List[Person],
    checked: List[Person]): Option[Person] = queue match {
    case Nil => None
    case hd :: tl =>
      if (checked.contains(hd))
        breadthFirstSearch(map, tl, checked)
      else if (hd.isMangoSeller) Some(hd) else breadthFirstSearch(map, tl ++ map(hd), checked :+ hd)
  }

  val relationships = Map(
    Person("YOU") -> List(Person("ALICE"), Person("BOB"), Person("CLAIRE"))
    , Person("ALICE") -> List(Person("PEGGY"))
    , Person("BOB") -> List(Person("PEGGY"), Person("ANUJ", true))
    , Person("CLAIRE") -> List(Person("JONNY"), Person("THOM"))
    , Person("PEGGY") -> List()
    , Person("ANUJ") -> List()
    , Person("THOM") -> List()
    , Person("JONNY") -> List()
  )

  println(breadthFirstSearch(relationships, relationships(Person("YOU")), List()))

}
