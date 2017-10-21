package example

import scala.collection.immutable.Map

object Dijkstra extends App {
  def dijkstra(
    graph: Map[String, Map[String, Int]]
    , costs: Map[String, Node]
  ): Map[String, Node] =
    nextLowestCostNodeNotProcessed(costs) match {
      case None => costs
      case Some(node) =>
        val nodeCost: Int = costs(node.name).cost
        val neighbours: Map[String, Int] = graph(node.name)
        val updatedCost = costs + (node.name -> node.copy(processed = true))
        val finalCosts = neighbours.foldLeft(updatedCost) { (acc, item) =>
          val currentItemCost: Int = acc(item._1).cost
          val newItemCost: Int = item._2 + nodeCost
          if (currentItemCost < newItemCost) acc else acc + (item._1 -> Node(item._1, newItemCost, processed = false, node.name))
        }
        dijkstra(graph, finalCosts)
    }


  def nextLowestCostNodeNotProcessed(costs: Map[String, Node]): Option[Node] = {
    costs.values.foldLeft(None: Option[Node]) {
      case (None, item@Node(_, _, false, _)) => Some(item)
      case (some, Node(_, _, true, _)) => some
      case (Some(Node(_, cost1, false, _)), item@Node(_, cost2, false, _)) if cost1 > cost2 => Some(item)
      case (Some(item@Node(_, cost1, false, _)), Node(_, cost2, false, _)) if cost1 <= cost2 => Some(item)
      case (acc, _) => acc
    }
  }

  val graph: Map[String, Map[String, Int]] = Map(
    "START" -> Map(
      "A" -> 6
      , "B" -> 2
    )
    , "A" -> Map(
      "FIN" -> 1
    )
    , "B" -> Map(
      "A" -> 3,
      "FIN" -> 5
    )
    , "FIN" -> Map()
  )

  case class Node(name: String, cost: Int, processed: Boolean = false, parent: String = "")

  val costs: Map[String, Node] = Map(
    "START" -> Node("START", 0)
    , "A" -> Node("A", Int.MaxValue)
    , "B" -> Node("B", Int.MaxValue)
    , "FIN" -> Node("FIN", Int.MaxValue)
  )

  def shortestPath(costs: Map[String, Node], acc: List[String], nodeName: String): List[String] = nodeName match {
    case "START" => "START" +: acc
    case nodeName => shortestPath(costs, nodeName +: acc, costs(nodeName).parent)
  }

  val result = dijkstra(graph, costs)
  println(result)
  println(shortestPath(result, List(), "FIN").mkString("->"))


}
