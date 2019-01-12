package mw.view

import mw.model.{Company, Tribe}
import scalafx.animation.ParallelTransition
import scalafx.scene.layout.Pane
import scalafx.util.Duration

class GraphPane extends Pane {
  outer =>
  var graph = Graph.empty
  var bubbles = List.empty[Bubble]
  var links = List.empty[Link]
  def animate(newCompany: Company, maxTribeSize: Int, positions: Map[Tribe, Point] = Map.empty) = {
    val oldBubbles = (for (bubble <- bubbles) yield bubble.label -> bubble).toMap
    val newGraph = graph.to(newCompany, positions)
    val newBubbles = (for (bubble <- bubbles(newGraph, maxTribeSize)) yield bubble.label -> bubble).toMap
    val bubbleLabels = oldBubbles.keySet ++ newBubbles.keySet
    val bubbleAnimations = for (label <- bubbleLabels) yield
      bubbleAnimation(oldBubbles.get(label), newBubbles.get(label))
    graph = newGraph
    bubbles = newBubbles.values.toList
    children = for (label <- bubbleLabels) yield oldBubbles.getOrElse(label, newBubbles(label))
    val animation = new ParallelTransition {
      children = bubbleAnimations.collect {
        case Some(anim) => anim
      }
      onFinished = { _ =>
        outer.children = bubbles
      }
    }
    animation.play()
  }
  def bubbles(graph: Graph, maxTribeSize: Int) = {
    val company = graph.company
    val tribeBubbles = for (tribe <- company if tribe.size > 1) yield
      Bubble(tribe, graph.position(tribe), tribe.size < maxTribeSize)
    val squadBubbles = for {
      tribe <- company
      squad <- tribe
    } yield Bubble(tribe, squad, graph.position(squad), tribe.size < maxTribeSize)
    tribeBubbles ++ squadBubbles
  }
  def links(graph: Graph, maxTribeSize: Int, bubble: Map[String, Bubble]) = {
    val company = graph.company
    for {
      tribe <- company if tribe.size > 1
      squad <- tribe
    } yield {
      val from = bubble(tribe.name)
      val to = bubble(squad.name)
      Link(from, to)
    }
  }
  def bubbleAnimation(oldBubble: Option[Bubble], newBubble: Option[Bubble]) =
    (oldBubble, newBubble) match {
      case (Some(fromBubble), Some(toBubble)) => UpdateAnimation(fromBubble, toBubble)
      case (Some(fromBubble), None) => Some(DeleteAnimation(fromBubble))
      case (None, Some(toBubble)) => Some(CreateAnimation(toBubble))
      case (None, None) => None
    }
}
object GraphPane {
  val animationDuration = Duration(5000)
}
