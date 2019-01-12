package mw.view

import scalafx.animation.FadeTransition

object CreateAnimation {
  def apply(bubble: Bubble) = new FadeTransition {
    node = bubble
    duration = GraphPane.animationDuration
    fromValue = 0
    toValue = 1
  }
}
