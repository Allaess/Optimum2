package mw.view

import scalafx.animation.FadeTransition

object DeleteAnimation {
  def apply(bubble: Bubble) = new FadeTransition {
    node = bubble
    duration = GraphPane.animationDuration
    fromValue = 1
    toValue = 0
  }
}
