package mw.view

import scalafx.animation.FillTransition

trait RecolorAnimation extends FillTransition {
  duration = GraphPane.animationDuration
}
object RecolorAnimation {
  def apply(fromBubble: Bubble, toBubble: Bubble): RecolorAnimation = new RecolorAnimation {
    shape = fromBubble.circle
    fromValue = fromBubble.color
    toValue = toBubble.color
  }
}
