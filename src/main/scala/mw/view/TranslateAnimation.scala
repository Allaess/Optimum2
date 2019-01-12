package mw.view

import scalafx.animation.TranslateTransition

trait TranslateAnimation extends TranslateTransition {
  duration = GraphPane.animationDuration
}
object TranslateAnimation {
  def apply(oldBubble: Bubble, newBubble: Bubble): TranslateAnimation = new TranslateAnimation {
    node = oldBubble
    byX = newBubble.centerX - oldBubble.centerX
    byY = newBubble.centerY - oldBubble.centerY
  }
}
