package mw.view

import scalafx.animation.ParallelTransition

object UpdateAnimation {
  def apply(fromBubble: Bubble, toBubble: Bubble) = if (fromBubble.center != toBubble.center) {
    val translation = TranslateAnimation(fromBubble, toBubble)
    if (fromBubble.color != toBubble.color) {
      val recolor = RecolorAnimation(fromBubble, toBubble)
      Some(new ParallelTransition {
        children = translation :: recolor :: Nil
      })
    } else {
      Some(translation)
    }
  } else {
    if (fromBubble.color != toBubble.color) {
      Some(RecolorAnimation(fromBubble, toBubble))
    } else {
      None
    }
  }
}
