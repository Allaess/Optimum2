import mw.model.{Company, Tribe}
import mw.view.GraphPane
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, ScrollPane}
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.stage.FileChooser

object Optimum extends JFXApp {
  var company = Company.empty
  var maxTribeSize = 7
  var undoStack = List.empty[Company]
  var redoStack = List.empty[Company]
  var loadScore = company.score
  var currentScore = 0
  var bestScore = 0
  var nextLink = Option.empty[(Tribe, Int, Tribe)]
  val graphPane = new GraphPane
  val loadButton = new Button {
    text = "Load"
    onAction = { _ =>
      val chooser = new FileChooser
      chooser.showOpenDialog(stage) match {
        case null =>
        case file =>
          undoStack = Nil
          redoStack = Nil
          company = Company(file)
          loadScore = company.score
          refresh()
      }
    }
  }
  val saveButton = new Button {
    text = "Save"
    onAction = { _ =>
      val chooser = new FileChooser
      chooser.showSaveDialog(stage) match {
        case null =>
        case file =>
          company.save(file)
      }
    }
  }
  val dropButton = new Button {
    text = "Drop Tribes"
    disable = true
    onAction = { _ =>
      undoStack ::= company
      company = company.explode
      redoStack = Nil
      refresh()
    }
  }
  val undoButton = new Button {
    text = "Undo"
    disable = true
    onAction = { _ =>
      redoStack ::= company
      company = undoStack.head
      undoStack = undoStack.tail
      refresh()
    }
  }
  val redoButton = new Button {
    text = "Redo"
    disable = true
    onAction = { _ =>
      undoStack ::= company
      company = redoStack.head
      redoStack = redoStack.tail
      refresh()
    }
  }
  val sizeField = new Label
  val statusField = new Label
  stage = new PrimaryStage {
    title = "Optimum"
    scene = new Scene {
      root = new BorderPane {
        top = new HBox {
          children = loadButton :: saveButton :: dropButton :: undoButton :: redoButton :: sizeField :: Nil
        }
        center = new ScrollPane {
          content = graphPane
        }
        bottom = new HBox {
          children = statusField :: Nil
        }
      }
    }
  }
  refresh()
  def refresh(): Unit = {
    graphPane.animate(company, maxTribeSize)
    currentScore = company.score
    bestScore = company.optimize.score
    sizeField.text = s"Max $maxTribeSize Squads per Tribe"
    statusField.text = nextLink match {
      case Some((tribe1, weight, tribe2)) =>
        s"Initial Score: $loadScore. Current Score: $currentScore. Optimized Score: $bestScore. $weight dependencies between ${tribe1.name} and ${tribe2.name}"
      case None =>
        s"Initial Score: $loadScore. Current Score: $currentScore. Optimized Score: $bestScore. No usable dependecy found"
    }
    dropButton.disable = !company.tribes.exists(_.size > 1)
    undoButton.disable = undoStack.isEmpty
    redoButton.disable = redoStack.isEmpty
  }
}
