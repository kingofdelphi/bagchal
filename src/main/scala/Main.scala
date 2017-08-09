package bagchal

import javafx.beans.{InvalidationListener, Observable}
import javax.swing.event.{ChangeEvent, ChangeListener}

import utils.Utils.Point

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalafx.Includes._
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.collections.{ObservableArray, ObservableBuffer}
import scalafx.geometry.Insets
import scalafx.scene.canvas.Canvas
import scalafx.scene.control._
import scalafx.scene.image.ImageView
import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import scalafx.scene.layout.{BorderPane, HBox, VBox}
import scalafx.scene.{Group, Scene}
import scalafx.stage.Stage

/**
 * Example adapted from code showed in [[http://docs.oracle.com/javafx/2/canvas/jfxpub-canvas.htm]].
 */

object Main extends JFXApp {
  val canvas = new Canvas(420, 420)
  canvas.focusTraversable = true
  canvas.onMouseClicked = (e : MouseEvent) => canvas.requestFocus()

  var winner = BagChalGame.None

  val Finished = 0
  val Running = 1
  val Thinking = 2
  val Paused = 3
  val Scoring = 4

  var gameStatus = Finished

  val gameStatusLabel = new Label("Game Status")

  val thinkingStatus = new Label("")
  val ai1 = new ToggleGroup()
  val ai2 = new ToggleGroup()
  val first_play = new ToggleGroup()
  val turnImage = new ImageView()
  val tiger_check_box = new CheckBox("Show Tiger Move Score")
  val goat_check_box = new CheckBox("Show Goat Move Score")
  val score_label = new Label("")

  var gameHistory : Seq[BagChalGame] = Seq()

  def getUI = {
    //Radio Button Toggle Group
    val g1 = new VBox
    val g2 = new VBox
    val zip = Seq(g1, g2) zip Seq(ai1, ai2) zip Seq("Tiger", "Goat")

    zip.foreach(x => {
      val rb1 = new RadioButton {
        text = "Human"
        toggleGroup = x._1._2
      }
      rb1.setUserData("human")
      val rb2 = new RadioButton {
        text = "Computer"
        selected = true
        toggleGroup = x._1._2
      }
      rb2.setUserData("computer")
      x._1._1.children = List(
        new Label(x._2), rb1, rb2
      )
    })

    val button1 = new Button("Runtime Set")
    val button2 = new Button("Reset")
    val button3 = new Button("Toggle")
    val button4 = new Button("History")
    var button_list = new VBox

    button_list.children = List(button1, button2, button3, button4).map(x => {
      x.setPrefWidth(120)
      x
    })

    button1.onMouseClicked = (e : MouseEvent) => {
      loadGameFromChoice(true)
    }

    button2.onMouseClicked = (e : MouseEvent) => {
      loadGameFromChoice(false)
    }

    button3.onMouseClicked = (e : MouseEvent) => gameLock.synchronized {
      gameStatus = (if (gameStatus == Paused) Running else Paused)
      if (gameStatus == Paused) {
        renderer.entities.foreach(_.pause)
      } else {
        renderer.entities.foreach(_.resume)
      }
    }

    button4.onMouseClicked = (e : MouseEvent) => {
      val canvas = new Canvas(420, 420)
      val renderer = new Renderer(canvas)
      // Create dialog

      val listbox = new ListView[String]
      val gameList = gameHistory.map(x => x)
      listbox.items = ObservableBuffer((1 to gameList.size).map(_.toString).reverse)

      val refresh = () => {
        val id = listbox.selectionModel.value.getSelectedItem.toInt - 1
        renderer.setGame(gameList.zipWithIndex.filter(_._2 == id).head._1)
        renderer.render
      }

      val listbtn = new Button {
        text = "Load Selected State"
        onAction = handle {
          val id = listbox.selectionModel.value.getSelectedItem.toInt - 1
          val (t, g) = getChoice
          val ns = gameList.zipWithIndex.filter(_._2 == id).head._1.clone()
          gameHistory = gameList.zipWithIndex.filter(_._2 <= id).map(_._1)
          loadGame(ns, t, g)
        }
      }

      listbox.onMouseClicked = (e : MouseEvent) => {
        refresh()
      }

      listbox.onKeyReleased = (e : KeyEvent) => {
        refresh()
      }

      listbox.selectionModel.value.selectFirst()
      refresh()

      val lbox = new HBox {
        children = List(canvas, listbox)
      }

      val dialogStage = new Stage {
        outer =>
        title = "Move History"
        scene = new Scene {
          root = new BorderPane {
            padding = Insets(25)
            center = new VBox {
              children = List(lbox, new HBox {
                children = List(listbtn , new Button {
                  text = "Close"
                  onAction = handle {
                    outer.close()
                  }
                })
              })
            }
          }
        }
      }

      // Show dialog and wait till it is closed
      dialogStage.showAndWait()
    }

    val tiger = new RadioButton {
      text = "Tiger"
      toggleGroup = first_play
    }

    tiger.setUserData("tiger")

    val goat = new RadioButton {
      text = "Goat"
      selected = true
      toggleGroup = first_play
    }
    goat.setUserData("goat")

    val score_box = new VBox
    score_box.children = List(tiger_check_box, goat_check_box, score_label)

    val ch = new HBox
    ch.children.addAll(tiger, goat)

    val box = new VBox
    box.children = List(g1, g2, new Label("First player"), ch, button_list, score_box, gameStatusLabel, thinkingStatus, turnImage)

    val layout = new HBox

    val eater = new Group(layout) {
      filterEvent(KeyEvent.KeyPressed) {
        (e: KeyEvent) => {
          keypress = Some(e.code)
          e.consume()
        }
      }
    }

    layout.children = List(canvas, box)

    val rootPane = new Group
    rootPane.children = List(eater)
    rootPane
  }

  val scene1 = new Scene {
    root = getUI
  }

  stage = new PrimaryStage {
    title = "BagChal Game"
    scene = scene1
    width = canvas.width.value + 220
    height = canvas.height.value
  }

  val gc = canvas.graphicsContext2D

  var game : BagChalGame = null

  var selbox = (0, 0)
  var first_sel = (-1, -1)
  var keypress : Option[KeyCode] = None
  var changeturn = 1
  var canselect = true

  def handleKeyEvent(code : KeyCode) = {

    val (dx : Int, dy : Int) = code match {
      case KeyCode.Left =>
        (-1, 0)
      case KeyCode.Right =>
        (1, 0)
      case KeyCode.Up =>
        (0, -1)
      case KeyCode.Down =>
        (0, 1)
      case KeyCode.Space =>
        if (!canselect) None
        else if (first_sel == selbox)
          first_sel = (-1, -1)
        else {
          val curbox = game.state.matrix(selbox._2)(selbox._1)
          game.turn match {
            case BagChalGame.Tiger =>
              //not previously selected
              if (first_sel._1 == -1) {
                if (curbox == BagChalGame.Tiger) {
                  first_sel = selbox
                }
              } else {
                if (curbox == BagChalGame.None) {
                  val r = game.handleTigerMovement(first_sel, selbox)
                  if (r._1) {
                    canselect = false
                    executeTigerMove((first_sel, selbox, if (r._2) 1 else 0))
                    first_sel = (-1, -1)
                  }
                }
              }

            case BagChalGame.Goat =>
              if (game.goats_to_insert > 0) {
                if (curbox == BagChalGame.None) {
                  canselect = false
                  executeGoatMove((selbox, selbox))
                }
              } else {
                if (first_sel._1 == -1) {
                  if (curbox == BagChalGame.Goat) {
                    first_sel = selbox
                  }
                } else {
                  if (curbox == BagChalGame.None) {
                    if (game.handleGoatMovement(first_sel, selbox)) {
                      canselect = false
                      executeGoatMove((first_sel, selbox))
                      first_sel = (-1, -1)
                    }
                  }
                }
              }
            case _ =>
          }
        }
        (0, 0)
      case key =>
        (0, 0)
    }
    selbox = (
      Math.max(0, Math.min(selbox._1 + dx, game.size - 1)),
      Math.max(0, Math.min(selbox._2 + dy, game.size - 1))
    )
  }

  def executeTigerMove(move : ((Int, Int), (Int, Int), Int), score : Option[Double] = None) = {
    if (move._3 == 1) {
      //remove goat
      val mid = BagChalGame.getMidPoint(move._1, move._2)
      renderer.entities.filter(x => !x.destroyed && x.data.asInstanceOf[(Int, Int)] == mid).foreach(x => x.destroy())
    }

    val p = renderer.entities.partition(x => !x.destroyed && x.data.asInstanceOf[(Int, Int)] == move._1)
    renderer.entities = p._2 :+ new Tiger(p._1.head.src, renderer.getPos(move._2), move._2)
    game.executeTigerMove(move)
    gameHistory = gameHistory :+ game.clone()

    if (game.getPossibleGoatMoves.isEmpty) {
      gameStatus = Finished
      winner = BagChalGame.Tiger
    } else {
      if (tiger_check_box.selected.value) {
        if (score.isDefined) {
          lastTigerScore = score.get
          changeturn = 1
        } else {
          gameStatus = Scoring
          val mv = Future(game.clone().getBestGoatMove.map(-_._2))
          mv.map(x => {
            lastTigerScore = x.get
            gameStatus = Running
            changeturn = 1
          })
        }
      } else {
        changeturn = 1
      }
    }

  }

  def executeGoatMove(move : ((Int, Int), (Int, Int)), score : Option[Double] = None) = {
    if (move._1 == move._2) {
      val goat = new Goat(Point(), renderer.getPos(move._1), move._1)
      goat.moveTo(goat.dest)
      goat.setEntering
      renderer.entities = renderer.entities :+ goat
    } else {
      val p = renderer.entities.partition(x => !x.destroyed && x.data.asInstanceOf[(Int, Int)] == move._1)
      renderer.entities = p._2 :+ new Goat(p._1.head.src, renderer.getPos(move._2), move._2)
    }
    game.executeGoatMove(move)
    gameHistory = gameHistory :+ game.clone()

    if (game.getPossibleTigerMoves.isEmpty) {
      gameStatus = Finished
      winner = BagChalGame.Goat
    } else {
      if (goat_check_box.selected.value) {
        if (score.isDefined) {
          lastGoatScore = score.get
          changeturn = 1
        } else {
          gameStatus = Scoring
          val mv = Future(game.clone().getBestTigerMove.map(-_._2))
          mv.map(x => {
            lastGoatScore = x.get
            gameStatus = Running
            changeturn = 1
          })
        }
      } else {
        changeturn = 1
      }
    }


  }

  def isTransitioning = {
    renderer.entities.exists(x => {
      !x.destroyed && x.isTransitioning
    })
  }

  case object gameLock

  var lastGoatScore : Double = 0
  var lastTigerScore : Double = 0
  var renderer : Renderer = new Renderer(canvas)
  var thinking : Int = BagChalGame.None

  def render {
    //render
    renderer.render
    if (game.turn == BagChalGame.Tiger) turnImage.setImage(Renderer.tigerImage) else turnImage.setImage(Renderer.goatImage)

    if (game.turn != game.player_goat && game.turn != game.player_tiger) {
      val sbox = new SelBox(renderer.getPos(selbox), Point(), 0.1)
      renderer.drawEntity(sbox)
      if (first_sel._1 != -1) {
        val sbox = new SelBox(renderer.getPos(first_sel), Point(), 0.3)
        renderer.drawEntity(sbox)
      }
    }

    if (thinking == BagChalGame.Tiger) thinkingStatus.text = "Tiger is thinking"
    else if (thinking == BagChalGame.Goat) thinkingStatus.text = "Goat is thinking"
    else thinkingStatus.text = ""

    //get events
    keypress.map(x => {
      handleKeyEvent(x)
      keypress = None
    })

    if (gameStatus == Running) {
      if (!isTransitioning) {
        if (changeturn == 1) {
          val turn = game.changeTurn()
          changeturn = 0
          canselect = false
          if (turn != game.player_goat && turn != game.player_tiger) {
            canselect = true
          } else if (turn == BagChalGame.Tiger) {
            thinking = BagChalGame.Tiger
            //donot use game, clone it instead, multiple threads one writing and one reading is causing inconsistency
            Future(game.clone().getBestTigerMove).map(move => {
              if (!move.isDefined) {
                gameStatus = Finished
                winner = BagChalGame.Goat
              } else {
                thinking = 0
                executeTigerMove(move.get._1, Some(move.get._2))
              }
            })
          } else {
            thinking = BagChalGame.Goat
            Future(game.clone().getBestGoatMove).map(move => {
              if (!move.isDefined) {
                gameStatus = Finished
                winner = BagChalGame.Tiger
              } else {
                thinking = 0
                executeGoatMove(move.get._1, Some(move.get._2))
              }
            })
          }
        }
      }
    }
    //update entity positions
    if (gameStatus != Paused) {
      renderer.entities.foreach(x => x.upd(dt))
    }

    if (gameStatus == Running) {
      val turn = if (game.turn == BagChalGame.Goat) "Goat" else "Tiger"
      gameStatusLabel.text = s"Game Status: $turn's turn"
    } else if (gameStatus == Finished) {
      val win = if (winner == BagChalGame.Goat) "Goat" else "Tiger"
      gameStatusLabel.text = s"Game Status: $win wins"
    } else if (gameStatus == Paused) {
      gameStatusLabel.text = "Game Status: Paused"
    } else if (gameStatus == Scoring) {
      gameStatusLabel.text = "Game Status: Calculating score for last move"
    }

    var scoring : Seq[String] = Seq()
    if (tiger_check_box.selected.value) {
      scoring = scoring :+ s"Tigers score: $lastTigerScore"
    }
    if (goat_check_box.selected.value) {
      scoring = scoring :+ s"Goats score: $lastGoatScore"
    }
    score_label.text = scoring.mkString("\n")

  }

  var frameStart : Long = 0
  var dt : Double = 0

  val timer = new AnimationTimer(t => {
    dt = (t - frameStart) / 1e9
    frameStart = t
    gameLock.synchronized {
      render
    }
  }) {

  }

  def getChoice = {
    val t = ai1.selectedToggle.value.getUserData.asInstanceOf[String] == "computer"
    val g = ai2.selectedToggle.value.getUserData.asInstanceOf[String] == "computer"
    (t, g)
  }

  def loadGameFromChoice(continueOld : Boolean = false) = {
    val (t, g) = getChoice
    val gm = if (continueOld) game else new BagChalGame(5)
    if (!continueOld) {
      gameHistory = gm.clone() :: Nil
    }
    loadGame(gm, t, g)
  }

  def loadGame(ng : BagChalGame, tigerAI : Boolean, goatAI : Boolean) = gameLock.synchronized {
    keypress = None
    game = ng
    game.setTiger(tigerAI)
    game.setGoat(goatAI)
    val tigerfirst = first_play.selectedToggle.value.getUserData().asInstanceOf[String] == "tiger"
    if (tigerfirst) {
      turnImage.setImage(Renderer.tigerImage)
    } else {
      turnImage.setImage(Renderer.goatImage)
    }
    val notfirst = if (tigerfirst) BagChalGame.Goat else BagChalGame.Tiger

    game.setTurn(notfirst)

    changeturn = 1
    renderer.setGame(game)
    gameStatus = Running
    gameStatusLabel.text = ""
  }

  loadGameFromChoice(false)
  timer.start()

}
