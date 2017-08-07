package bagchal

import utils.Utils.Point

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalafx.Includes._
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{Button, Label, RadioButton, ToggleGroup}
import scalafx.scene.image.ImageView
import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.{Group, Scene}

/**
 * Example adapted from code showed in [[http://docs.oracle.com/javafx/2/canvas/jfxpub-canvas.htm]].
 */
object Main extends JFXApp {
  val canvas = new Canvas(420, 420)
  canvas.focusTraversable = true
  canvas.onMouseClicked = (e : MouseEvent) => canvas.requestFocus()
  val gameStatus = new Label("Game Status")
  val thinkingStatus = new Label("")
  val ai1 = new ToggleGroup()
  val ai2 = new ToggleGroup()
  val first_play = new ToggleGroup()
  val turnImage = new ImageView()

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
    var button_list = new HBox
    button_list.children = List(button1, button2, button3)

    button1.onMouseClicked = (e : MouseEvent) => {
      loadGameFromChoice(true)
    }

    button2.onMouseClicked = (e : MouseEvent) => {
      loadGameFromChoice(false)
    }

    button3.onMouseClicked = (e : MouseEvent) => gameLock.synchronized {
      gameRunning = !gameRunning
      val turn = if (game.turn == BagChalGame.Goat) "goat" else "tiger"
      val sc = s"\nLast Goat score $lastGoatScore" +
        s"\nLast Tiger score $lastTigerScore"
      gameStatus.text = s"Game Status: $sc" + (if (gameRunning) "" else ", Paused")

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

    val ch = new HBox
    ch.children.addAll(tiger, goat)

    val box = new VBox
    box.children = List(g1, g2, new Label("First player"), ch, button_list, gameStatus, thinkingStatus, turnImage)

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
                    executeTigerMove((first_sel, selbox, if (r._2) 1 else 0))
                    first_sel = (-1, -1)
                  }
                }
              }

            case BagChalGame.Goat =>
              if (game.goats_to_insert > 0) {
                if (curbox == BagChalGame.None) {
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
    changeturn = 1
    canselect = false
    lastTigerScore = score.getOrElse(
      game.getBestGoatMove.map(-_._2).getOrElse(1000)
    )

    if (game.getPossibleGoatMoves.isEmpty) {
      gameRunning = false
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
    changeturn = 1
    canselect = false
    lastGoatScore = score.getOrElse(
      game.getBestTigerMove.map(-_._2).getOrElse(1000)
    )

    if (game.getPossibleTigerMoves.isEmpty) {
      gameRunning = false
    }

  }

  def isTransitioning = {
    renderer.entities.exists(x => {
      !x.destroyed && x.isTransitioning
    })
  }

  var gameRunning = false
  case object gameLock

  var lastGoatScore : Double = 0
  var lastTigerScore : Double = 0
  var renderer : Renderer = new Renderer(canvas)
  var thinking : Int = BagChalGame.None

  def render {
    if (gameRunning) {
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

      if (!isTransitioning) {
        if (changeturn == 1) {
          val turn = game.changeTurn()
          changeturn = 0
          if (turn != game.player_goat && turn != game.player_tiger) {
            canselect = true
          } else if (turn == BagChalGame.Tiger) {
            thinking = BagChalGame.Tiger
            //donot use game, clone it instead, multiple threads one writing and one reading is causing inconsistency
            Future(game.clone().getBestTigerMove).map(move => {
              if (!move.isDefined) {
                gameRunning = false
              } else {
                thinking = 0
                executeTigerMove(move.get._1, Some(move.get._2))
              }
            })
          } else {
            thinking = BagChalGame.Goat
            Future(game.clone().getBestGoatMove).map(move => {
              if (!move.isDefined) {
                gameRunning = false
              } else {
                thinking = 0
                executeGoatMove(move.get._1, Some(move.get._2))
              }
            })
          }
        }
      }
      //update entity positions
      renderer.entities.foreach(x => x.upd)

      if (gameRunning) {
        val sc = s"\nLast Goat score $lastGoatScore" +
          s"\nLast Tiger score $lastTigerScore"
        gameStatus.text = s"Game Status: $sc"
      } else {
        val turn = if (game.turn == BagChalGame.Goat) "Tiger wins" else "Goat wins"
        gameStatus.text = s"Game Status: $turn"
      }
    }
  }

  val timer = new AnimationTimer(t => {
    gameLock.synchronized {
      render
    }
  }) {

  }

  def loadGameFromChoice(continueOld : Boolean = false) = {
    val t = ai1.selectedToggle.value.getUserData.asInstanceOf[String] == "computer"
    val g = ai2.selectedToggle.value.getUserData.asInstanceOf[String] == "computer"
    loadGame(if (continueOld) game else new BagChalGame(5), t, g)
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
    gameRunning = true
    gameStatus.text = ""
  }

  loadGameFromChoice(false)
  timer.start()

}
