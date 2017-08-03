package bagchal

import scalafx.Includes._
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.DoubleProperty.sfxDoubleProperty2jfx
import scalafx.event.{Event, EventType}
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.Button
import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Stop.sfxStop2jfx
import scalafx.scene.paint.{Color, CycleMethod, LinearGradient, Stop}
import scalafx.scene.shape.{ArcType, Rectangle}
import scalafx.scene.text.Font
import scalafx.scene.{Group, Scene}

/**
 * Example adapted from code showed in [[http://docs.oracle.com/javafx/2/canvas/jfxpub-canvas.htm]].
 */
object Main extends JFXApp {
  val (width, height) = (400, 400)
  val canvas = new Canvas(width, height)

  val rootPane = new Group
  val button1 = new Button("list tiger moves")
  val button2 = new Button("list goat moves")

  button1.onMouseClicked = (event : MouseEvent) => {
    game.getPossibleTigerMoves
  }

  button2.onMouseClicked = (event : MouseEvent) => {
    game.getPossibleGoatMoves
  }

  val box = new VBox
  val buttons = new HBox
  buttons.children = List(button1, button2)
  box.children = List(canvas, buttons)
  rootPane.children = List(box)

  val scene1 = new Scene {
    root = rootPane
  }

  stage = new PrimaryStage {
    title = "BagChal Game"
    scene = scene1
  }

  val gc = canvas.graphicsContext2D

  val game = new BagChalGame(5)
  //game.dummy
  game.setAI(BagChalGame.Tiger)
  game.setTurn(BagChalGame.Goat)

  var selbox = (0, 0)
  var first_sel = (-1, -1)
  var keypress : Option[KeyCode] = None
  var changeturn = 0

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
      case KeyCode.S =>
        if (first_sel == selbox)
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

  scene1.onKeyPressed = (event: KeyEvent) => {
    keypress = Some(event.code)
  }

  case class Point(val x : Double = 0, val y : Double = 0)

  val offset = 50
  val w : Double = Math.min(canvas.width.get, canvas.height.get) - 2 * offset

  val gsz = w / (game.size - 1)

  abstract class Entity(var src : Point, var dest : (Int, Int)) {
    var alpha = 1.0
    val f = 0.004
    var destroyed = false
    def destroy() = destroyed = true
    def moveTo(p : (Int, Int)) = {
      src = Point(p._1 * gsz, p._2 * gsz)
      dest = p
    }

    def transitionTo(p : (Int, Int)) = {
      dest = p
    }

    def upd = {
      val dp = Point(dest._1 * gsz - src.x, dest._2 * gsz - src.y)
      src = Point(src.x + dp.x * f, src.y + dp.y * f)
      if (destroyed) {
        alpha = 0.999 * alpha
      }
    }
    def draw
  }

  class Tiger(src_p : Point, dest_p : (Int, Int)) extends Entity(src_p, dest_p) {
    def draw = {
      gc.fill = if (game.turn == BagChalGame.Tiger) Color.Green else Color.Black
      gc.fillText("Tiger", src.x, src.y)
    }
  }

  class Goat(src_p : Point, dest_p : (Int, Int)) extends Entity(src_p, dest_p) {
    def draw = {
      gc.fill = if (!destroyed) (if (game.turn == BagChalGame.Goat) Color.Green else Color.Black) else Color.Red
      gc.setGlobalAlpha(alpha)
      gc.fillText("Goat", src.x, src.y)
      gc.setGlobalAlpha(1.0)
    }
  }

  var entities : Seq[Entity] = Seq()

  def loadEntities = {
    entities = Seq(game.getGoats.map(x => new Goat(Point(), x)), game.getTigers.map(x => new Tiger(Point(), x))).flatten
    entities.foreach(x => x.moveTo(x.dest))
  }

  def executeTigerMove(move : ((Int, Int), (Int, Int), Int)) = {
    if (move._3 == 1) {
      //remove goat
      val mid = BagChalGame.getMidPoint(move._1, move._2)
      entities.filter(x => !x.destroyed && x.dest == mid).foreach(x => x.destroy())
    }
    entities.filter(_.dest == move._1).foreach(x => x.transitionTo(move._2))
    game.executeTigerMove(move)
    changeturn = 1
  }

  def executeGoatMove(move : ((Int, Int), (Int, Int))) = {
    if (move._1 == move._2) {
      val goat = new Goat(Point(), move._1)
      goat.moveTo(goat.dest)
      entities = entities :+ goat
    } else {
      entities.filter(_.dest == move._1).foreach(x => x.transitionTo(move._2))
    }
    game.executeGoatMove(move)
    changeturn = 1
  }

  def render {
    //get events
    keypress.map(x => {
      handleKeyEvent(x)
      keypress = None
    })
    if (changeturn == 1) {
      val turn = game.changeTurn()
      if (turn == game.ai) {
        if (turn == BagChalGame.Tiger) {
          val move = game.getBestTigerMove

          if (!move.isDefined) println("game finished") else {
            executeTigerMove(move.get)
          }
        } else {
          val move = game.getBestGoatMove

          if (!move.isDefined) println("game finished") else {
            executeGoatMove(move.get)
          }
        }
      } else changeturn = 0
    }
    //update entity positions
    entities.foreach(x => x.upd)
    gc.fill = Color.White
    gc.fillRect(0, 0, canvas.width.get, canvas.height.get)
    gc.translate(offset, offset)

    gc.setStroke(Color.Blue);

    for (row <- (0 until game.size)) {
      gc.strokeLine(0, gsz * row, w, gsz * row)
    }

    for (col <- (0 until game.size)) {
      gc.strokeLine(gsz * col, 0, gsz * col, w)
    }

    gc.strokeLine(0, 0, w, w)

    gc.strokeLine(w, 0, 0, w)

    gc.strokeLine(w / 2, 0, 0, w / 2)
    gc.strokeLine(0, w / 2, w / 2, w)
    gc.strokeLine(w / 2, w, w, w / 2)
    gc.strokeLine(w, w / 2, w / 2, 0)

    val fontsize = 10

    entities.foreach(x => x.draw)

    gc.fill = Color.Red
    val rad = gsz / 3.0
    gc.setGlobalAlpha(0.3)
    gc.fillArc(selbox._1 * gsz - rad, selbox._2 * gsz - rad, 2 * rad, 2 * rad, 0, 360, ArcType.Open)
    gc.setGlobalAlpha(1)

    if (first_sel._1 != -1) {
      gc.setGlobalAlpha(0.3)
      gc.fillRect(first_sel._1 * gsz - rad, first_sel._2 * gsz - rad, 2 * rad, 2 * rad)
      gc.setGlobalAlpha(1)
    }

    gc.translate(-offset, -offset)

  }

  val timer = new AnimationTimer(t => {
    render
  }) {

  }

  loadEntities
  timer.start()

}
