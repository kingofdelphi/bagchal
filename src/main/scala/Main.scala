package bagchal

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.DoubleProperty.sfxDoubleProperty2jfx
import scalafx.event.{Event, EventType}
import scalafx.scene.canvas.Canvas
import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
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

  rootPane.children = List(canvas)

  val scene1 = new Scene(width, height) {
    root = rootPane
  }

  stage = new PrimaryStage {
    title = "Canvas Doodle Test"
    scene = scene1
  }

  val gc = canvas.graphicsContext2D

  val game = new BagChalGame(5)
  var selbox = (0, 0)
  var first_sel = (-1, -1)

  scene1.onKeyPressed = (event: KeyEvent) => {
    val (dx : Int, dy : Int) = event.code match {
      case KeyCode.Left =>
        (-1, 0)
      case KeyCode.Right =>
        (1, 0)
      case KeyCode.Up =>
        (0, -1)
      case KeyCode.Down =>
        (0, 1)
      case KeyCode.Space =>
        if (first_sel == selbox)
          first_sel = (-1, -1)
        else {
          val curbox = game.state.matrix(selbox._2)(selbox._1)
          game.turn match {
            case BagChalGame.TIGER =>
              //not previously selected
              if (first_sel._1 == -1) {
                if (curbox == BagChalGame.TIGER) {
                  first_sel = selbox
                }
              } else {
                if (curbox == BagChalGame.NONE) {
                  val r = handleTigerMovement(first_sel, selbox)
                  if (r._1) {
                    game.turn = BagChalGame.GOAT
                    game.state.matrix(first_sel._2)(first_sel._1) = BagChalGame.NONE
                    game.state.matrix(selbox._2)(selbox._1) = BagChalGame.TIGER
                    if (r._2) {
                      val mid = getMidPoint(first_sel, selbox)
                      game.state.matrix(mid._2)(mid._1) = BagChalGame.NONE
                    }
                    first_sel = (-1, -1)
                  }
                }
              }

            case BagChalGame.GOAT =>
              if (game.goats_to_insert > 0) {
                if (curbox == BagChalGame.NONE) {
                  game.goats_to_insert -= 1
                  game.state.matrix(selbox._2)(selbox._1) = BagChalGame.GOAT
                  game.turn = BagChalGame.TIGER
                }
              } else {
                if (first_sel._1 == -1) {
                  if (curbox == BagChalGame.GOAT) {
                    first_sel = selbox
                  }
                } else {
                  if (curbox == BagChalGame.NONE) {
                    if (handleGoatMovement(first_sel, selbox)) {
                      game.turn = BagChalGame.TIGER
                      game.state.matrix(first_sel._2)(first_sel._1) = BagChalGame.NONE
                      game.state.matrix(selbox._2)(selbox._1) = BagChalGame.GOAT
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
    render
  }

  def getMidPoint(src: (Int, Int), dest: (Int, Int)) = {
    val x : Int = (src._1 + dest._1) / 2
    val y : Int = (src._2 + dest._2) / 2
    (x, y)
  }

  def handleTigerMovement(src: (Int, Int), dest: (Int, Int)) = {
    val (dx : Int, dy : Int) = (dest._1 - src._1, dest._2 - src._2)
    val rd = Math.max(Math.abs(dx), Math.abs(dy))
    if (rd == 1) {
      (Math.abs(dx) + Math.abs(dy) == 1 || (src._1 + src._2) % 2 == 0, false)
    } else if (rd == 2) {
      val diag = (dx == dy || dx == -dy)
      val r = (diag && (src._1 + src._2) % 2 == 0 || Math.abs(dx) + Math.abs(dy) == 2) && {
        val t = getMidPoint(src, dest)
        game.state.matrix(t._2)(t._1) == BagChalGame.GOAT
      }
      (r, r)
    } else {
      (false, false)
    }
  }

  def handleGoatMovement(src: (Int, Int), dest: (Int, Int)) = {
    val (dx : Int, dy : Int) = (dest._1 - src._1, dest._2 - src._2)
    if (Math.max(Math.abs(dx), Math.abs(dy)) <= 1) {
      Math.abs(dx) + Math.abs(dy) == 1 || (src._1 + src._2) % 2 == 0
    } else {
      false
    }
  }

  def render {
    gc.fill = Color.White
    gc.fillRect(0, 0, canvas.width.get, canvas.height.get)

    val offset = 50
    val w = Math.min(canvas.width.get, canvas.height.get) - 2 * offset

    val gsz = w / (game.size - 1)

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


    for (row <- (0 until game.size); col <- (0 until game.size)) {
      game.state.matrix(row)(col) match {
        case BagChalGame.TIGER =>
          gc.fill = if (game.turn == BagChalGame.TIGER) Color.Red else Color.Black
          gc.fillText("Tiger", col * gsz - fontsize, row * gsz)
        case BagChalGame.GOAT =>
          gc.fill = if (game.turn == BagChalGame.GOAT) Color.Green else Color.Black
          gc.fillText("Goat", col * gsz - fontsize, row * gsz)
        case BagChalGame.NONE =>
      }
    }
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

  render

}
