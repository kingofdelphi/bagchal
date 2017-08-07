package bagchal
/**
  * Created by dustbite on 8/7/17.
  */
import utils.Utils.Point

import scalafx.scene.canvas.Canvas
import scalafx.scene.image.Image
import scalafx.scene.paint.Color

object Renderer {

  val tigerImage = new Image(getClass().getResourceAsStream("/tiger.png"), 64, 64, true, true)
  val goatImage = new Image(getClass().getResourceAsStream("/goat.png"), 64, 64, true, true)

}

class Renderer(canvas : Canvas) {

  val gc = canvas.graphicsContext2D

  val offset = 50

  val w : Double = Math.min(canvas.width.get, canvas.height.get) - 2 * offset

  var gsz : Double = 0

  var game : BagChalGame = null

  var entities : Seq[Entity] = Seq()

  def setGame(_game : BagChalGame) = {
    game = _game
    gsz = w / (game.size - 1)
    loadEntities
  }

  //get pos from (column, row)
  def getPos(cr : (Int, Int)) = {
    Point(cr._1 * gsz, cr._2 * gsz)
  }

  def loadEntities = {
    entities = Seq(game.getGoats.map(x => new Goat(Point(), getPos(x), x)), game.getTigers.map(x => new Tiger(Point(), getPos(x), x))).flatten
    entities.foreach(x => x.moveTo(x.dest))
  }


  def render = {
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

    entities.foreach(x => x.draw(canvas))
    gc.translate(-offset, -offset)

  }

  def drawEntity(entity : Entity) = {
    gc.translate(offset, offset)
    entity.draw(canvas)
    gc.translate(-offset, -offset)
  }

}
