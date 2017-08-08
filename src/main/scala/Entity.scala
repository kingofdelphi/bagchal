package bagchal

import utils.Utils.Point

import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color

/**
  * Created by dustbite on 8/7/17.
  */
abstract class Entity(var src : Point, var dest : Point, var data : Any = null) {
  var alpha = 1.0
  val f = 0.004
  val Entering = 0
  val Destroyed = 1
  val Stable = 2

  var state = Entering

  def destroy() = state = Destroyed

  def moveTo(p : Point) = {
    dest = p
    src = p
  }

  def destroyed = {
    state == Destroyed
  }

  def entering = {
    state == Entering
  }

  def transitionTo(p : Point) = {
    dest = p
  }

  def alphaStable = alpha >= 0.99

  def isTransitioning = {
    !alphaStable || {
      val dx = src.x - dest.x
      val dy = src.y - dest.y
      Math.abs(dx) + Math.abs(dy) >= 1
    }
  }

  def upd = {
    val dp = Point(dest.x - src.x, dest.y - src.y)
    src = Point(src.x + dp.x * f, src.y + dp.y * f)
    if (destroyed) {
      alpha = 0.999 * alpha
    } else if (entering) {
      alpha = Math.min(alpha + 0.001, 1.0)
      if (alphaStable) state = Stable
    }
  }

  def setEntering = {
    alpha = 0.0
    state = Entering
  }

  def draw(canvas : Canvas)
}

class Tiger(src_p : Point, dest_p : Point, dest_pos : (Int, Int)) extends Entity(src_p, dest_p, dest_pos) {

  def draw(canvas : Canvas) = {
    val gc = canvas.graphicsContext2D
    gc.drawImage(Renderer.tigerImage, src.x - Renderer.tigerImage.width.value / 2, src.y - Renderer.tigerImage.height.value / 2)
  }

}

class Goat(src_p : Point, dest_p : Point, dest_pos : (Int, Int)) extends Entity(src_p, dest_p, dest_pos) {

  def draw(canvas : Canvas) = {
    val gc = canvas.graphicsContext2D
    gc.setGlobalAlpha(alpha)
    gc.drawImage(Renderer.goatImage, src.x - Renderer.goatImage.width.value / 2, src.y - Renderer.goatImage.height.value / 2)
    gc.setGlobalAlpha(1.0)
  }

}

class SelBox(src_p : Point, dest_p : Point, alpha : Double) extends Entity(src_p, dest_p, alpha) {

  def draw(canvas : Canvas) = {
    val rad = 30
    val gc = canvas.graphicsContext2D
    gc.setGlobalAlpha(data.asInstanceOf[Double])
    gc.fill = Color.Red
    gc.fillRect(src.x - rad, src.y - rad, 2 * rad, 2 * rad)
    gc.setGlobalAlpha(1)
  }

}

