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
  val AlphaTransitionTime = 1.5 //seconds

  var state = Entering

  var ztime : Long = System.currentTimeMillis()
  var origin : Point = src

  def destroy() = state = Destroyed

  //initial velocity, acceleration
  var u : Double = 0
  var a : Double = 0
  val PosTransitionTime : Double = 0.8

  def calc() = {
    val s = Math.max(Math.abs(dest.y - origin.y), Math.abs(dest.x - origin.x))
    u = 2 * s / PosTransitionTime
    if (s != 0) a = -u * u / 2 / s else a = 0
  }

  calc()

  def moveTo(p : Point) = {
    dest = p
    src = p
    origin = src
  }

  var paused : Boolean = false

  var pausetime : Long = 0

  def pause = {
    paused = true
    pausetime = System.currentTimeMillis()
  }

  def resume = {
    paused = false
    ztime = System.currentTimeMillis() - (pausetime - ztime)
  }

  def destroyed = {
    state == Destroyed
  }

  def entering = {
    state == Entering
  }

  def transitionTo(p : Point) = {
    dest = p
    ztime = System.currentTimeMillis()
    calc()
  }

  def alphaStable = alpha >= 0.995

  def isTransitioning = {
    !alphaStable || {
      val dx = src.x - dest.x
      val dy = src.y - dest.y
      Math.abs(dx) + Math.abs(dy) >= 1
    }
  }

  def upd(dt : Double) = {
    val tm = if (paused) pausetime else System.currentTimeMillis()
    val fd = (tm - ztime) / 1e3
    val md = Math.min(fd, PosTransitionTime)
    val xsgn = Math.signum(dest.x - origin.x)
    val ysgn = Math.signum(dest.y - origin.y)
    //val dp = md * Math.abs(dest.x - origin.x)
    val dp = u * md + 0.5 * a * md * md
    src = Point(origin.x + xsgn * dp, origin.y + ysgn * dp)
    val dalpha = dt / AlphaTransitionTime
    if (destroyed) {
      alpha = Math.max(0, alpha - dalpha)
    } else if (entering) {
      alpha = Math.min(alpha + dalpha, 1.0)
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

