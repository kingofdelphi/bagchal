package bagchal
/**
  * Created by dustbite on 7/29/17.
  */
object BagChalGame {
  val None = 0
  val Tiger = 1
  val Goat = 2
  def getMidPoint(src: (Int, Int), dest: (Int, Int)) = {
    val x : Int = (src._1 + dest._1) / 2
    val y : Int = (src._2 + dest._2) / 2
    (x, y)
  }

  val goat_deltas : Seq[(Int, Int)] = Seq(
    (-1, -1), (0, -1), (1, -1),
    (-1, 0),           (1, 0),
    (-1, 1),  (0, 1),  (1, 1)
  )

  val tiger_deltas : Seq[(Int, Int)] = Seq(goat_deltas, goat_deltas.map(x => (x._1 * 2, x._2 * 2))).flatten

  def inBounds(x : Int, y : Int, size : Int) = x >= 0 && x < size && y >= 0 && y < size

}

class BagChalGame(val size : Int) {
  var goats_to_insert = 20

  class State {
    val matrix = Array.ofDim[Int](size, size)
    matrix(0)(0) = BagChalGame.Tiger
    matrix(0)(size - 1) = BagChalGame.Tiger
    matrix(size - 1)(0) = BagChalGame.Tiger
    matrix(size - 1)(size - 1) = BagChalGame.Tiger

    def evaluate(turn : Int) : Double = {
      val goats = 0
      for (row <- (0 until size); col <- (0 until size)) {
        matrix(row)(col) match {
          case BagChalGame.Tiger =>
          case BagChalGame.Goat =>
          case BagChalGame.None =>
        }
      }
      0
    }

  }

  def dummy = {
    for (row <- (0 until size); col <- (0 until size)) {
      if (state.matrix(row)(col) == BagChalGame.None) {
        state.matrix(row)(col) = BagChalGame.Goat
      }
    }
    state.matrix(1)(0) = BagChalGame.None
  }

  def getPossibleGoatMoves = {
    val moves = if (goats_to_insert > 0) {
      for (row <- (0 until size); col <- (0 until size) if (state.matrix(row)(col) == BagChalGame.None)) yield {
        ((col, row), (col, row))
      }
    } else {
      val goat_pos = for (row <- (0 until size); col <- (0 until size) if (state.matrix(row)(col) == BagChalGame.Goat)) yield {
        (col, row)
      }
      goat_pos.flatMap(x => {
        BagChalGame.goat_deltas.map(y => {
          val (px, py) = (y._1 + x._1, y._2 + x._2)
          val valid = BagChalGame.inBounds(px, py, size) && state.matrix(py)(px) == BagChalGame.None
          (if (valid) x else (-1, -1), (px, py))
        }).filter(_._1._1 >= 0)

      })

    }

    moves.foreach(println)
    moves

  }

  def getPossibleTigerMoves = {
    val tiger_pos = for (row <- (0 until size); col <- (0 until size) if (state.matrix(row)(col) == BagChalGame.Tiger)) yield {
      (col, row)
    }

    val r = tiger_pos.flatMap(x => {
      BagChalGame.tiger_deltas.zipWithIndex.map(y => {
        val (px, py) = (y._1._1 + x._1, y._1._2 + x._2)
        val typ = Math.floorDiv(y._2, 8)
        val valid = BagChalGame.inBounds(px, py, size) && state.matrix(py)(px) == BagChalGame.None &&
          (typ match {
            case 0 => true
            case 1 =>
              val mid = BagChalGame.getMidPoint(x, (px, py))
              state.matrix(mid._2)(mid._1) == BagChalGame.Goat
          })

        (x, (px, py), if (valid) typ else -1)

      }).filter(_._3 >= 0)

    })

    r.foreach(println)

    r

  }

  def handleTigerMovement(src: (Int, Int), dest: (Int, Int)) : (Boolean, Boolean) = {
    val (dx : Int, dy : Int) = (dest._1 - src._1, dest._2 - src._2)
    val rd = Math.max(Math.abs(dx), Math.abs(dy))
    if (rd == 1) {
      (Math.abs(dx) + Math.abs(dy) == 1 || (src._1 + src._2) % 2 == 0, false)
    } else if (rd == 2) {
      val diag = dx == dy || dx == -dy
      val r = (diag && (src._1 + src._2) % 2 == 0 || Math.abs(dx) + Math.abs(dy) == 2) && {
        val t = BagChalGame.getMidPoint(src, dest)
        state.matrix(t._2)(t._1) == BagChalGame.Goat
      }
      (r, r)
    } else {
      (false, false)
    }
  }

  def handleGoatMovement(src: (Int, Int), dest: (Int, Int)) : Boolean = {
    val (dx : Int, dy : Int) = (dest._1 - src._1, dest._2 - src._2)
    if (Math.max(Math.abs(dx), Math.abs(dy)) <= 1) {
      Math.abs(dx) + Math.abs(dy) == 1 || (src._1 + src._2) % 2 == 0
    } else {
      false
    }
  }

  val state = new State
  var turn = BagChalGame.Goat

}
