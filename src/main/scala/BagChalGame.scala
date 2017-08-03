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
  var goats_eaten = 0

  override def clone = {
    val game = new BagChalGame(size)
    game.goats_to_insert = goats_to_insert
    game.goats_eaten = goats_eaten

    for (row <- (0 until size); col <- (0 until size)) {
      game.state.matrix(row)(col) = state.matrix(row)(col)
    }
    game
  }

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
    goats_to_insert = 0
  }

  def getGoats = {
    for (row <- (0 until size); col <- (0 until size) if (state.matrix(row)(col) == BagChalGame.Goat)) yield {
      (col, row)
    }
  }

  def getPossibleGoatMoves = {
    val moves = if (goats_to_insert > 0) {
      for (row <- (0 until size); col <- (0 until size) if (state.matrix(row)(col) == BagChalGame.None)) yield {
        ((col, row), (col, row))
      }
    } else {
      val goat_pos = getGoats
      goat_pos.flatMap(x => {
        BagChalGame.goat_deltas.map(y => {
          val (px, py) = (y._1 + x._1, y._2 + x._2)
          val valid = BagChalGame.inBounds(px, py, size) && state.matrix(py)(px) == BagChalGame.None &&
            (y._1 == 0 || y._2 == 0 || (x._1 + x._2) % 2 == 0)
          (if (valid) x else (-1, -1), (px, py))
        }).filter(_._1._1 >= 0)

      })

    }

    moves

  }

  def getTigers = {
    for (row <- (0 until size); col <- (0 until size) if (state.matrix(row)(col) == BagChalGame.Tiger)) yield {
      (col, row)
    }
  }

  def getPossibleTigerMoves = {
    val tiger_pos = getTigers

    val r = tiger_pos.flatMap(x => {
      BagChalGame.tiger_deltas.zipWithIndex.map(y => {
        val (px, py) = (y._1._1 + x._1, y._1._2 + x._2)
        val typ = Math.floorDiv(y._2, 8)
        val valid = BagChalGame.inBounds(px, py, size) && state.matrix(py)(px) == BagChalGame.None &&
          (y._1._1 == 0 || y._1._2 == 0 || (x._1 + x._2) % 2 == 0) &&
          (typ match {
            case 0 => true
            case 1 =>
              val mid = BagChalGame.getMidPoint(x, (px, py))
              state.matrix(mid._2)(mid._1) == BagChalGame.Goat
          })
        if (valid && state.matrix(py)(px) != BagChalGame.None) {
          println("fuck")
        }
        (x, (px, py), if (valid) typ else -1)

      }).filter(_._3 >= 0)

    })

    r

  }

  def strategy1 = {
    util.Random.shuffle(getPossibleTigerMoves).headOption
  }

  def getBestGoatMoveR(game : BagChalGame, depth : Int) : (((Int, Int), (Int, Int)), Double) = {
    val moves = game.getPossibleGoatMoves

    if (moves.isEmpty) (null, -1000) else {
      if (depth == 1) (moves.head, -game.goats_eaten)
      else {
        val candidates = moves.map(x => {
          val cl = game.clone()
          cl.executeGoatMove(x)
          val d = getTigerBestMoveR(cl, depth - 1)
          (x, -d._2)
        })

        val bstScore = candidates.map(_._2).max
        candidates.filter(_._2 == bstScore).head
      }

    }
  }

  def getTigerBestMoveR(game : BagChalGame, depth : Int) : (((Int, Int), (Int, Int), Int), Double) = {
    val moves = game.getPossibleTigerMoves

    if (moves.isEmpty) (null, -1000) else {
      if (depth == 1) (moves.head, game.goats_eaten)
      else {
        val candidates = moves.map(x => {
          val cl = game.clone()
          cl.executeTigerMove(x)
          val d = getBestGoatMoveR(cl, depth - 1)
          (x, -d._2)
        })

        val bstScore = candidates.map(_._2).max
        val bst = candidates.filter(_._2 == bstScore)
        val goat_eater_moves = bst.filter(_._1._3 == 1)
        if (goat_eater_moves.isEmpty) bst.head else goat_eater_moves.head
      }
    }
  }

  val mxtigerdepth = 5
  val mxgoatdepth = 5

  def strategy2 = {
    val move = getTigerBestMoveR(clone(), mxtigerdepth)
    move._1
  }

  def strategy3 = {
    val move = getTigerBestMoveR(clone(), 2)
    move._1
  }

  def getBestTigerMove = {
    if (getPossibleTigerMoves.isEmpty) None
    else {
      Some(strategy2)
    }
  }

  def getBestGoatMove = {
    if (getPossibleGoatMoves.isEmpty) None
    else {
      val move = getBestGoatMoveR(clone(), mxgoatdepth)
      Some(move._1)
    }
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

  var ai = BagChalGame.None

  def setAI(ai_type : Int): Unit = {
    ai = ai_type
  }

  def setTurn(which : Int): Unit = {
    turn = which
  }

  def executeGoatMove(mv : ((Int, Int), (Int, Int))) = {
    if (mv._1 == mv._2) {
      goats_to_insert -= 1
    }
    state.matrix(mv._1._2)(mv._1._1) = BagChalGame.None
    state.matrix(mv._2._2)(mv._2._1) = BagChalGame.Goat
  }

  def executeTigerMove(mv : ((Int, Int), (Int, Int), Int)) = {
    if (mv._3 == 1) {
      val mid = BagChalGame.getMidPoint(mv._1, mv._2)
      state.matrix(mid._2)(mid._1) = BagChalGame.None
      goats_eaten += 1
    }
    state.matrix(mv._1._2)(mv._1._1) = BagChalGame.None
    state.matrix(mv._2._2)(mv._2._1) = BagChalGame.Tiger
  }

  def changeTurn() : Int = {
    turn = if (turn == BagChalGame.Goat) BagChalGame.Tiger else BagChalGame.Goat
    turn
  }

}
