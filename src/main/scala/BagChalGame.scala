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
  val MX_GOATS = 20
  var goats_to_insert = MX_GOATS
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

  def print = {
    for (row <- 0 until size) {
      var s : String = ""
      for (col <- 0 until size) {
        val typ = state.matrix(row)(col)
        val ok = if (typ == BagChalGame.None) "-" else if (typ == BagChalGame.Tiger) "T" else "G"
        s = s + ok
      }
      println(s)
    }
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

    //(x, y) = (col, row)
    def isEmpty(pos : (Int, Int)) = {
      matrix(pos._2)(pos._1) == BagChalGame.None
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
  val INF = 1e9
  //get best move for goat, given that the opponents score is at least mins
  def getBestGoatMoveR(depth : Int, mins : Double) : (((Int, Int), (Int, Int)), Double) = {
    val moves = getPossibleGoatMoves

    if (moves.isEmpty) (null, -1000) else {
      if (depth == 1) (moves.head, -goats_eaten)
      else {
        var pruned = false
        var candidates : Seq[(((Int, Int), (Int, Int)), Double)] = Seq()
        var mymin = -INF
        moves.foreach(x => {
          if (!pruned) {
            executeGoatMove(x)
            val d = getTigerBestMoveR(depth - 1, mymin)
            executeGoatMove(x, true)
            if (mymin <= -d._2) {
              mymin = -d._2
              //if < is used here no need to add +1 down while returning otherwise we have to add +1
              //otherwise the move leading to game.state can be same as mins and it could be appended as a candidate
              //move(which may not be true since the move leading to game.state could have better score than mymin)
              //using <= instead of < prunes much more branches
              if (d._2 <= mins) {
                pruned = true
              } else {
                candidates = candidates :+ (x, -d._2)
              }
            }
          }
        })

        if (pruned) {
          //println(s"pruned at depth $depth for goat")
          (null, mymin + 1)
        } else {
          val bstScore = candidates.map(_._2).max
          candidates = candidates.filter(_._2 == bstScore)
          scala.util.Random.shuffle(candidates).head
        }
      }

    }
  }

  def goatVulnerability(pos : (Int, Int)) = {
    (-1 to 1).map(x => {
      if (x != 0 && (pos._1 + pos._2) % 2 != 0) 0
      else {
        val np = (pos._1 - x, pos._2 - 1)
        val np2 = (pos._1 + x, pos._2 + 1)
        val vulnerable = BagChalGame.inBounds(np._1, np._2, size) &&
          BagChalGame.inBounds(np2._1, np2._2, size) && state.isEmpty(np) && state.isEmpty(np2)
        if (vulnerable) 1 else 0
      }
    }).sum + 1 + {
      val np = (pos._1 - 1, pos._2)
      val np2 = (pos._1 + 1, pos._2)
      val vulnerable = BagChalGame.inBounds(np._1, np._2, size) &&
        BagChalGame.inBounds(np2._1, np2._2, size) && state.isEmpty(np) && state.isEmpty(np2)
      if (vulnerable) 1 else 0
    }
  }

  def getTigerBestMoveR(depth : Int, mins : Double) : (((Int, Int), (Int, Int), Int), Double) = {
    val moves = getPossibleTigerMoves

    if (moves.isEmpty) (null, -1000) else {
      if (depth == 1) (moves.head, goats_eaten)
      else {
        var pruned = false
        var candidates : Seq[(((Int, Int), (Int, Int), Int), Double)] = Seq()
        var mymin = -INF
        moves.foreach(x => {
          if (!pruned) {
            executeTigerMove(x)
            val d = getBestGoatMoveR(depth - 1, -INF)
            executeTigerMove(x, true)

            if (mymin <= -d._2) {
              mymin = -d._2
              //if < is used here no need to add +1 down while returning otherwise we have to add +1
              if (d._2 <= mins) {
                pruned = true
              } else {
                candidates = candidates :+ (x, -d._2)
              }
            }
          }
        })

        if (pruned) {
          (null, mymin + 1)
        } else {
          val bstScore = candidates.map(_._2).max
          var bst = candidates.filter(_._2 == bstScore)
          bst = scala.util.Random.shuffle(bst)
          val goat_eater_moves = bst.filter(_._1._3 == 1)
          if (goat_eater_moves.isEmpty) {
            if (depth == mxdepth && goats_to_insert < MX_GOATS) {
              val m = getGoats.map(x => (x, goatVulnerability(x)))
              val mx = m.map(_._2).max
              val bstGoat = m.filter(_._2 == mx).head._1
              println(bstGoat + " " + mx)
              val tmp = bst.map(x => {
                val dx = bstGoat._1 - x._1._2._1
                val dy = bstGoat._2 - x._1._2._2
                (x, (Math.abs(dx) + Math.abs(dy)) * 1.0 / mx)
              })
              val mind = tmp.map(_._2).min
              tmp.filter(_._2 == mind).head._1
            } else bst.head
          } else {
            goat_eater_moves.head
          }
        }

      }
    }
  }

  val mxdepth = 7

  def strategy2 = {
    val move = getTigerBestMoveR(mxdepth, -INF)
    move
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
      val move = getBestGoatMoveR(mxdepth, -INF)
      //println(s"goat pos ${move._2}")
      Some(move)
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

  var player_tiger = BagChalGame.None
  var player_goat = BagChalGame.None

  def setTiger(ai : Boolean): Unit = {
    player_tiger = if (ai) BagChalGame.Tiger else BagChalGame.None
  }

  def setGoat(ai : Boolean): Unit = {
    player_goat = if (ai) BagChalGame.Goat else BagChalGame.None
  }

  def setTurn(which : Int): Unit = {
    turn = which
  }

  def executeGoatMove(mv : ((Int, Int), (Int, Int)), undo : Boolean = false) = {
    if (mv._1 == mv._2) {
      goats_to_insert += (if (undo) 1 else -1)
      state.matrix(mv._1._2)(mv._1._1) = if (undo) BagChalGame.None else BagChalGame.Goat
    } else {
      val (s, d) = if (undo) (mv._2, mv._1) else mv
      state.matrix(s._2)(s._1) = BagChalGame.None
      state.matrix(d._2)(d._1) = BagChalGame.Goat
    }
  }

  def executeTigerMove(mv : ((Int, Int), (Int, Int), Int), undo : Boolean = false) = {
    if (mv._3 == 1) {
      val mid = BagChalGame.getMidPoint(mv._1, mv._2)
      state.matrix(mid._2)(mid._1) = if (undo) BagChalGame.Goat else BagChalGame.None
      goats_eaten += (if (undo) -1 else 1)
    }
    val (s, d) = if (undo) (mv._2, mv._1) else (mv._1, mv._2)
    state.matrix(s._2)(s._1) = BagChalGame.None
    state.matrix(d._2)(d._1) = BagChalGame.Tiger
  }

  def changeTurn() : Int = {
    turn = if (turn == BagChalGame.Goat) BagChalGame.Tiger else BagChalGame.Goat
    turn
  }

  def gameFinished = {
    getPossibleTigerMoves.isEmpty
  }

}
