package bagchal
/**
  * Created by dustbite on 7/29/17.
  */
object BagChalGame {
  val NONE = 0
  val TIGER = 1
  val GOAT = 2
}

class BagChalGame(val size : Int) {
  var goats_to_insert = 20
  class State {
    val matrix = Array.ofDim[Int](size, size)
    matrix(0)(0) = BagChalGame.TIGER
    matrix(0)(size - 1) = BagChalGame.TIGER
    matrix(size - 1)(0) = BagChalGame.TIGER
    matrix(size - 1)(size - 1) = BagChalGame.TIGER
  }
  val state = new State
  var turn = BagChalGame.GOAT

}
