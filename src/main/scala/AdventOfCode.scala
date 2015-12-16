
import ammonite.ops._

/**
  * Created by Giymo11 on 2015-12-16 at 23:33.
  */
object AdventOfCode {

  def main(args: Array[String]) {
    day2()
  }

  def day2() = {

    val input = read.lines ! cwd / 'AdventOfCode / 'day2
    //val input = List("2x3x4") // for testing putposes
    val intermediate = input map (_.split('x') map (_.toInt))
    val result = intermediate map (x => List(x(0) * x(1), x(1) * x(2), x(2) * x(0))) map (x => 2 * x.sum + x.min)
    println(result.sum)

    val result2 = intermediate map (x => x.sorted) map (x => 2 * (x(0) + x(1)) + x.product)
    println(result2.sum)
  }

  def day1() {

    def floorCounter(input: List[Char]): Int = input match {
      case x :: xs if x == '(' => 1 + floorCounter(xs)
      case x :: xs if x == ')' => -1 + floorCounter(xs)
      case _ => 0
    }

    val input = read ! cwd / 'AdventOfCode / 'day1
    println(floorCounter(input.toList))

    def basementChecker(input: List[Char], accu: Int, pos: Int): Int = input match {
      case xs if accu == -1 => pos
      case x :: xs if x == '(' => basementChecker(xs, accu + 1, pos + 1)
      case x :: xs if x == ')' => basementChecker(xs, accu - 1, pos + 1)
      case _ => -1
    }

    println(basementChecker(input.toList, 0, 0))
  }
}
