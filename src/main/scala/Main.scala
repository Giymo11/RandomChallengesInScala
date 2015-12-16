/**
  * Created by Giymo11 on 2015-11-12 at 20:46.
  */
object Main {
  def main(args: Array[String]) {
    val input = args.map(_.toInt)
    assert(input.length <= 15)

    def check(input: Array[Int], taken: Array[Boolean], start: Int, sum: Int): Unit = {
      if (sum == 20)
        println((((taken zip input) zipWithIndex) filter (_._1._1) map (tuple => s"${tuple._1._2}(${tuple._2})") mkString " + ") + " = 20")
      //^ bonus if you figure this one out ^
      else if (sum < 20 && start <= input.length - 1) {
        check(input, taken, start + 1, sum)
        taken(start) = true
        check(input, taken, start + 1, sum + input(start))
        taken(start) = false
      }
    }
    check(input, new Array[Boolean](input.length), 0, 0)

    def check2(input: List[Int], target: Int): List[List[Int]] = input match {
      case x :: xs if x == target => List(List(x))
      case x :: xs => check2(xs, target) ++ (if (x < target) check2(xs, target - x) map (x +: _) else Nil)
      case _ => Nil
    }
    check2(input.toList, 20) map (_.mkString(" + ") + " = 20") foreach println
  }
}
