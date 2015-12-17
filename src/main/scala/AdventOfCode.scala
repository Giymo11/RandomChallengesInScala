
import java.security.MessageDigest

import ammonite.ops._

/**
  * Created by Giymo11 on 2015-12-16 at 23:33.
  */
object AdventOfCode {

  def main(args: Array[String]) {
    day4()
  }

  def day4() = {

    val messageDigest = MessageDigest.getInstance("MD5")

    def md5(s: String): Array[Byte] = {
      messageDigest.digest(s.getBytes)
    }

    def crackit(secret: String, number: Int): Int = {
      val res = md5(secret + number)

      if (number % 10000 == 0) println(number)

      if (res(0) == 0 && res(1) == 0 && res(2) == 0) {
        println(res.map(x => f"$x%02x").mkString(""))
        return number
      }

      crackit(secret, number + 1)
    }

    println(crackit("bgvyzdsv", 0))
  }

  def day3() = {

    val input = read ! cwd / 'AdventOfCode / 'day3
    //val input = "^v^v^v^v^v"

    case class Coordinate(x: Int, y: Int)

    def navigate(input: List[Char], map: Map[Coordinate, Int], position: Coordinate): Map[Coordinate, Int] = input match {
      case x :: xs =>
        val deltaX = if (x == '^') 1 else if (x == 'v') -1 else 0
        val deltaY = if (x == '>') 1 else if (x == '<') -1 else 0
        navigate(xs, map + (position -> (map.getOrElse(position, 0) + 1)), Coordinate(position.x + deltaX, position.y + deltaY))
      case _ => map
    }

    val map = navigate(input.toList, Map[Coordinate, Int](Coordinate(0, 0) -> 1), Coordinate(0, 0))
    //println(map)
    println(map.values.size)

    def navigateWithALittleHelpFromMyFriends(input: List[Char], santasTurn: Boolean,
                                             santaMap: Map[Coordinate, Int], santaPos: Coordinate,
                                             roboMap: Map[Coordinate, Int], roboPos: Coordinate
                                            ): Seq[Map[Coordinate, Int]] = input match {

      case x :: xs =>

        val deltaX = if (x == '^') 1 else if (x == 'v') -1 else 0
        val deltaY = if (x == '>') 1 else if (x == '<') -1 else 0

        if (santasTurn) {
          val newSantaPos = Coordinate(santaPos.x + deltaX, santaPos.y + deltaY)
          navigateWithALittleHelpFromMyFriends(xs, !santasTurn,
            santaMap + (newSantaPos -> (santaMap.getOrElse(newSantaPos, 0) + 1)), newSantaPos,
            roboMap, roboPos)
        }
        else {
          val newRoboPos = Coordinate(roboPos.x + deltaX, roboPos.y + deltaY)
          navigateWithALittleHelpFromMyFriends(xs, !santasTurn,
            santaMap, santaPos,
            roboMap + (newRoboPos -> (roboMap.getOrElse(newRoboPos, 0) + 1)), newRoboPos)
        }

      case _ => Seq(santaMap, roboMap)
    }

    val maps = navigateWithALittleHelpFromMyFriends(input.toList, santasTurn = true,
      Map[Coordinate, Int](Coordinate(0, 0) -> 1), Coordinate(0, 0),
      Map[Coordinate, Int](Coordinate(0, 0) -> 1), Coordinate(0, 0))

    //println(maps)
    println(maps.reduce(_ ++ _).values.size)
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
