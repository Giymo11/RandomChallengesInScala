import java.io.File

import scala.io.Source

/**
  * Created by Giymo11 on 2015-11-13 at 02:17.
  */
object IntroSec {

  def main(args: Array[String]) {
    val integers = Source.fromFile(new File("IntroSec")).getLines().map(_.grouped(2).map(Integer.parseInt(_, 16)).toList).toList

    val crib = "conference in March, he went a step furtherâ\u0080\u0094a small screen bearing a live image of his face was p".map(_.toInt)

    def getKey(crib: Seq[Int], cypher: Seq[Int]): Seq[Int] = for (index <- crib.indices) yield crib(index) ^ cypher(index)

    def checkKey(integers: List[List[Int]], key: Seq[Int], i: Int): Boolean = {
      for (list <- integers; j <- key.indices)
        if (i + j < list.length) {
          val current = (list(i + j) ^ key(j)).toChar
          if (!((current >= 'A' && current <= 'Z') || (current >= 'a' && current <= 'z') || current == ' ' || current == '!' || current == '?' || current == '.'))
            return false
        }
      true
    }

    def checkKey2(key: Seq[Int]) = {
      key.containsSlice(Vector(71, 68, 104, 110, 115, 80, 109, 103, 100))
    }

    def decypher(integers: List[List[Int]], key: Seq[Int], i: Int) = {
      for (list <- integers) {
        println()
        for (j <- key.indices)
          if (i + j < list.length) {
            val current = (list(i + j) ^ key(j)).toChar
            print(current)
          }
      }
    }

    def decypher2(integers: List[List[Int]], key: Seq[Int], i: Int) = {
      for (list <- integers) {
        for (index <- i until math.min(list.length, key.length))
          print((list(index) ^ key(index - i)).toChar)
        println()
      }
    }

    for (list <- integers) {
      for (i <- 0 until list.length - crib.length) {
        val key = getKey(crib, list.drop(i))

        //val isGood = checkKey(integers, key, i)
        val isGood = checkKey2(key)
        if (isGood) {
          println(i)
          println(key)
          //decypher1(integers, key, i)
          decypher2(integers, key, i)
        }
      }
    }

    integers.foreach(println)
  }
}
