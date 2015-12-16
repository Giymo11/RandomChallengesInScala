/**
  * Created by Giymo11 on 2015-11-24 at 15:27.
  */
object Starwars {
  def main(args: Array[String]) {
    (for (i <- 1 to 15; l <- Seq("d", "l", "b")) yield s"https://www.gstatic.com/beyond/img/$l$i.jpg").foreach(println)
  }
}
