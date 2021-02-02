object Lab2 {

  def medisum(in: (Int, Int)):(Int, Int) ={
    (in._1+in._2, (in._1-in._2))
  }

  def geometric(base: Int, ratio: Int, numbers: Int):List[Int] ={
    if(numbers>0) {
      base::geometric(base*ratio,ratio,numbers-1)
    }
    else{
      List()
    }
  }

  def wordcharcount(list: List[String], char: Char): Int = {
    if (list.nonEmpty) {
      if (list.tail.nonEmpty) {
        if (list.head.contains(char)) {
          1 + wordcharcount(list.tail, char)
        }
        else {
          0 + wordcharcount(list.tail, char)
        }
      }
      else {
        if (list.head.contains(char)) {
          1
        }
        else {
          0
        }
      }
    }
    else{
        0
    }
  }

  def main(args: Array[String]): Unit = {

    println("Write number a Number")
    val x = scala.io.StdIn.readInt()
    println("Write Second number")
    val x2 = scala.io.StdIn.readInt()
    println(medisum(x,x2))
    println("task 1 done")

    println("Write base number")
    val z = scala.io.StdIn.readInt()
    println("Write ratio for the sequence")
    val z2 = scala.io.StdIn.readInt()
    println("Write length of sequence")
    val z3 = scala.io.StdIn.readInt()
    println(geometric(z, z2, z3))
    println("task 2 done")

    println("Write list of words seperated by ',':")
    val y = scala.io.StdIn.readLine()
    println("Write character to search for in words:")
    val y2 = scala.io.StdIn.readChar()
    println(wordcharcount(y.split(",").toList, y2))
    println("task 3 done")
  }
}
