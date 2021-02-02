object Lab7 {
  def zip(stream1: Stream[Any], stream2: Stream[Any]): Stream[Any] ={
    if (stream1.isEmpty){
      stream2
    }
    else if (stream2.isEmpty){
      stream1
    }
    else {
      stream1.head#::stream2.head#::zip(stream1.tail, stream2.tail)
    }
  }

  def main(args: Array[String]): Unit = {
    val test1_1 = Stream(1,2,3,4,5)
    val test1_2 = Stream('a', 'b', 'c')
    val c1 = zip(test1_1, test1_2)
    val test2_1 = Stream(1,2,3)
    val test2_2 = Stream('a','b','c')
    val c2 = zip(test2_1, test2_2)
    val test3_1 = Stream(1,2,3,4,5)
    val test3_2 = Stream.from(3)
    val c3 = zip(test3_1, test3_2)
    val test4_1 = Stream('a','b','c','d','e')
    val test4_2 = Stream.from(10)
    val c4 = zip(test4_1, test4_2)
    val test5_1 = Stream.from(51)
    val test5_2 = Stream.from(1)
    val c5 = zip(test5_1, test5_2)
    println(c1.take(10).toList)
    println(c2.take(10).toList)
    println(c3.take(10).toList)
    println(c3.take(25).toList)
    println(c4.take(10).toList)
    println(c4.take(25).toList)
    println(c5.take(10).toList)
    println(c5.take(25).toList)
  }
}
