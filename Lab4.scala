object Lab4 {

  def tailMean(list1: List[Int], list2: List[Int]): List[Int] ={
    if(list1.isEmpty && list2.isEmpty){
      List()
    }
    else if(list1.nonEmpty && list2.nonEmpty){
      ((list1.head+list2.head)/2)::tailMean(list1.tail, list2.tail)
    }
    else if (list1.isEmpty){
      ((0+list2.head)/2)::tailMean(List(), list2.tail)
    }
    else {
      ((list1.head+0)/2)::tailMean(list1.tail, List())
    }
  }

  /*
  def noTailMean(list1: List[Int], list2: List[Int]): List[Int] ={

  }

   */

  def tailDecode(list: List[(Int, Any)]): List[Any] = {
    if(list.isEmpty){
      List()
    }
    else {
      if(list.head._1 >0) {
        list.head._2 :: tailDecode((list.head._1 - 1, list.head._2) :: list.tail)
      }
      else {
        tailDecode(list.tail)
      }
    }
  }


  def noTailDecode(list: List[(Int, Any)]): List[Any] = list match {
    case n :: rest => if(n._1>0){n._2 :: noTailDecode((n._1-1, n._2)::rest)}else{noTailDecode(rest)}
    case nil => List()
  }


  def main(args: Array[String]): Unit = {
    println("Task 1.1: ")
    println(tailMean(List(1,2,1,3,1),List(1,3,2,1)))
    println(tailMean(List(7,3,1,4),List(4,6,2,3,6,8)))
    println(tailMean(List(1,2,3,4,5),List(1,2,3,4,5,6,7)))
    println(tailMean(List(32,12,14,21,11),List(17,37,24,71)))
    println(tailMean(List(0,0,0,0,0),List(0,0,0,0,0)))

    println("Task 2.1: ")
    println(tailDecode(List((3, 'A'), (2, 'B'), (5, 'C'))))
    println(tailDecode(List((4, "Good"), (8, 2), (3, "Need"))))
    println(tailDecode(List((3, 'H'), (0, 'O'), (5, 'D'))))
    println(tailDecode(List((10, 10), (1, 1), (1, 'C'))))
    println(tailDecode(List((2, "Test"), (3, '#'), (8, ("test", 2)))))

    println("Task 2.2: ")
    println(noTailDecode(List((3, 'A'), (2, 'B'), (5, 'C'))))
  }
}
