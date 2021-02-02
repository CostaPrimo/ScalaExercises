object Lab6 {

  //Functional Split
  def fSplit(list: List[Any]): (List[Any], List[Any]) = {
    val rList = (List(), List())
    if (list.isEmpty){
      (rList._1, rList._2)
    }
    else if(list.tail.isEmpty){
      (rList._1.appended(list.head), rList._2)
    }
    else {
      if (list.tail.tail.isEmpty){
        (rList._1.appended(list.head), rList._2.appended(list.tail.head))
      }
      else {
        val tList = fSplit(list.tail.tail)
        (rList._1.appended(list.head).concat(tList._1), rList._2.appended(list.tail.head).concat(tList._2))
      }
    }
  }

  //Imperial Split
  def iSplit(list: List[Any]): (List[Any], List[Any]) = {
    val list1 = new Array[Any](list.size/2+1)
    val list2 = new Array[Any](list.size/2+1)
    var x = 0
    var y = 0
    for (n <- 0 to list.size-1){

      if(n%2==0 || n==0){
        list1(x)= list(n)
        x = x+1
      }
      else {
        list2(y)= list(n)
        y = y+1
      }
    }
    (list1.toList, list2.toList)
  }

  //Stream of Powers
  def powers(n:Int, x: Int): Stream[Int] = scala.math.pow(n, x).toInt #:: powers(n, x+1) //Support to do recursive loop with incrementing exponent.
  def powers(n:Int): Stream[Int] = scala.math.pow(n, 0).toInt #:: powers(n, 1) //"Main" Stream

  def main(args: Array[String]): Unit = {
    val list1 = List("This", 1, "is", 2, "a", 3, "test", 4, "!")
    val list2 = List("How" , "Far", "Can", "I", "Take", "This", "Without", "Stopping", 1, 2, 3, 4, 5, 6)
    val list3 = List(("Test", 1), "Test", 1, 'D')
    val list4 = List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
    val list5 = List("Test", "One", "Last", "Time")
    println("TASK: 1")
    print("fSplit: ")
    println(fSplit(list1))
    print("iSplit: ")
    println(iSplit(list1))
    print("fSplit: ")
    println(fSplit(list2))
    print("iSplit: ")
    println(iSplit(list2))
    print("fSplit: ")
    println(fSplit(list3))
    print("iSplit: ")
    println(iSplit(list3))
    print("fSplit: ")
    println(fSplit(list4))
    print("iSplit: ")
    println(iSplit(list4))
    print("fSplit: ")
    println(fSplit(list5))
    print("iSplit: ")
    println(iSplit(list5))

    val power1 = powers(4).take(10)
    val power2 = powers(6).take(5)
    val power3 = powers(12).take(6)
    val power4 = powers(2).take(32)
    val power5 = powers(3).take(3)
    println("\nTASK 2")
    println(power1.toList)
    println(power2.toList)
    println(power3.toList)
    println(power4.toList)
    println(power5.toList)
  }
}
