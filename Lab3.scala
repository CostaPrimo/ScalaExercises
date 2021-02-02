object Lab3 {

  def subtractTailSum(list: List[Int]): Int ={
    if(list.size<=1){
      0
    }
    else {
      list.tail.sum-list.head
    }
  }

  def reverse(list: List[Any]): List[Any] = {
    list.foldLeft(List[Any]()){(r, h) => h::r}
    /*
    if(list.tail.isEmpty){
      List(list.head)
    }
    else {
      reverse(list.tail).appended(list.head)
    }
     */
  }



  def altListMerge(list1: List[Any], list2: List[Any]):List[Any]={
    if(list1.isEmpty){
      list2
    }
    else if(list2.isEmpty){
      list1
    }
    else {
      list1.head::altListMerge(list2, list1.tail)
    }
  }

  def main(args: Array[String]): Unit = {

    println("test1.1: "+ subtractTailSum(List(1,2,2)))
    println("test1.2: " + subtractTailSum(List(1,3,3)))
    println("test1.3: " +subtractTailSum(List(1,4)))
    println("test1.4: " + subtractTailSum(List(1)))
    println("test1.5: " + subtractTailSum(List(1,6,6)))
    println("task 1 done")

    println("Write a list separated by ';")
    val y1 = scala.io.StdIn.readLine()
    println(reverse(y1.split(";").toList))

    println("Write a list separated by ';'")
    val z1 = scala.io.StdIn.readLine()
    println("Write another list separated by ';'")
    val z2 = scala.io.StdIn.readLine()
    println(altListMerge(z1.split(";").toList, z2.split(";").toList))
    println("task 3 done")
  }
}
