object Lab10 {
  //Thread sorting-----------------------------------------------------------------------------------------------------
  var threadcounter = 0;

  class Sorter(toSort: threadSorter, listA: List[Int], listB: List[Int]) extends Runnable {
    override def run(): Unit = {
      threadcounter = threadcounter+1
      toSort.merge(listA, listB)
    }
  }

  class threadSorter(list: List[Int]) {
    var toSort = list

    def split(): Unit = {
      if (toSort.size > 1) {
        val m = toSort.size / 2
        var lSub = List[Int]()
        var rSub = List[Int]()
        for (x <- 0 to m - 1) {
          lSub = toSort(x) :: lSub
        }
        for (x <- m to list.size - 1) {
          rSub = toSort(x) :: rSub
        }
        val left = new threadSorter(lSub.reverse)
        val right = new threadSorter(rSub.reverse)
        left.split()
        right.split()
        val t1 = new Thread(new Sorter(this, left.getList(), right.getList()))
        t1.start()
        while (t1.isAlive()) {}
      }
    }

    def merge(listA: List[Int], listB: List[Int]): Unit = {
      toSort = sort(listA, listB)
    }

    def getList(): List[Int] = {
      toSort
    }
  }
  //Non thread sorting-------------------------------------------------------------------------------------------------
  def mergeSort(list: List[Int]): List[Int] = {
    if (list.size > 1){
      val m = list.size/2
      var lSub = List[Int]()
      var rSub = List[Int]()
      for (x <- 0 to m-1){
        lSub = list(x)::lSub
      }
      for (x <- m to list.size-1){
        rSub = list(x)::rSub
      }
      lSub = mergeSort(lSub)
      rSub = mergeSort(rSub)
      sort(lSub, rSub)
    }
    else{
      list
    }
  }
  //Support -----------------------------------------------------------------------------------------------------------
  def sort(listA: List[Int], listB: List[Int]): List[Int] = {
    if(listA.isEmpty && listB.isEmpty){
      List()
    }
    if(listA.isEmpty){
      return listB
    }
    if(listB.isEmpty){
      return listA
    }
    if(listA.head > listB.head){
      listB.head :: sort(listA, listB.tail)
    }
    else {
      listA.head :: sort(listA.tail, listB)
    }
  }

  def main(args: Array[String]): Unit = {
    //Base case
    val time = System.currentTimeMillis()
    println(mergeSort(List(22, 55, 91, 27, 15, 35, 74, 40, 1, 8, 51, 5, 42, 56, 76, 5, 48, 10, 81, 3, 9, 54, 45, 30,
      100, 50, 37, 20, 47, 15, 36, 8, 20, 69, 13, 14, 85, 7, 32, 64, 59, 52, 24, 19, 60, 81, 18, 91, 26, 6, 67, 70, 45,
      13, 1, 90, 4, 95, 9, 70, 89, 2, 6, 1, 40, 94, 89, 32, 16, 74, 95, 21, 76, 13, 85, 20, 10, 31, 72, 92, 46, 97, 9,
      42, 4, 29, 2, 97, 7, 81, 53, 80, 23, 8, 69, 83, 17, 57, 90, 90)))
    println("time taken: " + (System.currentTimeMillis()-time) + " milliseconds")
    //Thread solution
    val time2 = System.currentTimeMillis()
    val test = new threadSorter(List(22, 55, 91, 27, 15, 35, 74, 40, 1, 8, 51, 5, 42, 56, 76, 5, 48, 10, 81, 3, 9, 54, 45, 30,
      100, 50, 37, 20, 47, 15, 36, 8, 20, 69, 13, 14, 85, 7, 32, 64, 59, 52, 24, 19, 60, 81, 18, 91, 26, 6, 67, 70, 45,
      13, 1, 90, 4, 95, 9, 70, 89, 2, 6, 1, 40, 94, 89, 32, 16, 74, 95, 21, 76, 13, 85, 20, 10, 31, 72, 92, 46, 97, 9,
      42, 4, 29, 2, 97, 7, 81, 53, 80, 23, 8, 69, 83, 17, 57, 90, 90))
    test.split()
    println(test.getList())
    println("Time taken: " + (System.currentTimeMillis()-time2) + " milliseconds")
    println("Threads created: " + threadcounter)
    //Test on smaller list
    val temp = threadcounter
    val time3 = System.currentTimeMillis()
    val test1 = new threadSorter(List(22, 55, 91, 27, 15, 35, 74, 40, 1, 8, 51, 5, 42, 56, 76, 5, 48, 10, 81, 3, 9, 54, 45, 30,
      100, 50, 37, 20, 47, 15, 36, 8, 20, 69, 13, 14, 85, 7, 32, 64, 59, 52, 24, 19, 60, 81, 18, 91, 26, 6))
    test1.split()
    println(test1.getList())
    println("Time taken: " + (System.currentTimeMillis()-time3) + " milliseconds")
    println("Threads created: " + (threadcounter-temp))
  }
}
