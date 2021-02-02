object Lab5 {
  sealed trait Tree[+A]
  case class Leaf[A](Value: A) extends Tree[A]
  case class Branch[A](Value: A, left: Tree[A], Right: Tree[A]) extends Tree[A]

  //makes a DFS list over the sums of the sets of doubles within a Tree
  def traverse(t: Tree[(Double, Double)]): List[Double] = t match {
    case Branch(x, l, r) => (x._1+x._2) :: traverse(l).concat(traverse(r))
    case Leaf(x) => List(x._1+x._2)
  }
  //returns the minimum value within a list as a double
  def findMin(list: List[Double]): Double = {
    list.min
  }
  //Searches a tree for sum value min and returns the path.
  def pathTo(t: Tree[(Double, Double)], min: Double): List[Any] = t match {
    case Branch(x, l, r) => if((x._1+x._2)==min){List(x)} //if current node have the sum we are looking for the value is returned
                            else if(pathTo(l, min).isEmpty) { //else if the node with the sum we are looking for is not in the left child branch/leaf
                              if(pathTo(r, min).isEmpty){List()} // and if the node with the sum we are looking for isnt in right child/branch either return empty list
                              else{x :: pathTo(r, min)} //else return value for node along with current path for right child
                            }
                            else{x :: pathTo(l, min)} //else return value for node along with current path for left child
    case Leaf(x) => if((x._1+x._2)==min){List(x)}else{List()}
  }

  def main(args: Array[String]): Unit = {
    val tr = traverse(Branch((5.0,5.0),Branch((4.0, 3.0), Leaf((2.0, 2.0)), Leaf(2.5, 2.5)), Branch((4.0, 4.5), Leaf((3.2, 2.3)), Leaf((1.0, 1.0)))))
    val min = findMin(tr)
    println(pathTo(Branch((5.0,5.0),Branch((4.0, 3.0), Leaf((2.0, 2.0)), Leaf(2.5, 2.5)), Branch((4.0, 4.5), Leaf((3.2, 2.3)), Leaf((1.0, 1.0)))),min))
  }
}
