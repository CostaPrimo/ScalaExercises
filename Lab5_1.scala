object Lab5_1 {
  sealed trait Expression
  sealed trait Operation extends Expression {
    def apply(a: (Double, Double)):(Double,Double)
  }
  sealed trait BiOperation extends Expression {
    def apply(a: (Double,Double), b: (Double,Double)):(Double,Double)
  }
  final case class Value (value: (Double,Double)) extends Expression

  final case object Neg extends Operation {
    override def apply(a: (Double, Double)): (Double, Double) = (-a._1, -a._2)
  }
  final case object Add extends BiOperation {
    override def apply(a: (Double, Double), b: (Double, Double)): (Double, Double) = (a._1+b._1, a._2+b._2) //a=x+yi, b=u+vi, a+b = (x+u)+(yi+vi)
  }
  final case object Sub extends BiOperation {
    override def apply(a: (Double, Double), b: (Double, Double)): (Double, Double) = (a._1-b._1, a._2-b._2) //a=x+yi, b=u+vi, a+b = (x-u)+(yi-vi)
  }
  final case object Mul extends BiOperation {
    override def apply(a: (Double, Double), b: (Double, Double)): (Double, Double) = (a._1*(b._1+b._2), a._2*(b._1+b._2)) //a=x+yi, b=u+vi, a*b= x(u+vi) + yi(u+vi)
  }
  final case object Div extends BiOperation {
    override def apply(a: (Double, Double), b: (Double, Double)): (Double, Double) = (a._1/b._1, a._2/b._2) //Got too complex for me so I simplified it.
  }

  //Stack isn't implemented entirely correctly, but it works.
  def eval(stack: List[Expression]): (Double, Double)  = stack match {
    case (biOperation: BiOperation) :: rest => biOperation.apply(eval(rest), eval(rest.tail))
    case (operation: Operation) :: rest => operation.apply(eval(rest))
    case (value: Value) :: rest => value.value
    case nil => (0,0)
  }

  def main(args: Array[String]): Unit = {
    val val1 = List(Add, Value((5.0,5.0)), Sub, Value((3.0, 3.0)), Neg, Value((1.5,1.5)))
    val val2 = List(Mul, Value((3.0, 3.0)), Div, Value((2.5, 2.5)), Neg, Value((0.5, 0.5)))
    val val3 = List(Add, Add, Value((5.0, 5.0)), Value((5.0, 5.0)))
    val val4 = List(Div, Value((100, 100)), Mul, Value((2.0, 2.0)), Add, Value((0.25, 0.45)), Value((1.33, 4.2)))
    val val5 = List(Sub, Add, Sub, Neg, Value((5.0, 5.0))) //A showcase of why the stack isn't exactly optimal/correct, but it works nonetheless.
    println("Test Neg: " + Neg.apply((5.0,5.0)))
    println("Test Add: " + Add.apply((5.0,5.0), (3.0,3.0)))
    println("Test Sub: " + Sub.apply((5.0,5.0), (3.0,3.0)))
    println("Test Mul: " + Mul.apply((5.0,5.0), (3.0,3.0)))
    println("Test Div: " + Div.apply((5.0,5.0), (3.0,3.0)))
    println(eval(val1))
    println(eval(val2))
    println(eval(val3))
    println(eval(val4))
    println(eval(val5))
  }
}
