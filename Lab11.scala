object Lab11 {
  sealed trait Debug {
    def debugName(): Unit = println("Class: " + this.getClass.getSimpleName)
    def debugVars(): Unit = for(x <- this.getClass.getDeclaredFields.indices){
      val temp = this.getClass.getDeclaredFields()(x)
      temp.setAccessible(true)
      println("Field: " + temp.getName + " => " + temp.getGenericType + ", " + temp.get(this))
    }
  }

  class Point(var1: Int, var2: Int) extends Debug {
    var a = var1
    var b = var2
    var c = "Lorem Ipsum"
  }

  def main(args: Array[String]): Unit = {
    val p = new Point(42, 420)
    p.debugName()
    p.debugVars()
  }
}

