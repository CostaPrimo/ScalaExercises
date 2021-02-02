object Lab8 {
  class Employee(fName: String, lName: String, pay: Double){
    private var firstName = fName
    private var lastName = lName
    private var paycheck = pay
    def raise(percent: Double): Unit = {
      paycheck = paycheck*(1+(percent/100))
    }

    override def toString: String = "Name: " + firstName + " " + lastName + ", Pay: " + paycheck
  }

  class Specialist(fName: String, lName: String, pay: Double, bonus: Double) extends Employee(fName, lName, pay) {
    private var bonusPay = bonus
    def bonus(amount: Double): Unit = {
      bonusPay = bonusPay+amount
    }

    override def toString: String = super.toString + ", Bonus: " + bonusPay
  }

  class Company(){
    private var employees = List[Employee]()

    def addEmployee(employee: Employee): Unit ={
      employees = employees.::(employee)
    }

    def raisePay(): Unit ={
      employees.foreach(_.raise(5))
    }
    def giveBonus(): Unit ={
      employees.filter(_.isInstanceOf[Specialist]).foreach(_.asInstanceOf[Specialist].bonus(250))
    }

    override def toString: String = {
      var out = ""
      employees.foreach(out+=_.toString+"\n")
      out
    }
  }

  def main(args: Array[String]): Unit = {
    val x = new Employee("Steve", "Rogers", 1000)
    val y = new Specialist("Scott", "Sterling", 1000, 500)
    val z = new Company()
    //Variables for the switch
    var running = true
    var fName = ""
    var lName = ""
    var salary = 0.0
    var bonus = 0.0
    //Adding employees to the company
    z.addEmployee(x)
    z.addEmployee(y)
    //Loop for the UI
    while(running){
      println("SCALA TEST\nPick an option:\n1: Add Employee\n2: Add Specialist\n3: Raise Salary\n4: Give Bonus\n5: List employees\n6: Exit")
      val x = scala.io.StdIn.readInt()
      x match {
        case 1 => println("Enter First Name: ")
                  fName = scala.io.StdIn.readLine()
                  println("Enter Last Name: ")
                  lName = scala.io.StdIn.readLine()
                  println("Enter Salary: ")
                  salary = scala.io.StdIn.readDouble()
                  z.addEmployee(new Employee(fName, lName, salary))
                  println("Employee added!")
        case 2 => println("Enter First Name: ")
                  fName = scala.io.StdIn.readLine()
                  println("Enter Last Name: ")
                  lName = scala.io.StdIn.readLine()
                  println("Enter Salary: ")
                  salary = scala.io.StdIn.readDouble()
                  println("Enter Bonus: ")
                  bonus = scala.io.StdIn.readDouble()
                  z.addEmployee(new Specialist(fName, lName, salary, bonus))
                  println("Specialist added!")
        case 3 => z.raisePay()
                  println("Salary raised!")
        case 4 => z.giveBonus()
                  println("Bonus given!")
        case 5 => println(z.toString)
        case 6 => running = false
        case _ => null
      }
    }
  }
}
