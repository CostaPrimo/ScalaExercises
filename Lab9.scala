object Lab9 {
  class Consumer(account: Account) extends  Runnable{
    private var core = account
    override def run(): Unit = {
      for (x <- 1 to 10) {
        core.widraw(5)
      }
    }
  }

  class Provider(account: Account) extends Runnable {
    private var core = account
    override def run(): Unit = {
      for (x <- 1 to 10) {
        core.deposit(10)
      }
    }
  }

  class Account() {
    private var balance = 100
    var core = this

    def getInstance(): Account = {
      core
    }

    def widraw(amount: Int): Unit = {
      synchronized {
        println("Balance prior to widrawal: " + balance)
        balance = balance - amount
        println("Balance after widrawal: " + balance)
      }
    }

    def deposit(amount: Int): Unit = {
      synchronized {
        println("Balance prior to deposit: " + balance)
        balance = balance + amount
        println("Balance after deposit: " + balance)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val account = new Account
    val th1 = new Thread(new Consumer(account))
    val th2 = new Thread(new Consumer(account))
    val th3 = new Thread(new Consumer(account))
    val th4 = new Thread(new Provider(account))
    th1.start()
    th2.start()
    th3.start()
    th4.start()
  }
}
