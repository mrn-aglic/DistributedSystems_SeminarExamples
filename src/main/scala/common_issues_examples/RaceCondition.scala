package common_issues_examples

/**
  * Created by Marin on 10/04/2017.
  */
object RaceCondition {

  def main(args: Array[String]): Unit = {

    var x: ExpensiveEntity = null
    var y: ExpensiveEntity = null

    val t1 = thread { x = Singleton.getInstance() }
    val t2 = thread { y = Singleton.getInstance() }

    t1.start()
    t2.start()

    t1.join()
    t2.join()

    println(s"do x and y reference the same object: ${x eq y}")
  }

  def thread(body: => Unit): Thread = new Thread{

    override def run(): Unit = body
  }
}

class ExpensiveEntity {

  val content = "some expensive content"
}

object Singleton {

  var instance: ExpensiveEntity = null

  def getInstance(): ExpensiveEntity = {

    if (instance == null) {

      println(s"Current thread id: ${Thread.currentThread().getId}")

      instance = new ExpensiveEntity
    }

    instance
  }
}
