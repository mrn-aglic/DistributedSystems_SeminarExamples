package common_issues_examples

/**
  * Created by Marin on 12/04/2017.
  */
object Deadlock {

  import DeadlockExample._

  def main(args: Array[String]): Unit = {

    val a = new Component(1)
    val b = new Component(2)

    var t1 = thread { doComplexWork(a, b) }
    var t2 = thread { doComplexWork(b, a) }

    t1.join()
    t2.join()

    println(s"Work is done!")
  }

  def thread(body: => Unit): Thread = {

    val t = new Thread{

      override def run(): Unit = body
    }

    t.start()

    t
  }
}

class Component(id: Int) {

  def doWork(): Unit = println(s"$id doing work")
}

object DeadlockExample {

  def doComplexWork(a: Component, b: Component): Unit = a.synchronized {

    a.doWork()

    b.synchronized {

      b.doWork()
    }
  }
}