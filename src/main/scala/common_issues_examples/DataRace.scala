package common_issues_examples

/**
  * Created by Marin on 11/04/2017.
  */
class Account(var balance: Int)

object DataRace {

  def main(args: Array[String]): Unit = {

    val xs = (1 to 100).par

    var sum = 0

    xs.foreach(x => {

      sum = sum + x
    })

    println(s"sum is: $sum")
    println(s"should be: ${(1 to 100).sum}")
  }
}
