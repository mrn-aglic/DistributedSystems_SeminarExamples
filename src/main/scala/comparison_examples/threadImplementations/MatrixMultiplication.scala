package comparison_examples.threadImplementations

import java.util.Date

import scala.util.Random

/**
  * Created by Marin on 28/04/2017.
  */
object MatrixMultiplication {

	import common.MeasurementHelpers
	import common.Configuration
	import common.ThreadHelpers.thread

	val r = new Random()

	// pomoćna metoda za stvaranje matrica
    def createMatrix(row: Int, col: Int): List[List[Int]] =
	    (1 to row).map(x => (1 to col).map(_ => r.nextInt(10)).toList).toList

    def main(args: Array[String]): Unit ={

	    val nrow = Configuration.numberOfRows
	    val ncol = Configuration.numberOfCols

	    // stvorimo prvu matricu
		val firstMatrix = createMatrix(nrow, ncol)
	    // stvorimo drugu matricu
	    val secondMatrix = createMatrix(nrow, ncol)

	    /*println("first matrix")
	    firstMatrix.foreach(println)

	    println("second matrix")
	    secondMatrix.foreach(println)
		*/

	    val (duration, _) = MeasurementHelpers.time(doWork(nrow, ncol, firstMatrix, secondMatrix))

	    println(s"Duration: $duration milliseconds")

	    println(s"number of distinct threads: ${MeasurementHelpers.getDistinctThreads.length}")
	    println("all distinct threads: ")
	    MeasurementHelpers.getDistinctThreads.foreach(x => print(s"$x\t"))
	    println()
    }

	def doWork(nrow: Int, ncol: Int, firstMatrix: List[List[Int]], secondMatrix: List[List[Int]])(): Unit = {

		val result = Array.ofDim[List[Int]](nrow)

		var threads = List[Thread]()

		for(i <- 0 until nrow){

			val row = firstMatrix(i)

			// stvorimo po jednu nit za svaku red prve matrice
			val t = thread {

				MeasurementHelpers.addCurrentThread()

				var newRow = List[Int]()

				for (j <- 0 until nrow) {

					var sum = 0

					for(z <- 0 until ncol) {

						sum += row(z) * secondMatrix(z)(j)
					}

					newRow = newRow :+ sum
				}

				result.update(i, newRow)
			}

			threads = t :: threads
		}

		threads.foreach(_.join())

		/*println("result")
		result.foreach(println)*/
	}
}
