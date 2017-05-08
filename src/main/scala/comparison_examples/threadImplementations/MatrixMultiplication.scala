package comparison_examples.threadImplementations

import scala.concurrent.Future
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

    def main(args: Array[String]): Unit = {

	    val n: Double = Configuration.runTimes

	    val runNTimes = MeasurementHelpers.runNTimes(n.toInt) _

	    val results = runNTimes {

		    val nrow = Configuration.numberOfRows
		    val ncol = Configuration.numberOfCols

		    // stvorimo prvu matricu
		    val firstMatrix = createMatrix(nrow, ncol)
		    // stvorimo drugu matricu
		    val secondMatrix = createMatrix(nrow, ncol)

		    // posao koji se želi odraditi: Množenje matrica
		    matrixMultiply(nrow, ncol, firstMatrix, secondMatrix)
	    }

	    // ispišemo prosječnu brzinu izvršavanja
		println(s"Average duration: ${results.map(x => x._2).sum / n}")
	    // ispišemo prosječan broj niti

	    println(s"Average number of distinct threads: ${results.map(x => x._3.length).sum / n}")

	    println()
    }

	def matrixMultiply(nrow: Int, ncol: Int, firstMatrix: List[List[Int]], secondMatrix: List[List[Int]]): Unit = {

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
	}
}
