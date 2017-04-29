package singlethreaded

import scala.util.Random

/**
  * Created by Marin on 28/04/2017.
  */
object MatrixMultiplication_simple {

	import common.MeasurementHelpers
	import common.Configuration

	val r = new Random()

	def main(args: Array[String]): Unit = {

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

	// pomoćna metoda za stvaranje matrica
	def createMatrix(row: Int, col: Int): List[List[Int]] =
		(1 to row).map(x => (1 to col).map(_ => r.nextInt(10)).toList).toList

	def doWork(nrow: Int, ncol: Int, firstMatrix: List[List[Int]], secondMatrix: List[List[Int]])(): Unit = {

		val result = Array.ofDim[List[Int]](nrow)

		var threads = List[Thread]()

		for (i <- 0 until nrow) {

			val row = firstMatrix(i)

			MeasurementHelpers.addCurrentThread()

			var newRow = List[Int]()

			for (j <- 0 until nrow) {

				var sum = 0

				for (z <- 0 until ncol) {

					sum += row(z) * secondMatrix(z)(j)
				}

				newRow = newRow :+ sum
			}

			result.update(i, newRow)
		}

		/*println("result")
		result.foreach(println)*/
	}

	def thread(block: => Unit): Thread = {

		val t = new Thread {

			override def run(): Unit = {

				block
			}
		}

		t.start()

		t
	}
}
