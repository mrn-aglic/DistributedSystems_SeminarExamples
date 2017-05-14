package comparison_examples.threadImplementations

import scala.concurrent.Future
import scala.util.Random

/**
  * Created by Marin on 28/04/2017.
  */
object MatrixMultiplication_Functional {

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

			val transposedMatrix = transpose(secondMatrix)

			// posao koji se želi odraditi: Množenje matrica
			matrixMultiply(nrow, ncol, firstMatrix, transposedMatrix)
		}

		// ispišemo prosječnu brzinu izvršavanja
		println(s"Average duration: ${results.map(x => x._2).sum / n}")
		// ispišemo prosječan broj niti

		println(s"Average number of distinct threads: ${results.map(x => x._3.length).sum / n}")

		println()
	}

	def matrixMultiply(nrow: Int, ncol: Int, firstMatrix: List[List[Int]], secondMatrix: List[List[Int]]): List[List[Int]] = {

		var result = Array.ofDim[List[Int]](nrow)

		MeasurementHelpers.setNumThreads(Array.ofDim[Long](nrow))

		val threads = firstMatrix.zipWithIndex.map { case(x, index) => thread {

			MeasurementHelpers.addCurrentThread(index)

			result.update(index, multiplyRowByMatrix(x, secondMatrix))
		}}

		threads.foreach(_.join())

		result.toList
	}

	def multiplyRowByMatrix(row: List[Int], secondMatrix: List[List[Int]]): List[Int] =  {

		secondMatrix.map(x => multiplyRows(row, x))
	}

	def multiplyRows(r1: List[Int], r2: List[Int]): Int =
		r1.zip(r2).map { case (x, y) => x * y }.sum

	def transpose(m: List[List[Int]]): List[List[Int]] =
		if(m.head.isEmpty) Nil
		else m.map(_.head) :: transpose(m.map(_.tail))
}
