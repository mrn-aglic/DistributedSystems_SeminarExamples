package comparison_examples.futuresimplementation


import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

/**
  * Created by Marin on 28/04/2017.
  */
object MatrixMultiplication {

	import common.MeasurementHelpers._
	import common.{Configuration, MeasurementHelpers}

	val r = new Random()

	def main(args: Array[String]): Unit = {

		val n: Double = Configuration.runTimes

		val runNTimes = MeasurementHelpers.runNTimes(n.toInt) _

		val results = runNTimes {

			val nrow = Configuration.numberOfRows
			val ncol = Configuration.numberOfCols

			setNumThreads(Array.ofDim[Long](nrow))

			// stvorimo prvu matricu
			val firstMatrix = createMatrix(nrow, ncol)
			// stvorimo drugu matricu
			val secondMatrix = createMatrix(nrow, ncol)

			val transposedMatrix = transpose(secondMatrix)

			val f = matrixMultiply(nrow, ncol, firstMatrix, transposedMatrix)

			Await.result(f, Duration.Inf)

			//println(getThreads.length)
			//println(getDistinctThreads.length)
		}

		// ispišemo prosječnu brzinu izvršavanja
		println(s"Average duration: ${results.map(x => x._2).sum / n}")
		// ispišemo prosječan broj niti
		println(s"Average number of distinct threads: ${results.map(x => x._3.length).sum / n}")
		println()
	}

	def matrixMultiply(nrow: Int, ncol: Int, firstMatrix: List[List[Int]], secondMatrix: List[List[Int]]): Future[List[List[Int]]] = {

		val seqOfFutures = firstMatrix.zipWithIndex.map(x => {

			multiplyRowByMatrix(x._2, x._1, secondMatrix)
		})

		val allFutures = Future.sequence(seqOfFutures)

		allFutures
	}

	def multiplyRowByMatrix(index: Int, row: List[Int], secondMatrix: List[List[Int]]): Future[List[Int]] = Future {

		addCurrentThread(index)

		secondMatrix.map(x => multiplyRows(row, x))
	}

	def multiplyRows(r1: List[Int], r2: List[Int]): Int =
		r1.zip(r2).map { case (x, y) => x * y }.sum

	// pomoćna metoda za stvaranje matrica
	def createMatrix(row: Int, col: Int): List[List[Int]] =
		(1 to row).map(x => (1 to col).map(_ => r.nextInt(10)).toList).toList

	def transpose(m: List[List[Int]]): List[List[Int]] =
		if (m.head.isEmpty) Nil
		else m.map(_.head) :: transpose(m.map(_.tail))

}
