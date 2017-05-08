package comparison_examples.futuresimplementation

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Random, Success, Failure}

/**
  * Created by Marin on 28/04/2017.
  */
object MatrixMultiplication {

	import common.{Configuration, MeasurementHelpers}

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

		    firstMatrix.foreach(println)
		    println("------------------------")
		    secondMatrix.foreach(println)

		    // posao koji se želi odraditi: Množenje matrica
			matrixMultiply(nrow, ncol, firstMatrix, secondMatrix) onComplete {

				case Success(matrix) => matrix.foreach(x => println(s"$x"))
				case Failure(x) => println(s"failed matrix multiplication...${x.getMessage}")
			}

		    //val f = matrixMultiply(nrow, ncol, firstMatrix, secondMatrix)

		    //Await.result(f, 5 seconds )
	    }

	    // ispišemo prosječnu brzinu izvršavanja
		println(s"Average duration: ${results.map(x => x._2).sum / n}")
	    // ispišemo prosječan broj niti

	    println(s"Average number of distinct threads: ${results.map(x => x._3.length).sum / n}")

	    println()
    }

	def matrixMultiply(nrow: Int, ncol: Int, firstMatrix: List[List[Int]], secondMatrix: List[List[Int]]): Future[List[List[Int]]] = {

		val seqOfFutures = (0 until nrow).map(x => multiplyRowByMatrix(firstMatrix(x), secondMatrix)).toList

		val allFutures = Future.sequence(seqOfFutures)

		allFutures
	}

	def transpose(m: List[List[Int]]): List[List[Int]] =
		if(m.head.isEmpty) Nil
		else m.map(_.head) :: transpose(m.map(_.tail))

	def multiplyRowByMatrix(row: List[Int], secondMatrix: List[List[Int]]): Future[List[Int]] = Future {

		MeasurementHelpers.addCurrentThread()

		val nrow = row.length

		val newRow = (0 until nrow).map(index => (0 until nrow).map(r => secondMatrix(r)(index) * row(r)).sum)

		newRow.toList
	}
}
