package comparison_examples.actorsimplementation

import akka.actor.{Actor, ActorSystem, PoisonPill, Props}
import common.Types.{Location, Matrix, RowNumber}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

/**
  * Created by Marin on 28/04/2017.
  */
object MatrixMultiplication {

	import common.{Configuration, MeasurementHelpers}

	val r = new Random()

	def main(args: Array[String]): Unit = {

		var system = ActorSystem.create("matrix-multiplication")


		val n: Double = Configuration.runTimes

		val runNTimes = MeasurementHelpers.runNTimes(n.toInt) _

		val results = runNTimes {

			val nrow = Configuration.numberOfRows
			val ncol = Configuration.numberOfCols

			// stvorimo prvu matricu
			val firstMatrix = createMatrix(nrow, ncol)
			// stvorimo drugu matricu
			val secondMatrix = createMatrix(nrow, ncol)

		}

		// ispišemo prosječnu brzinu izvršavanja
		println(s"Average duration: ${results.map(x => x._2).sum / n}")
		// ispišemo prosječan broj niti
		println(s"Average number of distinct threads: ${results.map(x => x._3.length).sum / n}")
		println()
	}

	// pomoćna metoda za stvaranje matrica
	def createMatrix(row: Int, col: Int): List[List[Int]] =
		(1 to row).map(x => (1 to col).map(_ => r.nextInt(10)).toList).toList

}

case class Matrices(m1: Matrix[Int], m2: Matrix[Int])

case class MultiplyRowsByMatrix(i: RowNumber, row: List[Int], matrix: Matrix[Int])

case class RowsMultiplicationResult(i: RowNumber, values: List[Int])

case object PrintResult

object PropsConfigurations {

	def matrixProps(): Props = Props[MatrixActor]

	def multiplierProps(): Props = Props[Multiplier]
}

class MatrixActor extends Actor {

	import PropsConfigurations.multiplierProps

	var matrix: Array[List[Int]] = _
	var numberOfReceivedResponses = 0

	def transpose(m: Matrix[Int]): Matrix[Int] =
		if (m.head.isEmpty) Nil
		else m.map(_.head) :: transpose(m.map(_.tail))

	def receive: Receive = {

		case Matrices(m1, m2) =>


			val transposed = transpose(m2)

			matrix = Array.ofDim[List[Int]](m1.length)

			m1.zipWithIndex.foreach { case (row, i) =>

				val child = context.actorOf(multiplierProps())
				child ! MultiplyRowsByMatrix(i, row, m2)
			}
		case RowsMultiplicationResult(i, row) =>

			matrix(i) = row
			numberOfReceivedResponses = numberOfReceivedResponses + 1

			if (numberOfReceivedResponses == matrix.length) self ! PrintResult
		case PrintResult =>

			matrix.foreach(println)
	}
}

class Multiplier extends Actor {

	def receive: Receive = {

		case MultiplyRowsByMatrix(i, row, matrix) =>

			val result = matrix.map(x => multiplyRows(row, x))

			sender ! RowsMultiplicationResult(i, result)

			self ! PoisonPill
	}

	def multiplyRows(r1: List[Int], r2: List[Int]): Int =
		r1.zip(r2).map { case (x, y) => x * y }.sum
}
