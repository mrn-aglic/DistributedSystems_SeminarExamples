package comparison_examples.actorsimplementation

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.pattern.ask
import akka.util.Timeout
import common.MeasurementHelpers
import common.Types.{Matrix, RowNumber}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}

/**
  * Created by Marin on 28/04/2017.
  */
case class Matrices(m1: Matrix[Int], m2: Matrix[Int])

case class MultiplyRowsByMatrix(i: RowNumber, row: List[Int], matrix: Matrix[Int])

case class RowsMultiplicationResult(i: RowNumber, values: List[Int])

case class Result(matrix: Matrix[Int])

object PropsConfigurations {

	def matrixProps(): Props = Props[Distributer]

	def multiplierProps(): Props = Props[Multiplier]
}

object MatrixMultiplication {

	import PropsConfigurations._
	import common.{Configuration, MeasurementHelpers}

	val r = new Random()

	def main(args: Array[String]): Unit = {

		implicit val timeout = Timeout(10 seconds)

		var system = ActorSystem.create("matrix-multiplication")

		val n: Double = Configuration.runTimes

		val runNTimes = MeasurementHelpers.runNTimes(n.toInt) _

		MeasurementHelpers.setNumThreads(Array.ofDim[List[Long]](Configuration.numberOfRows))

		val results = runNTimes {

			val matrixActor = system.actorOf(matrixProps())

			val nrow = Configuration.numberOfRows
			val ncol = Configuration.numberOfCols

			// stvorimo prvu matricu
			val firstMatrix = createMatrix(nrow, ncol)
			// stvorimo drugu matricu
			val secondMatrix = createMatrix(nrow, ncol)

			val f = matrixActor ? Matrices(firstMatrix, secondMatrix)

			f.onComplete {

				case Success(x) => matrixActor ! PoisonPill
				case Failure(er) =>
			}

			Await.result(f, Duration.Inf)
		}

		// ispišemo prosječnu brzinu izvršavanja
		println(s"Average duration: ${results.map(x => x._2).sum / n}")
		// ispišemo prosječan broj niti
		println(s"Average number of distinct threads: ${results.map(x => x._3.length).sum / n}")
		println()

		system.terminate()
	}

	// pomoćna metoda za stvaranje matrica
	def createMatrix(row: Int, col: Int): List[List[Int]] =
		(1 to row).map(x => (1 to col).map(_ => r.nextInt(10)).toList).toList

}

class Distributer extends Actor {

	import PropsConfigurations.multiplierProps

	var matrix: Array[List[Int]] = _
	var numberOfReceivedResponses = 0
	var respondToTopLevel: ActorRef = _

	def transpose(m: Matrix[Int]): Matrix[Int] =
		if (m.head.isEmpty) Nil
		else m.map(_.head) :: transpose(m.map(_.tail))

	def receive: Receive = {

		case Matrices(m1, m2) =>

			respondToTopLevel = sender

			val transposed = transpose(m2)

			matrix = Array.ofDim[List[Int]](m1.length)

			m1.zipWithIndex.foreach { case (row, i) =>

				val child = context.actorOf(multiplierProps())
				child ! MultiplyRowsByMatrix(i, row, transposed)
			}
		case RowsMultiplicationResult(i, row) =>

			matrix(i) = row
			numberOfReceivedResponses = numberOfReceivedResponses + 1

			sender ! PoisonPill

			if (numberOfReceivedResponses == matrix.length) {

				respondToTopLevel ! Result(matrix.toList)
			}
	}
}

class Multiplier extends Actor {

	def receive: Receive = {

		case MultiplyRowsByMatrix(i, row, matrix) =>

			MeasurementHelpers.addCurrentThread(i)

			val result = matrix.map(x => multiplyRows(row, x))

			sender ! RowsMultiplicationResult(i, result)
	}

	def multiplyRows(r1: List[Int], r2: List[Int]): Int =
		r1.zip(r2).map { case (x, y) => x * y }.sum
}
