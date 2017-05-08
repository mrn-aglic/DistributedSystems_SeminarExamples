package singlethreaded

import scala.util.Random

/**
  * Created by Marin on 28/04/2017.
  */
object MatrixMultiplication {

	import common.MeasurementHelpers
	import common.Configuration

	val r = new Random()

    def createMatrix(row: Int, col: Int): List[List[Int]] =
	    (1 to row).map(x => (1 to col).map(_ => r.nextInt(10)).toList).toList

	def multiplyRows(r1: List[Int], r2: List[Int]): Int =
		r1.zip(r2).map { case (x, y) => x * y }.sum

	def transpose(m: List[List[Int]]): List[List[Int]] =
		if(m.head.isEmpty) Nil
		else m.map(_.head) :: transpose(m.map(_.tail))

    def main(args: Array[String]): Unit ={

	    val nrow = Configuration.numberOfRows
	    val ncol = Configuration.numberOfCols

		val firstMatrix = createMatrix(nrow, ncol)
	    val secondMatrix = createMatrix(nrow, ncol)

	    //val transposedSecondMatrix = transpose(secondMatrix)

	    /*println("first matrix")
	    firstMatrix.foreach(println)

	    println("second matrix")
	    secondMatrix.foreach(println)
		*/

	    val (duration, _) = MeasurementHelpers.time({

		//    println("transposed second matrix")
		//    transposedSecondMatrix.foreach(println)

		    val transposedSecondMatrix = transpose(secondMatrix)

		    val result = firstMatrix.map { x =>

			    transposedSecondMatrix.map(y => multiplyRows(x, y))
		    }
	    })

	    println(s"Duration: $duration milliseconds")

	    println(s"number of distinct threads: ${MeasurementHelpers.getDistinctThreads.length}")
	    println("all distinct threads: ")
	    MeasurementHelpers.getDistinctThreads.foreach(x => print(s"$x\t"))
	    println()

	    /*println("transposed second matrix")
	    transposedSecondMatrix.foreach(println)

	    val result = firstMatrix.map { x =>

		    transposedSecondMatrix.map(y => multyplyRows(x, y))
	    }

	    println("result")
	    result.foreach(println)*/
    }
}
