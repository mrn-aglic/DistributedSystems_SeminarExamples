package common

import java.util.Date

/**
  * Created by Marin on 28/04/2017.
  */
object MeasurementHelpers {

	private var _numThreadsPerCreated = Array.ofDim[List[Long]](0)

	def setNumThreads(arr: Array[List[Long]]): Unit = {

		_numThreadsPerCreated = arr
		_numThreadsPerCreated.zipWithIndex.foreach(x => _numThreadsPerCreated.update(x._2, List[Long]()))

		println("init complete")
	}

	def addCurrentThread(index: Int): Unit = {

		val newValue = Thread.currentThread().getId :: _numThreadsPerCreated(index)
		_numThreadsPerCreated.update(index, newValue)
	}

	def getDistinctThreads: List[Long] = _numThreadsPerCreated.flatten.toList.distinct

	private var _numThreads: Array[Long] = Array.ofDim[Long](0)

	def getAndClearThreadsList(): List[Long] = {

		val temp = getDistinctThreads
		setNumThreads(_numThreadsPerCreated)

		//println(temp.distinct.length)
		temp.distinct
	}

	def time[A](block: => A, num: Option[Int]): (Long, A) = {

		val now = System.currentTimeMillis()

		num match {

			case None =>
			case Some(x) => print(s"run number: $x, ")
		}
		println(s"start time: ${new Date(now)}")

		val result = block

		val end = System.currentTimeMillis()

		println(s"end time: ${new Date(end)}")

		val duration = end - now

		(duration, result)
	}

	def runNTimes[A](n: Int)(block: => A): List[(Int, Long, List[Long])] = {

		for{
			i <- 1 to n
			val (duration, _) = time(block, Some(i))
			val distinctThreads = getAndClearThreadsList()

		} yield (i, duration, distinctThreads)
	}.toList
}
