package common

import java.util.Date

/**
  * Created by Marin on 28/04/2017.
  */
object MeasurementHelpers {

	private var _numThreads: Array[Long] = _

	def setNumThreads(arr: Array[Long]): Unit = _numThreads = arr

	def addCurrentThread(index: Int): Unit = _numThreads.update(index, Thread.currentThread().getId)

	def numThread(): Array[Long] = _numThreads

	def getThreads: Array[Long] = _numThreads
	def getDistinctThreads: Array[Long] = _numThreads.distinct

	def getAndClearThreadsList(): Array[Long] = {

		val temp = getDistinctThreads
		_numThreads = Array[Long]()

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

	def runNTimes[A](n: Int)(block: => A): List[(Int, Long, Array[Long])] = {

		for{
			i <- 1 to n
			val (duration, _) = time(block, Some(i))
			val distinctThreads = getAndClearThreadsList()

		} yield (i, duration, distinctThreads)
	}.toList
}
