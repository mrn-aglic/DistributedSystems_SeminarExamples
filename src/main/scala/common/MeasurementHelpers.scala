package common

import java.util.Date

/**
  * Created by Marin on 28/04/2017.
  */
object MeasurementHelpers {

	private var _numThreads = List[Long]()

	def addCurrentThread(): Unit = _numThreads = Thread.currentThread().getId :: _numThreads

	def numThread(): List[Long] = _numThreads

	def getDistinctThreads: List[Long] = _numThreads.distinct

	def getAndClearThreadsList(): List[Long] = {

		val temp = _numThreads
		_numThreads = List[Long]()

		temp
	}

	def addCurrentThreadWith[A](block: => A): A = {

		addCurrentThread()
		block
	}

	def time[A](block: => A): (Long, A) = {

		val now = System.currentTimeMillis()

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
			val (duration, _) = time(block)
			val distinctThreads = getAndClearThreadsList()

		} yield (i, duration, distinctThreads)
	}.toList
}
