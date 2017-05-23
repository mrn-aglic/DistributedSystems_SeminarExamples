package comparison_examples.threadImplementations

import common.MeasurementHelpers._
import common._

import scala.collection.mutable

case class Item(value: Int)

object ProducerConsumer {

	private val sharedQueue = mutable.Queue[Item]()

	var timesOfStartingThreads: List[Long] = List[Long]()
	var timesOfCreatingAllConsumers: List[Long] = List[Long]()

	def main(args: Array[String]): Unit = {

		val n: Double = if(args.nonEmpty) args(0).toDouble else Configuration.runTimes.toDouble
		val numConsumers = if(args.length == 2) args(1).toInt else Configuration.numberOfConsumers

		val funRunNTimes = MeasurementHelpers.runNTimes(n.toInt) _

		setNumThreads(Array.ofDim[List[Long]](numConsumers))

		val results = funRunNTimes {

			Helper.isFinished = false

			val producer = new Producer(sharedQueue)

			val now = System.currentTimeMillis()
			val cs = startConsumers(numConsumers, List[Consumer](), sharedQueue)
			val cur = System.currentTimeMillis()

			timesOfCreatingAllConsumers = (cur - now ) :: timesOfCreatingAllConsumers

			cs.foreach(_.join())
			producer.join()

			println(s"Total items consumed: ${cs.map(x => x.getObtainedItems.length).sum}")
		}

		println(s"Run times: $n")
		println(s"Average duration: ${results.map(x => x._2).sum / n} milliseconds")

		println(s"Time of starting threads: ${timesOfStartingThreads.sum / n}")
		println(s"Time of creating consumers: ${timesOfCreatingAllConsumers.sum / n}")

		println(results.map(x => x._3.length).sum / n)

		println()
	}

	def startConsumers(max: Int, result: List[Consumer], sharedQueue: mutable.Queue[Item]): List[Consumer] =
		if (max == 0) result
		else {
			val consumer = new Consumer(max - 1, sharedQueue)
			val now = System.currentTimeMillis()
			consumer.start()
			val cur = System.currentTimeMillis()

			timesOfStartingThreads = (cur - now)::timesOfStartingThreads

			startConsumers(max - 1, consumer :: result, sharedQueue)
		}
}

object Helper {
	var isFinished = false
}

class Producer(sharedQueue: mutable.Queue[Item]) extends Thread {

	override def run(): Unit = {

		val n = Configuration.workToProduce

		for (i <- 1 to n) {

			val item = Item(i)

			// pristup dijeljenom resursu mora biti sinkroniziran
			sharedQueue.synchronized {

				sharedQueue.enqueue(item)
				sharedQueue.notify()
			}
		}

		sharedQueue.synchronized {

			Helper.isFinished = true
			sharedQueue.notifyAll()
		}
	}

	start()
}

class Consumer(index: Int, sharedQueue: mutable.Queue[Item]) extends Thread {

	private var obtainedItems = List[Item]()

	def getObtainedItems: List[Item] = obtainedItems

	override def run(): Unit = {

		addCurrentThread(index)

		while (sharedQueue.nonEmpty || !Helper.isFinished) {

			obtainedItems = getItem match {

				case None => obtainedItems
				case Some(item) => item :: obtainedItems
			}
		}
	}

	def getItem: Option[Item] =
	// pristup dijeljenom resurse (redu) mora biti sinkroniziran
		sharedQueue.synchronized {

			while (sharedQueue.isEmpty && !Helper.isFinished) {

				sharedQueue.wait()
			}

			if (sharedQueue.nonEmpty)
				Some(sharedQueue.dequeue())
			else
				None
		}

	def printObtainedItems(): Unit =
		obtainedItems.foreach(x => print(s"\t ${x.value}"))
}