package comparison_examples.threadImplementations

import common.MeasurementHelpers._
import common.ThreadHelpers.thread
import common._
import comparison_examples.threadImplementations.ProducerConsumer.sharedQueue

import scala.collection.mutable

/**
  * Created by Marin on 29/04/2017.
  */

case class Item(value: Int)

object ProducerConsumer {

	private val sharedQueue = mutable.Queue[Item]()

	def main(args: Array[String]): Unit = {

		val n: Double = if(args.nonEmpty) args(0).toDouble else Configuration.runTimes.toDouble
		val numConsumers = if(args.length == 2) args(1).toInt else Configuration.numberOfConsumers

		val funRunNTimes = MeasurementHelpers.runNTimes(n.toInt) _

		var consumers: List[List[Int]] = List[List[Int]]()

		val results = funRunNTimes {

			Helper.isFinished = false

			val producer = new Producer(sharedQueue)
			val cs = startConsumers(numConsumers, List[Consumer](), sharedQueue)

			cs.foreach(_.join())
			producer.join()
		}

		println(s"Run times: $n")
		println(s"Average duration: ${results.map(x => x._2).sum / n} milliseconds")

		println()
	}

	def startConsumers(max: Int, result: List[Consumer], sharedQueue: mutable.Queue[Item]): List[Consumer] =
		if (max == 0) result
		else {
			val consumer = new Consumer(sharedQueue)

			consumer.start()

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
				sharedQueue.notifyAll()
			}
		}

		Helper.isFinished = true
	}

	start()
}

class Consumer(sharedQueue: mutable.Queue[Item]) extends Thread {

	private var obtainedItems = List[Item]()

	def getObtainedItems: List[Item] = obtainedItems

	override def run(): Unit = {

		addCurrentThread()

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

			sharedQueue.notifyAll()

			if (sharedQueue.nonEmpty)
				Some(sharedQueue.dequeue())
			else
				None
		}

	def printObtainedItems(): Unit =
		obtainedItems.foreach(x => print(s"\t ${x.value}"))
}