package comparison_examples.threadImplementations

import common.MeasurementHelpers._
import common._

import scala.collection.mutable

case class Item(value: Int)

object ProducerConsumer {

	private val sharedQueue = mutable.Queue[Item]()

	//var xs: List[Long] = List[Long]()

	def main(args: Array[String]): Unit = {

		val n: Double = if(args.nonEmpty) args(0).toDouble else Configuration.runTimes.toDouble
		val numConsumers = if(args.length == 2) args(1).toInt else Configuration.numberOfConsumers

		//var ls = List[Long]()

		val funRunNTimes = MeasurementHelpers.runNTimes(n.toInt) _

		val results = funRunNTimes {

			Helper.isFinished = false

			val producer = new Producer(sharedQueue)

			val now = System.currentTimeMillis()

			val cs = startConsumers(numConsumers, List[Consumer](), sharedQueue)

			val cur = System.currentTimeMillis()

			//ls = (cur - now) :: ls

			cs.foreach(_.join())
			producer.join()

			println(s"Total items consumed: ${cs.map(x => x.getObtainedItems.length).sum}")
		}

		println(s"Run times: $n")
		println(s"Average duration: ${results.map(x => x._2).sum / n} milliseconds")

		//println(s"average time for creating consumers: ${ls.sum / n} milliseconds")
		//println(s"${xs.length}")
		//println(s"average time for starting a thread: ${xs.sum / n} milliseconds")

		println()
	}

	def startConsumers(max: Int, result: List[Consumer], sharedQueue: mutable.Queue[Item]): List[Consumer] =
		if (max == 0) result
		else {
			val consumer = new Consumer(sharedQueue)
			//val now = System.currentTimeMillis()
			consumer.start()
			//val cur = System.currentTimeMillis()

			//xs = (cur - now) :: xs
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

			if (sharedQueue.nonEmpty)
				Some(sharedQueue.dequeue())
			else
				None
		}

	def printObtainedItems(): Unit =
		obtainedItems.foreach(x => print(s"\t ${x.value}"))
}