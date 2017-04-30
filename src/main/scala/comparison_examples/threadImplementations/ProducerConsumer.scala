package comparison_examples.threadImplementations

import common.MeasurementHelpers._
import common.ThreadHelpers.thread
import common._

import scala.collection.mutable

/**
  * Created by Marin on 29/04/2017.
  */
object ProducerConsumer {

	private val sharedQueue = mutable.Queue[Item]()

	def main(args: Array[String]): Unit = {

		var consumers: Option[List[Consumer]] = None

		val (duration, _) = MeasurementHelpers.time {

			val producer = producerWork()
			consumers = Some(startConsumers(10, List[Consumer](), sharedQueue))

			consumers.get.foreach(_.join())
			producer.join()
		}

		println(s"Sum of all obtained items ${consumers.get.map(x => x.getObtainedItems.length).sum}")

		println(s"Duration: $duration milliseconds")

		println(s"number of distinct threads: ${getDistinctThreads.length}")
		println("all distinct threads: ")
		getDistinctThreads.foreach(x => print(s"$x\t"))
		println()

		println("Consumers gained values")
		consumers.get.foreach(x => println(s"${x.getObtainedItems.length}"))
		println()
	}

	def producerWork(): Thread = thread {

		for (i <- 1 to 500) {

			val item = Item(i)

			sharedQueue.synchronized {

				sharedQueue.enqueue(item)
				sharedQueue.notifyAll()
			}
		}
	}

	def startConsumers(max: Int, result: List[Consumer], sharedQueue: mutable.Queue[Item]): List[Consumer] =
		if(max == 0) result
		else {

			val consumer = new Consumer(sharedQueue)

			consumer.start()

			startConsumers(max - 1, consumer :: result, sharedQueue)
		}
}

case class Item(value: Int)

class Consumer(sharedQueue: mutable.Queue[Item]) extends Thread {

	private var obtainedItems = List[Item]()

	def getObtainedItems: List[Item] = obtainedItems

	override def run(): Unit = {

		addCurrentThread()

		while(obtainedItems.length < 50) {

			obtainedItems = getItem :: obtainedItems
		}
		//println(obtainedItems)
	}

	def getItem: Item =
		sharedQueue.synchronized {

			while (sharedQueue.isEmpty) {

				sharedQueue.wait()
			}

			sharedQueue.dequeue()
		}

	def printObtainedItems(): Unit =
		obtainedItems.foreach(x => print(s"\t ${x.value}"))
}