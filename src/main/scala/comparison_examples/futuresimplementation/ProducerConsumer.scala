package comparison_examples.futuresimplementation

import common.MeasurementHelpers._
import common._

import scala.collection.mutable
import scala.concurrent.blocking
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}

case class Item(value: Int)

object ProducerConsumer {

	private val sharedQueue = mutable.Queue[Item]()

	def main(args: Array[String]): Unit = {

		val n: Double = if (args.nonEmpty) args(0).toDouble else Configuration.runTimes.toDouble
		val numConsumers = if (args.length == 2) args(1).toInt else Configuration.numberOfConsumers

		val funRunNTimes = MeasurementHelpers.runNTimes(n.toInt) _

		val results = funRunNTimes {

			val p = Promise[Boolean]()

			setNumThreads(Array.ofDim[Long](numConsumers))

			val producer = new Producer(p, sharedQueue)
			val cs = startConsumers(numConsumers, List[Consumer](), sharedQueue, p)

			val fp = producer.start()
			val fs = cs.map(x => x.start())

			val f = Future.sequence(fs)

			Await.result(fp, Duration.Inf)
			Await.result(f, Duration.Inf)

			val allElementsObtained = cs.flatMap(_.getObtainedItems)

			println(allElementsObtained.length)
		}

		println(s"Run times: $n")
		println(s"Average number of distinct threads: ${results.map(x => x._3.length).sum / n}")
		println(s"Average duration: ${results.map(x => x._2).sum / n} milliseconds")

		println()
	}

	def startConsumers(max: Int, result: List[Consumer], sharedQueue: mutable.Queue[Item], p: Promise[Boolean]): List[Consumer] =
		if (max == 0) result
		else {
			val consumer = new Consumer(max - 1, p, sharedQueue)

			startConsumers(max - 1, consumer :: result, sharedQueue, p)
		}
}

class Producer(p: Promise[Boolean], sharedQueue: mutable.Queue[Item]) {

	def start(): Future[Unit] = Future {

		val n = Configuration.workToProduce

		for (i <- 1 to n) {

			val item = Item(i)

			// pristup dijeljenom resursu mora biti sinkroniziran
			sharedQueue.synchronized {

				sharedQueue.enqueue(item)
				sharedQueue notify()
			}
		}

		p success true

		sharedQueue.synchronized {

			sharedQueue.notifyAll()
		}
	}
}

class Consumer(index:Int, p: Promise[Boolean], sharedQueue: mutable.Queue[Item]) {

	private var obtainedItems = List[Item]()

	def getObtainedItems: List[Item] = obtainedItems

	def start(): Future[Unit] = Future {

		addCurrentThread(index)

		while (sharedQueue.nonEmpty || !p.isCompleted) {

			obtainedItems = getItem match {

				case None => obtainedItems
				case Some(item) => item :: obtainedItems
			}
		}
	}

	def getItem: Option[Item] = blocking {

		sharedQueue.synchronized {

			while (sharedQueue.isEmpty && !p.isCompleted) {

				sharedQueue.wait()
			}

			val result = if (sharedQueue.nonEmpty)
				Some(sharedQueue.dequeue())
			else
				None

			result
		}
	}


	def printObtainedItems(): Unit =
		obtainedItems.foreach(x => print(s"\t ${x.value}"))
}