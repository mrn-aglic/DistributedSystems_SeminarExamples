package comparison_examples.actorsimplementation

/**
  * Created by Marin on 19/05/2017.
  */

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props, Terminated}
import akka.pattern.ask
import akka.routing.{Broadcast, RoundRobinGroup, TailChoppingGroup}
import akka.util.Timeout
import common._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}


object ProducerActor {

	def apply(): Props = Props(new ProducerActor())

	case class Start(max: Int, pool: ActorRef)

	case class Produce(index: Int)
}

object ConsumerActor {

	import Common._

	def apply(index: Int): Props = Props(new ConsumerActor(index))

	case class ObtainedItems(items: List[Item])

	case object SendObtained
}

object RouterProps {

	def apply(paths: List[String]): Props = RoundRobinGroup(paths).props()
}

object Common {

	case class Item(value: Int)

	case object Finished

}

object ProducerConsumer {

	import ConsumerActor._
	import ProducerActor._

	def main(args: Array[String]): Unit = {

		implicit val timeout = Timeout(10 seconds)

		val n: Double = if (args.nonEmpty) args(0).toDouble else Configuration.runTimes.toDouble
		val numConsumers = if (args.length == 2) args(1).toInt else Configuration.numberOfConsumers

		val funRunNTimes = MeasurementHelpers.runNTimes(n.toInt) _

		val system = ActorSystem("producer-consumer")

		MeasurementHelpers.setNumThreads(Array.ofDim[List[Long]](numConsumers))

		val results = funRunNTimes {

			val producer = system.actorOf(ProducerActor())

			val consumers = for (i <- 0 until numConsumers) yield system.actorOf(ConsumerActor(i))

			val paths = consumers.map(_.path.toString).toList

			val router = system.actorOf(RouterProps(paths))

			val f = producer ? Start(Configuration.workToProduce, router)

			val obtainedF = f flatMap (_ => {

				val z = consumers.map(x => (x ? SendObtained).mapTo[ObtainedItems].map(x => x.items))

				Future.sequence(z).map(x => x.flatten)
			})

			obtainedF onComplete {

				case Success(x) =>

					println(s"Number of all elements obtained: ${x.length}")

					router ! Broadcast(PoisonPill)
				case Failure(err) => println(s"An error occurred: $err")
			}

			Await.result(obtainedF, Duration.Inf)

			router ! PoisonPill
		}


		println(s"Run times: $n")
		println(s"Average number of distinct threads: ${results.map(x => x._3.length).sum / n}")
		println(s"Average duration: ${results.map(x => x._2).sum / n} milliseconds")

		println()

		system.terminate().onComplete {

			case Success(_) => println("Shutdown complete")
			case Failure(err) => println(err)
		}
	}
}

class ProducerActor extends Actor {

	import Common._
	import ProducerActor._

	var respondToTopLevel: ActorRef = _

	def receive: Receive = {

		case Start(max, pool) =>

			println("starting...")

			respondToTopLevel = sender()
			context.become(produceWork(max, pool))

			self ! Produce(0)
	}

	def produceWork(max: Int, pool: ActorRef): Receive = {

		case Produce(index) if index < max =>

			val item = Item(index)

			pool ! item

			self ! Produce(index + 1)

		case Produce(index) =>

			respondToTopLevel ! Finished

			self ! PoisonPill
	}
}

class ConsumerActor(index: Int) extends Actor {

	import Common._
	import ConsumerActor._

	private var obtainedItems = List[Item]()

	def receive: Receive = {

		case msg@Item(x) =>
			
			MeasurementHelpers.addCurrentThread(index)

			obtainedItems = msg :: obtainedItems

		case SendObtained =>

			sender() ! ObtainedItems(obtainedItems)
	}
}
