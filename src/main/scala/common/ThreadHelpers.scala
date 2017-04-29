package common

/**
  * Created by Marin on 29/04/2017.
  */
object ThreadHelpers {

	def thread(block: => Unit): Thread = {

		val t = new Thread{

			override def run(): Unit = {

				block
			}
		}

		t.start()

		t
	}
}
