import scala.collection.immutable.Queue
import scala.util.Random

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

	var q = Queue.empty[Int]                  //> q  : scala.collection.immutable.Queue[Int] = Queue()
	q :+= 1
	q :+= 2
	q :+= 3
	for(e <-q) println(e)                     //> 1
                                                  //| 2
                                                  //| 3
}