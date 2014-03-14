import scala.collection.immutable.Queue
import scala.util.Random

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

	var kv = Map.empty[String, String]        //> kv  : scala.collection.immutable.Map[String,String] = Map()
	
	kv += "1" -> "one"
	kv += "1" -> "oneone"
	kv += "2" -> "two"
System.currentTimeMillis                          //> res0: Long = 1394790499308

	val s = Set(1,2,3)                        //> s  : scala.collection.immutable.Set[Int] = Set(1, 2, 3)
	s - 1                                     //> res1: scala.collection.immutable.Set[Int] = Set(2, 3)
}