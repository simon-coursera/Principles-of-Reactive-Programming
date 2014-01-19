import scala.language.postfixOps
import scala.util._
import scala.util.control.NonFatal
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{ async, await }

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
 val t= Try({2})                                  //> t  : scala.util.Try[Int] = Success(2)
}