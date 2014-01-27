import scala.language.postfixOps
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Try, Success, Failure }
import rx.subscriptions.CompositeSubscription
import rx.lang.scala.Observable
import suggestions.gui._

object week4 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  "Eric Ji".replace(' ', '_')                     //> res0: String = Eric_Ji
  
 // Observable.interval(1 seconds).take(3).subscribe(println(_))
}