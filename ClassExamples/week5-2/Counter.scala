package week12

import akka.actor.Actor

class Counter extends Actor {
  var count = 0
  def receive = {/Users/simon/Documents/workspace/Coursera/Principles-of-Reactive-Programming/ClassExamples/week5-2/CounterMain.scala
    case "incr" => count += 1
    case "get"  => sender ! count
  }
}