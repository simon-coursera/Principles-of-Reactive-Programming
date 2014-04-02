package week13

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.actorRef2Scala

object DistributedStore {
  case class Update(x: Int)
  case object Get
  case class Result(x: Int)
  case class Sync(x: Int, timestamp: Long)
  case object Hello
}

class DistributedStore extends Actor {
  import DistributedStore._
  
  var peers: List[ActorRef] = Nil
  var field = 0
  var lastUpdate = System.currentTimeMillis()
  
  def receive = {
    case Update(x) =>
      field = x
      lastUpdate = System.currentTimeMillis()
      peers foreach (_ ! Sync(field, lastUpdate))
    case Get => sender ! Result(field)
    case Sync(x, timestamp) if timestamp > lastUpdate =>
      field = x
      lastUpdate = timestamp
    case Hello =>
      peers ::= sender
      sender ! Sync(field, lastUpdate)
  }
}