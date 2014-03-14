package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._
import akka.actor.Cancellable

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)

  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  case object Resend
  case object Send

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]

  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  /* TODO Behavior for the Replicator. */

  def receive: Receive = {
    case Replicate(key, valueOption, id) => {
      //merge the replication on same key
      var merged = false
      var seq = -1L
      pending = pending.map(e => {
        if (e.key == key) {
          merged = true
          seq = e.seq
          Snapshot(key, valueOption, seq)
        } else e
      })
      if (!merged) {
        seq = nextSeq
        pending = pending :+ Snapshot(key, valueOption, seq)
      }

      acks += (seq -> (sender -> Replicate(key, valueOption, id)))

      self ! Send
    }
    case SnapshotAck(key: String, seq: Long) => {
      if (acks.contains(seq)) {
        acks(seq)._1 ! Replicated(key, acks(seq)._2.id)
        acks -= seq
        if (acks.size == 0) stopResendScheduler
      }
    }
    case Send => {
      pending.foreach(s => replica ! s)
      pending = Vector.empty[Snapshot]
      startResendScheduler
    }
    case Resend => {
      scheduledChecker = None
      if (acks.size > 0) {
        acks.foreach(a => {
          val replicate = a._2._2
          val seq = a._1
          replica ! Snapshot(replicate.key, replicate.valueOption, seq)
        })
        startResendScheduler
      }
    }
  }

  var scheduledChecker: Option[Cancellable] = None

  def startResendScheduler = scheduledChecker match {
    case None => scheduledChecker = Some(context.system.scheduler.scheduleOnce(100.milliseconds, self, Resend))
    case Some(_) =>
  }
  
  def stopResendScheduler = scheduledChecker match {
    case Some(scheduler) => {
      scheduler.cancel
      scheduledChecker = None
    }
    case None =>
  }
}
