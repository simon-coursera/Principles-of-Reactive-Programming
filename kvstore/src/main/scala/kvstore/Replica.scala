package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout
import akka.actor.Cancellable

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  case class PersistMessage(receiver: ActorRef, key: String, valueOption: Option[String], starttime: Long)
  case class ReplicateFailed(receiver: ActorRef, key: String, id: Long)

  case class ReplicatorsMessage(receiver: ActorRef, key: String, replicators: Set[ActorRef], starttime: Long)

  case object Check

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  var expected_seq = 0L

  def receive = {
    case JoinedPrimary => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  arbiter ! Join

  val persistor: ActorRef = context.actorOf(persistenceProps)
  var persistAcks = Map.empty[Long, PersistMessage]
  var replicateAcks = Map.empty[Long, ReplicatorsMessage]

  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1.minute) {
    case _: Exception => Restart
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Replicas(replicas: Set[ActorRef]) => {
      replicas.foreach(replica => {
        if (replica != self && !secondaries.contains(replica)) {
          val newReplicator = context.actorOf(Replicator.props(replica))
          secondaries += (replica -> newReplicator)
          replicators += newReplicator

          replicateNew(newReplicator)
        }
      })

      if (replicas.size - 1 < secondaries.size) {
        //Handle replica has been removed case
        val removedReplicators = secondaries.filter(m => !replicas.contains(m._1)).map(m => m._2).toSet
        
        //Wave ack message from removed replicators
        replicateAcks = replicateAcks.map(a =>{
        	val id = a._1
        	val replicators = a._2.replicators.filter(r => !removedReplicators.contains(r))
        	if(replicators.size == 0 && !persistAcks.contains(id)) {
        	  a._2.receiver ! OperationAck(id)
        	}
        	(id -> ReplicatorsMessage(a._2.receiver, a._2.key, replicators, a._2.starttime))
        })
        
        replicateAcks = replicateAcks.filter(a => a._2.replicators.size > 0)
        if (persistAcks.size == 0 && replicateAcks.size == 0) stopCheckerScheduler
        
        removedReplicators.foreach(r => context.stop(r))
        secondaries = secondaries.filter(m => !removedReplicators.contains(m._2))
        replicators = secondaries.map(m => m._2).toSet
      }
    }
    case Insert(key: String, value: String, id: Long) => {
      kv += key -> value
      doReplicate(key, Some(value), id)
      doPersist(key, Some(value), id)
      startCheckerScheduler
    }
    case Remove(key: String, id: Long) => {
      kv -= key
      doReplicate(key, None, id)
      doPersist(key, None, id)
      startCheckerScheduler
    }
    case Get(key: String, id: Long) => sender ! GetResult(key, GetValue(key), id)
    case Persisted(key: String, id: Long) => if (persistAcks.contains(id)) {
      val receiver = persistAcks(id).receiver
      persistAcks -= id

      checkToConfirm(id, receiver)
    }
    case Check => doCheck
    case ReplicateFailed(receiver: ActorRef, key: String, id: Long) => {
      persistAcks -= id
      replicateAcks -= id
      receiver ! OperationFailed(id)
    }
    case Replicated(key: String, id: Long) => {
      if (replicateAcks.contains(id)) {
        val remainingReplicators = replicateAcks(id).replicators - sender
        val receiver = replicateAcks(id).receiver
        if (remainingReplicators.isEmpty) replicateAcks -= id
        else replicateAcks += (id -> ReplicatorsMessage(replicateAcks(id).receiver, key, remainingReplicators, replicateAcks(id).starttime))

        checkToConfirm(id, receiver)
      }
    }
  }

  def doReplicate(key: String, valueOption: Option[String], id: Long) = {
    if (replicators.size > 0) {
      replicators.foreach(replicator => {
        replicator ! Replicate(key, valueOption, id)
      })
      replicateAcks += (id -> ReplicatorsMessage(sender, key, replicators, System.currentTimeMillis))
    }
  }

  def checkToConfirm(id: Long, receiver: ActorRef) = {
    if (!replicateAcks.contains(id) && !persistAcks.contains(id)) receiver ! OperationAck(id)
    if (persistAcks.size == 0 && replicateAcks.size == 0) stopCheckerScheduler
  }

  def replicateNew(replicator: ActorRef) = {
    var id = 0L
    kv.foreach(e => {
      doReplicate(e._1, Some(e._2), id)
      id += 1
    })
  }

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Get(key: String, id: Long) => sender ! GetResult(key, GetValue(key), id)
    case Snapshot(key, valueOption, seq) => {
      if (seq == expected_seq) {
        valueOption match {
          case Some(value) => kv += key -> value
          case None => kv -= key
        }
        doPersist(key, valueOption, seq)
        startCheckerScheduler

        expected_seq += 1
      } else if (seq < expected_seq) {
        sender ! SnapshotAck(key, seq)
      } else None
    }
    case Persisted(key: String, id: Long) => if (persistAcks.contains(id)) {
      persistAcks(id).receiver ! SnapshotAck(persistAcks(id).key, id)
      persistAcks -= id
      if (persistAcks.size == 0) stopCheckerScheduler
    }
    case Check => doCheck
    case ReplicateFailed(receiver: ActorRef, key: String, id: Long) => None
  }

  def GetValue(key: String) = if (kv.contains(key)) Some(kv(key)) else None

  def doPersist(key: String, valueOption: Option[String], id: Long) = {
    persistor ! Persist(key, valueOption, id)
    persistAcks += (id -> PersistMessage(sender, key, valueOption, System.currentTimeMillis))
  }

  val TIMEOUT_MILLIS = 1000
  var scheduledChecker: Cancellable = null

  def startCheckerScheduler =
    if (scheduledChecker == null)
      scheduledChecker = context.system.scheduler.scheduleOnce(100.milliseconds, self, Check)

  def stopCheckerScheduler =
    if (scheduledChecker != null) {
      scheduledChecker.cancel
      scheduledChecker = null
    }

  def doCheck = {
    val currentTime = System.currentTimeMillis
    scheduledChecker = null
    //Persistent resend
    persistAcks.foreach(a => {
      if (currentTime - a._2.starttime >= TIMEOUT_MILLIS) self ! ReplicateFailed(a._2.receiver, a._2.key, a._1)
      else persistor ! Persist(a._2.key, a._2.valueOption, a._1)
    })
    //Clean up long period persistent effort
    persistAcks = persistAcks.filter(a => currentTime - a._2.starttime < TIMEOUT_MILLIS)

    //Cleanup non-responsive replication 
    replicateAcks.foreach(a => {
      if (currentTime - a._2.starttime >= TIMEOUT_MILLIS) self ! ReplicateFailed(a._2.receiver, a._2.key, a._1)
    })
    replicateAcks = replicateAcks.filter(a => currentTime - a._2.starttime < TIMEOUT_MILLIS)

    if (persistAcks.size > 0 || replicateAcks.size > 0) startCheckerScheduler
  }
}
