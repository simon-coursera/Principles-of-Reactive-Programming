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

  context.system.scheduler.scheduleOnce(100.milliseconds, self, Resend)

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Replicas(replicas: Set[ActorRef]) => {
      replicas.foreach(replica => {
        if (!secondaries.contains(replica)) {
          val newReplicator = context.actorOf(Replicator.props(replica))
          secondaries += (replica -> newReplicator)
          replicators += newReplicator
        }
      })
    }
    case Insert(key: String, value: String, id: Long) => {
      kv += key -> value
      doReplicate(key, Some(value), id)
      doPersist(key, Some(value), id)
      //sender ! OperationAck(id)
    }
    case Remove(key: String, id: Long) => {
      kv -= key
      doReplicate(key, None, id)
      doPersist(key, None, id)
      //sender ! OperationAck(id)
    }
    case Get(key: String, id: Long) => sender ! GetResult(key, GetValue(key), id)
    case Persisted(key: String, id: Long) => if (persistAcks.contains(id)) {
      val receiver = persistAcks(id).receiver
      persistAcks -= id

      checkToConfirm(id, receiver)
    }
    case Resend => doCleanUp
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
      replicateAcks += (id -> ReplicatorsMessage(sender,key, replicators, System.currentTimeMillis))
    }
  }

  def checkToConfirm(id: Long, receiver: ActorRef) = if (!replicateAcks.contains(id) && !persistAcks.contains(id)) receiver ! OperationAck(id)

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

        expected_seq += 1
      } else if (seq < expected_seq) {
        sender ! SnapshotAck(key, seq)
      } else None
    }
    case Persisted(key: String, id: Long) => if (persistAcks.contains(id)) {
      persistAcks(id).receiver ! SnapshotAck(persistAcks(id).key, id)
      persistAcks -= id
    }
    case Resend => doCleanUp
    case ReplicateFailed(receiver: ActorRef, key: String, id: Long) => None
  }

  def GetValue(key: String) = if (kv.contains(key)) Some(kv(key)) else None

  def doPersist(key: String, valueOption: Option[String], id: Long) = {
    persistor ! Persist(key, valueOption, id)
    persistAcks += (id -> PersistMessage(sender, key, valueOption, System.currentTimeMillis))
  }

  val TIMEOUT_MILLIS = 1000
  
  def doCleanUp = {
    val currentTime = System.currentTimeMillis
    
    //Persistent resend
    persistAcks.foreach(a => {
      if (currentTime - a._2.starttime >= TIMEOUT_MILLIS) self ! ReplicateFailed(a._2.receiver, a._2.key, a._1)
      else persistor ! Persist(a._2.key, a._2.valueOption, a._1)
    })

    //Clean up long period persistent effort
    persistAcks = persistAcks.filter(a => currentTime - a._2.starttime < 1000)
    
    //Cleanup non-responsive replication 
    replicateAcks.foreach(a => {
      if(currentTime - a._2.starttime >= TIMEOUT_MILLIS) self ! ReplicateFailed(a._2.receiver, a._2.key, a._1)
    })
    replicateAcks = replicateAcks.filter(a => currentTime - a._2.starttime < 1000)
    
    context.system.scheduler.scheduleOnce(100.milliseconds, self, Resend)
  }
}
