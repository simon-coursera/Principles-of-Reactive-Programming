/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /**
   * Request with identifier `id` to insert an element `elem` into the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /**
   * Request with identifier `id` to check whether an element `elem` is present
   * in the tree. The actor at reference `requester` should be notified when
   * this operation is completed.
   */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /**
   * Request with identifier `id` to remove the element `elem` from the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  //case object TreeCopyFinished
  //case object Stop
  //case object ChildrenStopped
  //case object RerunOperation

  /** Request to perform garbage collection*/
  case object GC

  /**
   * Holds the answer to the Contains request with identifier `id`.
   * `result` is true if and only if the element is present in the tree.
   */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}

class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case Insert(requester, id, elem) => root ! Insert(requester, id, elem)
    case Contains(requester, id, elem) => root ! Contains(requester, id, elem)
    case Remove(requester, id, elem) => root ! Remove(requester, id, elem)
    case GC => {
      val newRoot = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))
      context.become(garbageCollecting(newRoot))
      root ! CopyTo(newRoot)
    }
  }

  // optional
  /**
   * Handles messages while garbage collection is performed.
   * `newRoot` is the root of the new binary tree where we want to copy
   * all non-removed elements into.
   */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case Insert(requester, id, elem) => pendingQueue :+= Insert(requester, id, elem)
    case Contains(requester, id, elem) => pendingQueue :+= Contains(requester, id, elem)
    case Remove(requester, id, elem) => pendingQueue :+= Remove(requester, id, elem)
    case GC => None
    /*
    case ChildrenStopped => {
      root = newRoot
      context.become(normal)

      //println(s"Resending queued message$message")
      //Must make sure the queued the message directly to newRoot here!!! Other place may lead to some later message handled before the queued message     
      for (message <- pendingQueue) newRoot ! message
      pendingQueue = Queue.empty[Operation]      
    }
    */
    case CopyFinished => {
      //println("SetTree Copy Finished")
      //Stop the node actor and all its sub-notes
      //Use explicit message to make sure all obsoleted children stopped gracefully before serving new operations
      //root ! Stop
      
      context.stop(root)
      root = newRoot
      context.become(normal)
      for (message <- pendingQueue) newRoot ! message
      pendingQueue = Queue.empty[Operation]        
    }
  }
}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode], elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved
  var notifyParent = false

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester, id, e) => {
      if (e == elem) {
        removed = false
        requester ! OperationFinished(id)
      } else if (e > elem) {
        if (subtrees.contains(Right)) subtrees(Right) ! Insert(requester, id, e)
        else {
          subtrees += (Right -> context.actorOf(BinaryTreeNode.props(e, false)))
          requester ! OperationFinished(id)
        }
      } else {
        if (subtrees.contains(Left)) subtrees(Left) ! Insert(requester, id, e)
        else {
          subtrees += (Left -> context.actorOf(BinaryTreeNode.props(e, false)))
          requester ! OperationFinished(id)
        }
      }
    }
    case Contains(requester, id, e) => {
      if (e == elem) requester ! ContainsResult(id, !removed) //Make sure we return false even the node exists but was removed
      else if (e > elem) {
        if (subtrees.contains(Right)) {
          subtrees(Right) ! Contains(requester, id, e)
        } else requester ! ContainsResult(id, false)
      } else {
        if (subtrees.contains(Left)) subtrees(Left) ! Contains(requester, id, e)
        else requester ! ContainsResult(id, false)
      }
    }
    case Remove(requester, id, e) => {
      if (e == elem) {
        removed = true
        requester ! OperationFinished(id)
      } else if (e > elem) {
        if (subtrees.contains(Right)) subtrees(Right) ! Remove(requester, id, e)
        else requester ! OperationFinished(id)
      } else {
        if (subtrees.contains(Left)) subtrees(Left) ! Remove(requester, id, e)
        else requester ! OperationFinished(id)
      }
    }
    case CopyTo(treeNode) => {
      //println(s"Prepare to copy:$self")
      var expected = Set[ActorRef]()

      if (subtrees.contains(Right)) {
        expected += subtrees(Right)
        subtrees(Right) ! CopyTo(treeNode)
      }
      if (subtrees.contains(Left)) {
        expected += subtrees(Left)
        subtrees(Left) ! CopyTo(treeNode)
      }
      context.become(copying(expected, false))

      if (!removed) treeNode ! Insert(self, -1, elem)
      else self ! OperationFinished(-1) //if the node is removed just send message to notify copy complete
    }
    /*
    case Stop => {
      notifyParent = true
      context.stop(self)
    }
    */
  }

  // optional
  /**
   * `expected` is the set of ActorRefs whose replies we are waiting for,
   * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
   */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(_) => {
      if (expected.isEmpty) {
        context.become(normal)
        context.parent ! CopyFinished
      } else
        context.become(copying(expected, true))
    }
    case CopyFinished => {
      val remain = expected - sender
      if (remain.isEmpty && insertConfirmed) {
        context.become(normal)
        context.parent ! CopyFinished
      } else
        context.become(copying(remain, insertConfirmed))
    }
  }

  override def postStop() {
    //if (notifyParent) context.parent ! ChildrenStopped
  }
}
