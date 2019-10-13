/**
  * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
  */
package actorbintree

import actorbintree.BinaryTreeNode.{CopyFinished, CopyTo}
import actorbintree.BinaryTreeSet._
import akka.actor._
import scala.concurrent.duration.Duration

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

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {

  import BinaryTreeSet._
  val system = ActorSystem("binary-tree")

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case operation: Operation =>   root ! operation
    case GC => {
      val newRoot = createRoot
      context.watch(root)
      context.become(garbageCollecting(newRoot))
      root ! CopyTo(newRoot)
    }
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case operation: Operation =>  pendingQueue = pendingQueue.enqueue(operation)
    case Terminated(_) => {
      pendingQueue.foreach(op => newRoot ! op)
      pendingQueue = Queue.empty
      root = newRoot
      context.become(normal)
    }
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {

  import BinaryTreeNode._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  def exists(next:Position) = subtrees.get(next).isDefined
  def getNode(next: Position) = subtrees.get(next).get
  def nextNode(number:Int) = if(elem > number) Left else Right

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {

    case Insert(requester, id, number) =>
    {
      def newNode = context.actorOf(BinaryTreeNode.props(number, false))
      def next = nextNode(number)
      if (elem==number) {
        removed = false
        requester ! OperationFinished(id)
      }
      else if (!exists(next)) {
                  subtrees = subtrees ++  Map( next -> newNode)
                  requester ! OperationFinished(id)
                }
      else getNode(next) ! Insert(requester, id, number)
    }

    case Remove(requester, id, number) => {
      def next = nextNode(number)
      if (elem == number) {
        removed = true
        requester ! OperationFinished(id)
      }
      else if (exists(next)) getNode(next) ! Remove(requester, id, number)
      else requester ! OperationFinished(id)
    }

    case  Contains(requester, id, number) => {
      def next = nextNode(number)
      if (elem == number && !removed) requester ! ContainsResult(id, true)
      else if (exists(next)) getNode(next) !  Contains(requester, id, number)
      else requester ! ContainsResult(id, false)
    }

    case CopyTo(newRoot) => {
      context.children.foreach(child => {
        context.watch(child)
        child ! CopyTo(newRoot)
      })

      if (!removed) {
        newRoot ! Insert(self, -1, elem)
        context.become(copying(false))
      }
      else
        context.become(copying(true))
    }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(insertConfirmed: Boolean): Receive = {
    if (insertConfirmed && context.children.isEmpty) context.stop(self)

    {
      case OperationFinished(_) => context.become(copying(true))
      case Terminated(_) => context.become(copying(insertConfirmed))
    }
  }

}