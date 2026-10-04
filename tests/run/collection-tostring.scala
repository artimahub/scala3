import scala.collection.immutable.{NumericRange, Queue, Range}
import scala.collection.mutable.PriorityQueue

object Test {
  def main(args: Array[String]): Unit = {
    queueToString()
    rangeToString()
    numericRangeToString()
    priorityQueueToString()
  }

  def queueToString(): Unit = {
    assert(Queue.empty[Int].toString == "Queue()")
    assert(Queue(1).toString == "Queue(1)")
    assert(Queue(1, 2, 3).toString == "Queue(1, 2, 3)")
    assert(Queue(1).enqueue(2).enqueue(3).toString == "Queue(1, 2, 3)")
  }

  def rangeToString(): Unit = {
    assert(Range(0, 10).toString == "Range 0 until 10")
    assert(Range(0, 10, 2).toString == "Range 0 until 10 by 2")
    assert(Range(0, 10, 3).toString == "inexact Range 0 until 10 by 3")
    assert(Range(0, 0).toString == "empty Range 0 until 0")
    assert(Range.inclusive(1, 5).toString == "Range 1 to 5")
    assert(Range.inclusive(1, 1).toString == "Range 1 to 1")
    assert(Range.inclusive(1, 5, 2).toString == "Range 1 to 5 by 2")
  }

  def numericRangeToString(): Unit = {
    assert(NumericRange(1, 10, 1).toString == "NumericRange 1 until 10")
    assert(NumericRange.inclusive(1, 10, 2).toString == "NumericRange 1 to 10 by 2")
    assert(NumericRange(0, 0, 1).toString == "empty NumericRange 0 until 0")
    assert(Range.Long(0L, 3L, 1L).toString == "NumericRange 0 until 3")
    assert(Range.Long(0L, 3L, 2L).toString == "NumericRange 0 until 3 by 2")
  }

  def priorityQueueToString(): Unit = {
    assert(PriorityQueue.empty[Int].toString == "PriorityQueue()")
    assert(PriorityQueue(1).toString == "PriorityQueue(1)")
    assert(PriorityQueue(3, 1, 2).toString == "PriorityQueue(3, 1, 2)")
    assert(PriorityQueue(9, 4, 7, 1).toString == "PriorityQueue(9, 4, 7, 1)")
  }
}