package porcEpic

import scala.concurrent.{Await, Future, ExecutionContext}
import scala.concurrent.duration.{FiniteDuration, Duration}
import scala.collection.mutable.{BitSet => MBitset, Map => MMap}

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{Executors, TimeUnit, Callable, CountDownLatch}
import java.util.concurrent.atomic.AtomicLong

enum Verbosity:
  case Debug
  case Error

extension [State, Input, Output](specification: OperationSpecification[State, Input, Output]) {
  def checkOperations(
    history: List[Operation[Input, Output]],
    timeout: Option[FiniteDuration] = None,
    verbosity: Verbosity = Verbosity.Error
  ): (CheckResult, LinearizationInfo[Input, Output]) = {
    val partitions = specification.partitionOperations(history).map(Entry.fromOperations)
    specification.checkParallel(partitions, timeout, verbosity)
  }
}

extension [State, Input, Output](specification: EntriesSpecification[State, Input, Output]) {
  def checkEntries(
    history: List[Entry[Input, Output]],
    timeout: Option[FiniteDuration] = None,
    verbosity: Verbosity = Verbosity.Error
  ): (CheckResult, LinearizationInfo[Input, Output]) = {
    val partitions = specification.partitionEntries(history).map(renumber)
    specification.checkParallel(partitions, timeout, verbosity)
  }
}

extension [State, Input, Output](specification: Specification[State, Input, Output]) {
  def checkParallel(
    partitionnedHistory: List[List[Entry[Input, Output]]],
    timeout: Option[FiniteDuration],
    verbosity: Verbosity
  ): (CheckResult, LinearizationInfo[Input, Output]) = {

    val totalTasksCount = partitionnedHistory.size

    val processors = Runtime.getRuntime().availableProcessors()
    val threadPool = Executors.newFixedThreadPool(processors)
    var result = true
    var timedout = false
    val longest = Array.ofDim[List[List[OperationId]]](totalTasksCount)
    val killSwitch = new AtomicBoolean(false)

    timeout.foreach { t =>
      val scheduler = Executors.newScheduledThreadPool(1)
      scheduler.schedule(
        new Callable[Unit] {
          def call(): Unit = {
            timedout = true
            killSwitch.set(true)
          }
        },
        t.toSeconds,
        TimeUnit.SECONDS
      )
    }

    val tasksCountLatch = new CountDownLatch(totalTasksCount)

    partitionnedHistory.zipWithIndex.map { (history, i) =>
      threadPool.submit(
        new Callable[Unit] {
          def call(): Unit = {
            val (isLinearizable, longest0) = checkSingle(history, killSwitch)
            longest(i) = longest0
            result = result && isLinearizable
            if (!isLinearizable) {
              killSwitch.set(true)
            }
            
            tasksCountLatch.countDown()

            if (verbosity == Verbosity.Debug) {
              val padCount = leftPad(i.toString)(length = totalTasksCount.toString.length, pad = ' ')
              println(s"tasks [$padCount/$totalTasksCount] Linearizable: ${isLinearizable}")
            }
          }
        }
      )
    }

    val allDone =
      timeout match {
        case Some(t) => 
          tasksCountLatch.await(t.toSeconds, TimeUnit.SECONDS)

        case None =>
          tasksCountLatch.await()
          true
      }
      
    if (allDone && !timedout) {
      val resultOutput = 
        if (result) CheckResult.Ok
        else CheckResult.Illegal

      val info = LinearizationInfo[Input, Output](
        history = partitionnedHistory,
        partialLinearizations = longest.map(_.distinct).toList
      )

      (resultOutput, info)

    } else {
      (CheckResult.TimedOut, LinearizationInfo.empty[Input, Output])
    }
  }

  private def checkSingle(history: List[Entry[Input, Output]], killSwitch: AtomicBoolean): (Boolean, List[List[OperationId]]) = {
    case class CacheEntry(linearized: MBitset, state: State)
    case class CallEntry(entry: EntryLinkedList[Input, Output], state: State)

    extension (bitset: MBitset) {
      def set(v: OperationId): MBitset = bitset += toInt(v)
      def clear(v: OperationId): MBitset = bitset -= toInt(v)
    }

    var entry = EntryLinkedList(history)

    val n = entry.length / 2
    val linearized = MBitset.fromBitMaskNoCopy(Array.ofDim(n))
    val cache = MMap.empty[Int, List[CacheEntry]].withDefaultValue(Nil)

    def cacheContains(entry: CacheEntry): Boolean = {
      cache.getOrElse(entry.linearized.hashCode, Nil).exists( elem =>
        entry.linearized == elem.linearized && 
          specification.equal(entry.state, elem.state)
      )
    }

    var calls = Stack.empty[CallEntry]
    val longest = Array.ofDim[List[OperationId]](n)
    var state = specification.initialState

    val buggy = 
      DoubleLinkedList[EntryNode[Input, Output]](
        EntryNode.Return[Input, Output](
          value = null.asInstanceOf[Output],
          id = opid(-1)
        )
      )
    val headEntry = buggy.insertBefore(entry)

    while (headEntry.next != null) {
      if (killSwitch.get) {
        return (false, longest.toList)
      }
      entry.elem match {
        case node: EntryNode.Call[_, _] =>
          val matching = 
            node.matches.elem match {
              case r: EntryNode.Return[_, _] => r.asInstanceOf[EntryNode.Return[Input, Output]]
              case _: EntryNode.Call[_, _]   => throw new Exception("call matching should be a return")
            }

          val (isLinearizable, newState) = specification.apply(state, node.value, matching.value)

          if (isLinearizable) {
            val newLinearized = linearized.clone().set(node.id)
            val newCacheEntry = CacheEntry(newLinearized, newState)
            if (!cacheContains(newCacheEntry)) {
              val hash = newLinearized.hashCode
              cache(hash) = newCacheEntry :: cache(hash)
              calls.push(CallEntry(entry, state))
              state = newState
              linearized.set(node.id)
              entry.lift()
              entry = headEntry.next
            } else {
              entry = entry.next
            }
          } else {
            entry = entry.next
          }

        case r: EntryNode.Return[_, _] =>
          val callsLength = calls.length
          if (callsLength == 0) {
            return (false, longest.toList.filter(_ != null))
          }
          var seq: List[OperationId] = null
          calls.foreach { v =>
            if (longest(toInt(v.entry.elem.id)) == null || 
                callsLength > longest(toInt(v.entry.elem.id)).length) {
              if (seq == null) {
                seq = calls.reverse.map(_.entry.elem.id)
              }
              longest(toInt(v.entry.elem.id)) = seq
            }
          }
          val callTop = calls.pop
          entry = callTop.entry
          state = callTop.state
          linearized.clear(entry.elem.id)
          
          entry.unlift()
          entry = entry.next
      }
    } // while

    val seq = calls.reverse.map(_.entry.elem.id)
    {
      var i = 0
      while(i < n) {
        longest(i) = seq
        i += 1
      }
    }

    (true, longest.toList.filter(_ != null))
  }
}

def renumber[Input, Output](events: List[Entry[Input, Output]]): List[Entry[Input, Output]] = {
  val renumbering = MMap.empty[OperationId, OperationId]
  var id = 0

  events.map{ event =>
    event.withId(
      renumbering.getOrElse(event.id, {
        val i = id
        renumbering(event.id) = opid(id)
        id += 1
        opid(i)
      })
    )
  }
}

given EntryOrderingByTime[Input, Output]: Ordering[Entry[Input, Output]] =
  Ordering.by(e =>
    (
      toLong(e.time),
      e match {
        case _: Entry.Call[_, _] => 0
        case _: Entry.Return[_, _] => 1
      }
    )
  )

object Stack {
  def empty[T]: Stack[T] = new Stack[T](Nil)
}

class Stack[T](var xs: List[T]) {
  def push(x: T): Unit = xs = x :: xs
  def pop: T = {
    val t = xs.head
    xs = xs.tail
    t
  }
  def length: Int = xs.length
  def foreach(f: T => Unit): Unit = xs.foreach(f)
  def reverse: List[T] = xs.reverse
}

private [porcEpic] def leftPad(a: String)(length: Int, pad: Char): String =
  (pad.toString * (length - a.length)) + a