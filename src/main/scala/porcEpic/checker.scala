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

extension [S, T](specification: OperationSpecification[S, T]) {
  def checkOperations(
    history: List[Operation[S, T]],
    timeout: Option[FiniteDuration] = None,
    verbosity: Verbosity = Verbosity.Error
  ): (CheckResult, LinearizationInfo[S, T]) = {
    val partitions = specification.partitionOperations(history).map(Entry.fromOperations)
    specification.checkParallel(partitions, timeout, verbosity)
  }
}

extension [S, T](specification: EntriesSpecification[S, T]) {
  def checkEntries(
    history: List[Entry[S, T]],
    timeout: Option[FiniteDuration] = None,
    verbosity: Verbosity = Verbosity.Error
  ): (CheckResult, LinearizationInfo[S, T]) = {
    val partitions = specification.partitionEntries(history).map(renumber)
    specification.checkParallel(partitions, timeout, verbosity)
  }
}


extension [S, T](specification: Specification[S, T]) {
  def checkParallel(
    partitionnedHistory: List[List[Entry[S, T]]],
    timeout: Option[FiniteDuration],
    verbosity: Verbosity
  ): (CheckResult, LinearizationInfo[S, T]) = {

    val processors = Runtime.getRuntime().availableProcessors()
    val threadPool = Executors.newFixedThreadPool(processors)
    var result = true
    var timedout = false

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

    val totalTasksCount = partitionnedHistory.size
    
    val tasksCountLatch = new CountDownLatch(totalTasksCount)

    partitionnedHistory.zipWithIndex.map { (history, i) =>
      threadPool.submit(
        new Callable[Unit] {
          def call(): Unit = {
            val (isLinearizable, _) = checkSingle(history, killSwitch)
            result = result && isLinearizable
            if (!isLinearizable) {
              killSwitch.set(true)
            }
            
            tasksCountLatch.countDown()

            if (verbosity == Verbosity.Debug) {
              def leftPad(a: String)(length: Int, pad: Char): String =
                (pad.toString * (length - a.length)) + a

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

      // TODO
      val info = LinearizationInfo.empty[S, T]
      (resultOutput, info)

    } else {
      val info = LinearizationInfo[S, T](
        history = Nil,
        partialLinearizations = Nil
      )

      (CheckResult.TimedOut, LinearizationInfo.empty[S, T])
    }
  }

  private def checkSingle(history: List[Entry[S, T]], killSwitch: AtomicBoolean): (Boolean, Array[Array[Int]]) = {
    case class CacheEntry(linearized: MBitset, state: S)
    case class CallEntry(entry: EntryLinkedList[S, T], state: S)

    extension (bitset: MBitset) {
      def set(v: Int): MBitset = bitset += v
      def clear(v: Int): MBitset = bitset -= v
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
    val longest = Array.ofDim[Array[Int]](n)
    var state = specification.initialState

    val buggy = 
      DoubleLinkedList[EntryNode[S, T]](
        EntryNode.Return[S, T](
          value = null.asInstanceOf[S],
          id = -1
        )
      )
    val headEntry = buggy.insertBefore(entry)

    while (headEntry.next != null) {
      if (killSwitch.get) {
        return (false, longest)
      }
      entry.elem match {
        case node: EntryNode.Call[_, _] =>
          val matching = 
            node.matches.elem match {
              case r: EntryNode.Return[_, _] => r.asInstanceOf[EntryNode.Return[S, T]]
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
            return (false, longest)
          }
          var seq: Array[Int] = null
          calls.foreach { v =>
            if (longest(v.entry.elem.id) == null || 
                callsLength > longest(v.entry.elem.id).length) {
              if (seq == null) {
                seq = Array.ofDim(callsLength)
                calls.zipWithIndex.foreach{(c, i) =>
                  seq(i) = v.entry.elem.id
                }
              }
              longest(v.entry.elem.id) = seq
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

    val seq = Array.ofDim[Int](calls.length)
    calls.zipWithIndex.reverse.foreach{  (v, i) =>
      seq(i) = v.entry.elem.id
    }

    {
      var i = 0
      while(i < n) {
        longest(i) = seq
        i += 1
      }
    }

    (true, longest)
  }
}


def renumber[S, T](events: List[Entry[S, T]]): List[Entry[S, T]] = {
  val renumbering = MMap.empty[Int, Int]
  var id = 0

  events.map{ event =>
    event.withId(
      renumbering.getOrElse(event.id, {
        val i = id
        renumbering(event.id) = id
        id += 1
        i
      })
    )
  }
}

given EntryOrderingByTime[S, T]: Ordering[Entry[S, T]] =
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
  def zipWithIndex: List[(T, Int)] = xs.zipWithIndex
}