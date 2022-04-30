package porcEpic

import scala.concurrent.{Await, Future, ExecutionContext}
import scala.concurrent.duration.{FiniteDuration, Duration}

import java.util.{BitSet, Stack, HashMap}

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{Executors, TimeUnit, Callable, CountDownLatch}
import java.util.concurrent.atomic.AtomicLong
import scala.util.control.NonFatal

enum Verbosity:
  case Debug
  case Error

extension [S, I, O](specification: OperationSpecification[S, I, O]) {
  def checkOperations(
    history: List[Operation[I, O]],
    timeout: Option[FiniteDuration] = None,
    verbosity: Verbosity = Verbosity.Error
  ): (CheckResult, Option[LinearizationInfo[I, O]]) = {
    val partitions = specification.partitionOperations(history).map(Entry.fromOperations)
    specification.checkParallel(partitions, timeout, verbosity)
  }
}

extension [S, I, O](specification: EntriesSpecification[S, I, O]) {
  def checkEntries(
    history: List[Entry[I, O]],
    timeout: Option[FiniteDuration] = None,
    verbosity: Verbosity = Verbosity.Error
  ): (CheckResult, Option[LinearizationInfo[I, O]]) = {
    val partitions = specification.partitionEntries(history).map(renumber)
    specification.checkParallel(partitions, timeout, verbosity)
  }
}

extension [S, I, O](specification: Specification[S, I, O]) {
  def checkParallel(
    partitionnedHistory: List[List[Entry[I, O]]],
    timeout: Option[FiniteDuration],
    verbosity: Verbosity
  ): (CheckResult, Option[LinearizationInfo[I, O]]) = {

    val totalTasksCount = partitionnedHistory.size

    val processors = Runtime.getRuntime().availableProcessors() / 2
    val threadPool = Executors.newFixedThreadPool(processors)
    var result = true
    val longest = Array.ofDim[List[List[OperationId]]](totalTasksCount)
    val killSwitch = new AtomicBoolean(false)
    var timedout = false

    timeout.foreach { t =>
      val scheduler = Executors.newScheduledThreadPool(1)
      scheduler.schedule(
        new Callable[Unit] {
          def call(): Unit = {
            killSwitch.set(true)
            timedout = true
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
            try {
              val paddedTaskId = leftPad(i.toString)(length = totalTasksCount.toString.length, pad = ' ')
              def remainingTaskCount = totalTasksCount - tasksCountLatch.getCount
              if (verbosity == Verbosity.Debug) {
                println(s"Start task $paddedTaskId [${remainingTaskCount}/$totalTasksCount]")
              }

              val (isLinearizable, longest0) = checkSingle(history, killSwitch)
              longest(i) = longest0
              result = result && isLinearizable
              if (!isLinearizable) {
                killSwitch.set(true)
              }
              
              tasksCountLatch.countDown()

              if (verbosity == Verbosity.Debug) {
                println(s"Done task $paddedTaskId Linearizable: ${isLinearizable} [${remainingTaskCount}/$totalTasksCount]")
              }
            } catch {
              case NonFatal(e) =>
                e.printStackTrace()
                killSwitch.set(true)
                tasksCountLatch.countDown()
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

    threadPool.shutdown()
      
    if (allDone && !timedout) {
      val resultOutput = 
        if (result) CheckResult.Ok
        else CheckResult.Illegal

      val info = LinearizationInfo[I, O](
        history = partitionnedHistory,
        partialLinearizations = longest.map(_.distinct).toList
      )

      (resultOutput, Some(info))

    } else {
      (CheckResult.TimedOut, None)
    }
  }

  private def checkSingle(history: List[Entry[I, O]], killSwitch: AtomicBoolean): (Boolean, List[List[OperationId]]) = {
    
    case class CacheEntry(linearized: BitSet, state: S)
    case class CallEntry(entry: EntryLinkedList[I, O], state: S)

    var entry = EntryLinkedList(history)

    val n = entry.length / 2
    val linearized = new BitSet(n)
    val cache = new HashMap[Int, List[CacheEntry]]

    def cacheContains(entry: CacheEntry): Boolean = {
      cache.getOrElse(entry.linearized.hashCode, Nil).exists( elem =>
        entry.linearized == elem.linearized && 
          specification.equal(entry.state, elem.state)
      )
    }

    var calls = new Stack[CallEntry]()
    val longest = Array.ofDim[Vector[OperationId]](n)
    var state = specification.initialState

    val sentinel = 
      DoubleLinkedList[EntryNode[I, O]](
        EntryNode.Return[I, O](
          output = null.asInstanceOf[O],
          id = OperationId(-1)
        )
      )
    val headEntry = sentinel.insertBefore(entry)

    def fail = (false, longest.toList.filter(_ != null).map(_.toList))

    while (headEntry.next != null) {
      if (killSwitch.get) {
        return fail
      }

      entry.elem match {
        case node: EntryNode.Call[_, _] =>
          val matching = 
            node.matches.elem match {
              case r: EntryNode.Return[_, _] => r.asInstanceOf[EntryNode.Return[I, O]]
              case _: EntryNode.Call[_, _]   => throw new Exception("call matching should be a return")
            }

          val (isLinearizable, newState) = specification.apply(state, node.input, matching.output)

          if (isLinearizable) {
            val newLinearized = linearized.clone().asInstanceOf[BitSet]
            newLinearized.set(OperationId.toInt(node.id))
            val newCacheEntry = CacheEntry(newLinearized, newState)
            if (!cacheContains(newCacheEntry)) {
              val hash = newLinearized.hashCode
              val existing = 
                if (cache.containsKey(hash)) cache.get(hash)
                else Nil
              cache.put(hash, newCacheEntry :: existing)
              calls.push(CallEntry(entry, state))
              state = newState
              linearized.set(OperationId.toInt(node.id))
              entry.lift()
              entry = headEntry.next
            } else {
              entry = entry.next
            }
          } else {
            entry = entry.next
          }

        case r: EntryNode.Return[_, _] =>
          val callsLength = calls.size
          if (callsLength == 0) {
            return fail
          }
          var seq: Vector[OperationId] = null
          calls.forEach { v =>
            if (longest(OperationId.toInt(v.entry.elem.id)) == null || 
                longest(OperationId.toInt(v.entry.elem.id)).lengthCompare(callsLength) < 0) {
              if (seq == null) {
                seq = calls.map(_.entry.elem.id)
              }
              longest(OperationId.toInt(v.entry.elem.id)) = seq
            }
          }
          val callTop = calls.pop
          entry = callTop.entry
          state = callTop.state
          linearized.clear(OperationId.toInt(entry.elem.id))
          
          entry.unlift()
          entry = entry.next
      }
    } // while

    val seq = calls.map(_.entry.elem.id)
    {
      var i = 0
      while(i < n) {
        longest(i) = seq
        i += 1
      }
    }

    (true, longest.map(_.toList).toList.filter(_ != null))
  }
}

def renumber[I, O](events: List[Entry[I, O]]): List[Entry[I, O]] = {
  val renumbering = new HashMap[OperationId, OperationId]
  var id = 0

  events.map{ event =>
    event.withId(
      renumbering.getOrElse(event.id, {
        val i = id
        renumbering.put(event.id, OperationId(id))
        id += 1
        OperationId(i)
      })
    )
  }
}

given EntryOrderingByTime[I, O]: Ordering[Entry[I, O]] =
  Ordering.by(e =>
    (
      Time.toLong(e.time),
      e match {
        case _: Entry.Call[_, _] => 0
        case _: Entry.Return[_, _] => 1
      }
    )
  )

extension [T](stack: Stack[T]) {
    def map[B](f: T => B): Vector[B] = {
      val builder = Vector.newBuilder[B]
      builder.sizeHint(stack.size)
      stack.forEach { v =>
        builder += f(v)
      }
      
      builder.result
    }
  }

extension [K, V](map: HashMap[K, V]) {
  def getOrElse(k: K, other: => V): V = {
    if (map.containsKey(k)) map.get(k)
    else other
  }
}

private [porcEpic] def leftPad(a: String)(length: Int, pad: Char): String =
  (pad.toString * (length - a.length)) + a