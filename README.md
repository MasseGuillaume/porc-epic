# Porc-épic [![Build Status](https://github.com/MasseGuillaume/porc-epic/actions/workflows/ci.yml/badge.svg)](https://github.com/MasseGuillaume/porc-epic/actions?query=workflow%3Aci)

Scala re-implementation of [Porcupine](https://github.com/anishathalye/porcupine) by 
Anish Athalye, please read the original project README.

## Usage

```sbt
scalaVersion := "3.1.0" // I only published for Scala 3 so far
resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies += "com.github.masseguillaume" %% "porc-epic % "0.0.0+29-e0ec9714-SNAPSHOT"
```

## Defining a Specification with Operations

## Specification

```scala
object Register {

  opaque type State = Int
  object State {
    def apply(value: Int): State = value 
  }

  enum Input:
    case Put(value: State)
    case Get

  opaque type Output = State
  object Output {
    def apply(value: Int): Output = value 
  }


  val specification = new OperationSpecification[State, Input, Output]{
    def initialState: State = 0
    def apply(state: State, input: Input, output: State): (Boolean, State) = {
      input match {
        case Put(value) => (true, value)
        case Get => (output == state, state)
      }
    }
  }
}
```

### History

```scala
import Register._
import Input._

// Non-linearizable history defined with operations (call-return)
val history = List[Operation[Input, Output]](
  Operation(OperationId(0), ClientId(0), Put(State(1)), Output(0), invocation = Time(0), response = Time(10)),
  Operation(OperationId(1), ClientId(1), Get,           Output(1), invocation = Time(2), response = Time(7)),
  Operation(OperationId(2), ClientId(2), Get,           Output(0), invocation = Time(3), response = Time(7)),
)

// Checking for linearizability
val (result, Some(info)) = specification.checkOperations(history)
// result == CheckResult.Illegal
```

### Visualization

```scala
def describeOperation(operation: Operation[Input, Output]): String =
  operation.input match {
    case Put(value) => s"put($value)"
    case Get        => s"get() -> ${operation.output}"
  }

def describeState(state: State): String = state.toString

// Generates a temporary folder with a visualization for histories and linearization points
specification.visualize(info, describeOperation, describeState).save()

/*
/tmp/porc-epic14498592030205251063
├── app.js
├── data.js
├── style.css
└── visualization.html
/*
```

## Defining a Specification with Entries

### Specification

```scala
object KeyValue {
  opaque type State = String
  object State {
    def apply(value: String): State = value
  }
  extension (state: State) {
    def +(other: State): State = state + other
  }
  sealed trait Input {
    val key: String
  }
  object Input {
    case class Put(key: String, value: State) extends Input
    case class Get(key: String) extends Input
    case class Append(key: String, value: State) extends Input
  }
  opaque type Output = State
  object Output {
    def apply(value: String): Output = value
  }

  val specification = new EntriesSpecification[State, Input, Output] {
    override def partitionEntries(entries: List[Entry[Input, Output]]): List[List[Entry[Input, Output]]] = {
      Entry
        .toOperations(entries)
        .groupBy(_.input.key)
        .values
        .map(Entry.fromOperations)
        .toList
    }
    def initialState: State = State("")
    def apply(state: State, input: Input, output: State): (Boolean, State) = {
      input match {
        case Input.Get(key)           => (output == state, state)
        case Input.Put(key, value)    => (true, value)
        case Input.Append(key, value) => (true, state + value)
      }
    }
  }
}
```

### History

```scala
import KeyValue._
import Input._

val history = List[Entry[Input, Output]](
  Entry.Call(   Put("k", State("1")) , Time(0) , OperationId(0), ClientId(0)),
  Entry.Return( Output("0")          , Time(10), OperationId(0), ClientId(0)),
  Entry.Call(   Get("k")             , Time(2) , OperationId(2), ClientId(0)),
  Entry.Return( Output("1")          , Time(7) , OperationId(2), ClientId(0)),
  Entry.Call(   Get("k")             , Time(3) , OperationId(3), ClientId(0)),
  Entry.Return( Output("0")          , Time(7) , OperationId(3), ClientId(0)),
)

val (result, _) = KeyValue.specification.checkEntries(history)
// result == CheckResult.Illegal
```

## Benchmark

```
sbt bench/jmh:run

Benchmark                    Mode  Cnt  Score   Error  Units
porcEpic.EtcdBench.etcd_000  avgt   25  2.329 ± 0.270  ms/op
...
```