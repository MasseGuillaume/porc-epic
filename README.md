# Porc-épic [![Build Status](https://github.com/MasseGuillaume/porc-epic/actions/workflows/ci.yml/badge.svg)](https://github.com/MasseGuillaume/porc-epic/actions?query=workflow%3Aci)

Scala re-implementation of [Porcupine](https://github.com/anishathalye/porcupine) by 
Anish Athalye, please read the original project README.

## Usage

```sbt
scalaVersion := "3.1.0" // I only published for Scala 3 so far
libraryDependencies += "com.github.masseguillaume" %% "porc-epic % "0.0.0+29-e0ec9714-SNAPSHOT"
```

Defining a specification:

```scala

// Model definition

opaque type State = Int
object State {
  def apply(value: Int): State = value 
}
opaque type Output = State
object Output {
  def apply(value: Int): Output = value 
}

enum Input:
  case Put(value: State)
  case Get

import Input._

def describeOperation(operation: Operation[Input, Output]): String =
  operation.input match {
    case Put(value) => s"put($value)"
    case Get        => s"get() -> ${operation.output}"
  }

def describeState(state: State): String = state.toString

val model = new OperationSpecification[State, Input, Output]{
  def initialState: State = 0
  def apply(state: State, input: Input, output: State): (Boolean, State) = {
    input match {
      case Put(value) => (true, value)
      case Get => (output == state, state)
    }
  }

  // use partitionOperations if your history can be partitionned to get the full 
  // benefit of P-compositionality since it will check linearizability in parallel
}

// Non-linearizable history defined with operations (call-return)
val history = List[Operation[Input, Output]](
  Operation(OperationId(1), ClientId(0), Put(state(1)), output(0), invocation = Time(0), response = Time(10)),
  Operation(OperationId(2), ClientId(1), Get,           output(1), invocation = Time(2), response = Time(7)),
  Operation(OperationId(3), ClientId(2), Get,           output(0), invocation = Time(3), response = Time(7)),
)

// Checking for linearizability
val (result, Some(info)) = model.checkOperations(history)
assert(result == CheckResult.Illegal)

// Generates a visualization for histories and linearization points
model.visualize(info, describeOperation, describeState).save()

// Creates a temp folder
```
