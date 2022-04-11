package porcEpic
package jepsen

import org.scalatest.funsuite.AnyFunSuite

object KeyValue {
  opaque type State = String

  sealed trait Input {
    val key: String
  }
  case class Put(key: String, value: String) extends Input
  case class Get(key: String) extends Input
  case class Append(key: String, value: String) extends Input
}

class KeyValueTest extends AnyFunSuite {

  import KeyValue._

  // val specification = new EntriesSpecification[State, Input]{
  //   def partitionEntries(entries: List[Entry[State, Input]]): List[List[Entry[State, Input]]] = {
  //     Entry.toOperations(entries)
  //       .groupBy(_.input.key)
  //       .values
  //       .map(Entry.fromOperations)
  //       .toList
  //   }
  //   def initialState: State = ""
  //   def apply(state: State, input: Input, output: State): (Boolean, State) = {
  //     input match {
  //       case Get(key)           => (output == state, state)
  //       case Put(key, value)    => (true, value)
  //       case Append(key, value) => (true, state + value)
  //     }
  //   }
  //   def describeOperation(input: Input, output: State): String = {
  //     input match {
  //       case Get(key)           => s"get($key) -> $output"
  //       case Put(key, value)    => s"put($key, $value)"
  //       case Append(key, value) => s"append($key, $value)"
  //     }
  //   }
  // }


  test("") {
    
  }

  // invokeGet, _    := `{:process (\d+), :type :invoke, :f :get,    :key "(.*)", :value nil}`
  // invokePut, _    := `{:process (\d+), :type :invoke, :f :put,    :key "(.*)", :value "(.*)"}`
  // nvokeAppend, _  := `{:process (\d+), :type :invoke, :f :append, :key "(.*)", :value "(.*)"}`

  // returnGet, _    := `{:process (\d+), :type :ok,     :f :get,    :key ".*",   :value "(.*)"}`
  // returnPut, _    := `{:process (\d+), :type :ok,     :f :put,    :key ".*",   :value ".*"}`
  // returnAppend, _ := `{:process (\d+), :type :ok,     :f :append, :key ".*",   :value ".*"}`
}

/*
var kvModel = Model{
  Partition: func(history []Operation) [][]Operation {
    m := make(map[string][]Operation)
    for _, v := range history {
      key := v.Input.(kvInput).key
      m[key] = append(m[key], v)
    }
    keys := make([]string, 0, len(m))
    for k := range m {
      keys = append(keys, k)
    }
    sort.Strings(keys)
    ret := make([][]Operation, 0, len(keys))
    for _, k := range keys {
      ret = append(ret, m[k])
    }
    return ret
  },
  PartitionEvent: func(history []Event) [][]Event {
    m := make(map[string][]Event)
    match := make(map[int]string) // id -> key
    for _, v := range history {
      if v.Kind == CallEvent {
        key := v.Value.(kvInput).key
        m[key] = append(m[key], v)
        match[v.Id] = key
      } else {
        key := match[v.Id]
        m[key] = append(m[key], v)
      }
    }
    var ret [][]Event
    for _, v := range m {
      ret = append(ret, v)
    }
    return ret
  },
  Init: func() interface{} {
    // note: we are modeling a single key's value here;
    // we're partitioning by key, so this is okay
    return ""
  },
  Step: func(state, input, output interface{}) (bool, interface{}) {
    inp := input.(kvInput)
    out := output.(kvOutput)
    st := state.(string)
    if inp.op == 0 {
      // get
      return out.value == st, state
    } else if inp.op == 1 {
      // put
      return true, inp.value
    } else {
      // append
      return true, (st + inp.value)
    }
  },
  DescribeOperation: func(input, output interface{}) string {
    inp := input.(kvInput)
    out := output.(kvOutput)
    switch inp.op {
    case 0:
      return fmt.Sprintf("get('%s') -> '%s'", inp.key, out.value)
    case 1:
      return fmt.Sprintf("put('%s', '%s')", inp.key, inp.value)
    case 2:
      return fmt.Sprintf("append('%s', '%s')", inp.key, inp.value)
    default:
      return "<invalid>"
    }
  },
}

*/