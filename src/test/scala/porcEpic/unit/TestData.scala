package porcEpic
package unit

object TestData {
  
  enum OperationKind:
    case W
    case R

  import OperationKind._
  import porcEpic.{Time => t}

  /*    t0    t1 t2   t3 t4    t5   t6 t7 |
    C0: |-W 0-|                           |
    C1:          |---------W 1---------|  |
    C2:               |----R 1------|     |
    C3:                  |-R 0-|          |
  */
  val history = List(
    Operation(OperationId(1), ClientId(0), input = (W, 0), output = 0, invocation = t(0), response = t(1)),
    Operation(OperationId(2), ClientId(1), input = (W, 1), output = 1, invocation = t(2), response = t(7)),
    Operation(OperationId(3), ClientId(2), input = (R, 1), output = 1, invocation = t(3), response = t(6)),
    Operation(OperationId(4), ClientId(3), input = (R, 0), output = 1, invocation = t(4), response = t(5)),
  )
}

