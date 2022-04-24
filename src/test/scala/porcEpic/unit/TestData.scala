package porcEpic
package unit

object TestData {
  
  enum OperationKind:
    case W
    case R

  import OperationKind._

  /*    t0    t1 t2   t3 t4    t5   t6 t7 |
    C0: |-W 0-|                           |
    C1:          |---------W 1---------|  |
    C2:               |----R 1------|     |
    C3:                  |-R 0-|          |
  */
  val history = List(
    Operation(id = OperationId(1), clientId = ClientId(0), input = (W, 0), invocation = Time(0), output = 0, response = Time(1)),
    Operation(id = OperationId(2), clientId = ClientId(1), input = (W, 1), invocation = Time(2), output = 1, response = Time(7)),
    Operation(id = OperationId(3), clientId = ClientId(2), input = (R, 1), invocation = Time(3), output = 1, response = Time(6)),
    Operation(id = OperationId(4), clientId = ClientId(3), input = (R, 0), invocation = Time(4), output = 1, response = Time(5)),
  )
}

