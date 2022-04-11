package porcEpic

import porcEpic.{fromLong => t}

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
  Operation(clientId = cid(0), input = (W, 0), invocation = t(0), output = 0, response = t(1)),
  Operation(clientId = cid(1), input = (W, 1), invocation = t(2), output = 1, response = t(7)),
  Operation(clientId = cid(2), input = (R, 1), invocation = t(3), output = 1, response = t(6)),
  Operation(clientId = cid(3), input = (R, 0), invocation = t(4), output = 1, response = t(5)),
)
