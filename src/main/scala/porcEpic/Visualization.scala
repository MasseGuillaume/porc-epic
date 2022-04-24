package porcEpic

import scala.reflect.ClassTag

import io.circe._, io.circe.generic.semiauto._

object HistoryElement {
  def apply[I, O](
      operation: Operation[I, O],
      describe: Operation[I, O] => String,
  ): HistoryElement =
    new HistoryElement(
      ClientId = operation.clientId,
      Invocation = operation.invocation,
      Response = operation.response,
      Description = describe(operation)
    )
}

case class HistoryElement(
  ClientId: ClientId,
  Invocation: Time,
  Response: Time,
  Description: String
)

case class LinearizationStep(
  Index: OperationId,
  StateDescription: String
)

case class PartitionVisualizationData(
  History: List[HistoryElement],
  PartialLinearizations: List[List[LinearizationStep]],
  LargestIndex: Map[Int, Int]
)

type VisualizationData = List[PartitionVisualizationData]

extension [S, I, O](specification: Specification[S, I, O])(using ci: ClassTag[I], co: ClassTag[O]) {
  def visualize(
      info: LinearizationInfo[I, O],
      path: String
  ): Unit = {}

  def visualize(
      info: LinearizationInfo[I, O],
      describeOperation: Operation[I, O] => String,
      describeState: S => String,
  ): VisualizationData = {

    info.history.zipWithIndex.map { (partition, i) =>
      val n = partition.length / 2
      val callValue = Array.ofDim[I](n)
      val returnValue = Array.ofDim[O](n)
      val operations = Entry.toOperations(partition)

      operations.foreach { operation =>
        callValue(toInt(operation.id)) = operation.input
        returnValue(toInt(operation.id)) = operation.output
      }

      val largestIndex = scala.collection.mutable.Map.empty[Int, Int]
      val largestSize = Array.ofDim[Int](n)
      

      val partialLinearizations =
        info.partialLinearizations(i).sortBy(_.length).zipWithIndex.map{ (partial, i) =>
          val linearization = Array.ofDim[LinearizationStep](partial.length)
          var state = specification.initialState
          partial.zipWithIndex.foreach { (histId, j) =>
            val (isLinearizable, nextState) = 
              specification.apply(
                state = state,
                input = callValue(toInt(histId)),
                output = returnValue(toInt(histId))
              )
            state = nextState
            if (!isLinearizable) {
              throw new Exception("valid partial linearization returned non-ok result from model step")
            }
            linearization(j) = LinearizationStep(
              Index = histId,
              StateDescription = describeState(state)
            )
            if (largestSize(toInt(histId)) < partial.length) {
              largestSize(toInt(histId)) = partial.length
              largestIndex(toInt(histId)) = i
            }
          }
          linearization.toList
        }

      PartitionVisualizationData(
        History = operations.map(operation => HistoryElement(operation, describeOperation)),
        PartialLinearizations = partialLinearizations,
        LargestIndex = largestIndex.toMap
      )
    }
  }
}
