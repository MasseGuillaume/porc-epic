package porcEpic

import scala.reflect.ClassTag


import io.circe.generic.extras._, io.circe.syntax._
implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames

object HistoryElement {
  def apply[I, O](
      operation: Operation[I, O],
      describe: Operation[I, O] => String,
  ): HistoryElement =
    new HistoryElement(
      clientId = operation.clientId,
      invocation = operation.invocation,
      response = operation.response,
      description = describe(operation)
    )
}

@ConfiguredJsonCodec case class HistoryElement(
    clientId: ClientId,
    invocation: Time,
    response: Time,
    description: String
)

@ConfiguredJsonCodec case class LinearizationStep(
    index: OperationId,
    stateDescription: String
)

@ConfiguredJsonCodec case class PartitionVisualizationData(
    history: List[HistoryElement],
    partialLinearizations: List[List[LinearizationStep]],
    largestIndex: Map[Int, Int]
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
              index = histId,
              stateDescription = describeState(state)
            )
            if (largestSize(toInt(histId)) < partial.length) {
              largestSize(toInt(histId)) = partial.length
              largestIndex(toInt(histId)) = i
            }
          }
          linearization.toList
        }

      PartitionVisualizationData(
        history = operations.map(operation => HistoryElement(operation, describeOperation)),
        partialLinearizations = partialLinearizations,
        largestIndex = largestIndex.toMap
      )
    }
  }
}
