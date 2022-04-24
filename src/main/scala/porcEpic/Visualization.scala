package porcEpic

import scala.reflect.ClassTag

import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._

import java.nio.file._

object HistoryElement {
  implicit val clientIdEncoder: Encoder[ClientId] = Encoder.encodeInt.contramap(ClientId.toInt)
  implicit val timeEncoder: Encoder[Time] = Encoder.encodeLong.contramap(Time.toLong)

  implicit val historyElementEncoder: Encoder[HistoryElement] = deriveEncoder

  def apply[I, O](
      operation: Operation[I, O],
      describe: Operation[I, O] => String,
  ): HistoryElement =
    new HistoryElement(
      ClientId = operation.clientId,
      Start = operation.invocation,
      End = operation.response,
      Description = describe(operation)
    )
}

case class HistoryElement(
  ClientId: ClientId,
  Start: Time,
  End: Time,
  Description: String
)

object LinearizationStep {
  implicit val operationIdEncoder: Encoder[OperationId] = Encoder.encodeInt.contramap(OperationId.toInt)
  implicit val linearizationStepEncoder: Encoder[LinearizationStep] = deriveEncoder
}

case class LinearizationStep(
  Index: OperationId,
  StateDescription: String
)

object PartitionVisualizationData {
  implicit val partitionVisualizationDataEncoder: Encoder[PartitionVisualizationData] = deriveEncoder
}

case class PartitionVisualizationData(
  History: List[HistoryElement],
  PartialLinearizations: List[List[LinearizationStep]],
  LargestIndex: Map[Int, Int]
)

type VisualizationData = List[PartitionVisualizationData]

extension (data: List[PartitionVisualizationData]) {
  def save(filename: String): Unit = {
    val json = data.asJson.spaces2
    Files.writeString(Paths.get(filename), s"const data = $json")
  }
}

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
        callValue(OperationId.toInt(operation.id)) = operation.input
        returnValue(OperationId.toInt(operation.id)) = operation.output
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
                input = callValue(OperationId.toInt(histId)),
                output = returnValue(OperationId.toInt(histId))
              )
            state = nextState
            if (!isLinearizable) {
              throw new Exception("valid partial linearization returned non-ok result from model step")
            }
            linearization(j) = LinearizationStep(
              Index = histId,
              StateDescription = describeState(state)
            )
            if (largestSize(OperationId.toInt(histId)) < partial.length) {
              largestSize(OperationId.toInt(histId)) = partial.length
              largestIndex(OperationId.toInt(histId)) = i
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
