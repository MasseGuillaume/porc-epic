package porcEpic

case class HistoryElement(
  clientId: ClientId,
  invocation: Time,
  response: Time,
  description: String
)

object HistoryElement 

case class LinearizationStep(
  Index: Int,
  StateDescription: String
)
case class PartitionVisualizationData(
  History: List[HistoryElement],
  PartialLinearizations: List[List[LinearizationStep]],
  Largest: Map[Int, Int]
)
type VisualizationData = List[PartitionVisualizationData]

trait Describe[T]:
  def describe: String


extension [State, Input, Output] (specification: Specification[State, Input, Output])(using describe: Describe[Operation[Input, Output]]) {
  def visualize(info: LinearizationInfo[Input, Output], path: String): Unit = {

  }

  private def computeVisualizationData(info: LinearizationInfo[Input, Output]): VisualizationData = {
    // info.history.map { partition =>
    //   val n = partition.length / 2
    //   // val history = Array.ofDim[HistoryElement](n)
      
    //   val callValue = scala.collection.mutable.Map.empty[Int, Input]
    //   val returnValue = scala.collection.mutable.Map.empty[Int, Output]


    //   Entry.toOperations(partition).sortBy(_.id).map( operation =>
    //     HistoryElement(
    //       ClientId = operation.clientId
    //       Start = operation.Start
    //       End = 
    //       Description = 

    //       clientId
    //       invocation
    //       response
    //       description
    //     )
    //   )

    //     clientId: ClientId,
    //     input: Input,
    //     invocation: Time,
    //     output: Output,
    //     response: Time
    //   }

      // partition.foreach {
      //   case call: Entry.Call[_, _, _] =>
      //     history(call.id)
      //   case ret: Entry.Return[_, _, _] =>
      // }

      ???

    // }
  }
}