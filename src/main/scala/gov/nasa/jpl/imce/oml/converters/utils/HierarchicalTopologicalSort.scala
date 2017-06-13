package gov.nasa.jpl.imce.oml.converters.utils

import gov.nasa.jpl.omf.scala.core.OMFError.Throwables

import scala.collection.immutable._
import scala.reflect.ClassTag
import scala.{Option,None,Some}
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.connectivity.GraphComponents
import scalaz._, Scalaz._

object HierarchicalTopologicalSort {

  @scala.annotation.tailrec
  final def hierarchicalTopologicalSort[N: ClassTag, E[M] <: DiEdge[M]]
  (queue: Seq[Graph[N, E]], before: Seq[N] = Seq.empty, after: Seq[N] = Seq.empty)
  : Throwables \/ Seq[N]
  = queue match {
    case Nil =>
      val sort: Seq[N] = (before ++ after).reverse
      import java.lang.System.out._
      import scala.StringContext
      println(s"hierarchicalTopologicalSort: ${sort.size} nodes")
      sort.zipWithIndex.foreach { case (n, i) =>
        println(s" $i: $n")
      }
      sort.right
    case g :: gs =>

      // Workaround
      // https://github.com/scala-graph/scala-graph/issues/75
      Option.apply(
        org.apache.log4j.LogManager.getLogger("scalax.collection.connectivity.GraphComponents")
      ) match {
        case Some(logger) =>
          logger.setLevel(org.apache.log4j.Level.OFF)
        case None =>
          ()
      }

      val dag = GraphComponents.graphToComponents(g).stronglyConnectedComponentsDag

      dag.topologicalSortByComponent().toList match {
        case Nil =>
          (before ++ after).right[Throwables]

        case n :: ns =>
          if (n.isLeft) {
            val n1 = n.left.get
            val ns: Seq[N] = n1.toOuter.toOuterNodes.to[Seq]
            hierarchicalTopologicalSort(gs, before ++ ns, after)
          } else if (n.isRight) {
            val cycle = n.right.get
            val ns: Seq[N] = cycle.toOuterNodes.to[Seq].flatMap(_.toOuterNodes.to[Seq])
            hierarchicalTopologicalSort(gs, before ++ ns, after)
          } else
            hierarchicalTopologicalSort(gs, before, after)

      }
  }

}
