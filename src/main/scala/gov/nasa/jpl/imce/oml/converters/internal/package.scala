/*
 * Copyright 2017 California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * License Terms
 */

package gov.nasa.jpl.imce.oml.converters

import gov.nasa.jpl.imce.oml.graphs.hierarchicalTopologicalSort
import gov.nasa.jpl.imce.oml.resolver.{ModuleGraphEdge, api}

import scala.collection.immutable.{Map, Seq, Set}
import scala.{None,Some,StringContext}
import scala.Predef.ArrowAssoc
import scalax.collection.GraphEdge.NodeProduct
import scalax.collection.immutable.Graph
import scalaz._
import Scalaz._

package object internal {

  type Throwables = Set[java.lang.Throwable]

  def sortExtents
  (moduleExtents: Map[api.Module, api.Extent],
   edgeExtents: Map[api.ModuleEdge, api.Extent])
  : Throwables \/ Seq[(api.Module, api.Extent)]
  = for {
    m2e <- moduleExtents.right[Throwables]

    g0 = Graph[api.Module, ModuleGraphEdge]()

    g1 = moduleExtents.foldLeft(g0) {
      case (gi, (mi, _)) =>
        gi + mi
    }

    g2 <- edgeExtents.foldLeft(g1.right[Throwables]) { case (acc, (me, ext)) =>
      for {
        gi <- acc
        source <- me.sourceModule()(ext) match {
          case Some(s) =>
            s.right[Throwables]
          case _ =>
            Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
              s"No source module for edge: $me"
            )).left
        }
        targetIRI = me.targetModule()(ext)
        gj = gi.toOuterNodes.find(_.iri == targetIRI).fold(gi) { target: api.Module =>
          val edge = new ModuleGraphEdge[api.Module](NodeProduct(source, target), me)
          gi + edge
        }
      } yield gj
    }

    g = g2

    moduleSort <-
    hierarchicalTopologicalSort[api.Module, ModuleGraphEdge](Seq(g), Seq.empty)
      .map(_.reverse)

    result <- moduleSort.foldLeft(Seq.empty[(api.Module, api.Extent)].right[Throwables]) { case (acc, m) =>
      for {
        prev <- acc
        e <- m2e.get(m) match {
          case Some(_e) =>
            _e.right[Throwables]
          case None =>
            Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
              s"No extent for module: $m"
            )).left
        }
        next = prev :+ (m -> e)
      } yield next
    }
  } yield result

}
