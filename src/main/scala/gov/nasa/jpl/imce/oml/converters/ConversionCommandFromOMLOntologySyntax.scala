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

import java.lang.System
import java.util.Properties

import ammonite.ops.Path
import gov.nasa.jpl.imce.oml.converters.utils.FileSystemUtilities
import gov.nasa.jpl.imce.oml.resolver.GraphUtilities
import gov.nasa.jpl.imce.oml.resolver.ResolverUtilities
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import gov.nasa.jpl.imce.xml.catalog.scope.CatalogScope
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.binding.owlapi.common.ImmutableModule
import gov.nasa.jpl.omf.scala.core.OMFError
import gov.nasa.jpl.omf.scala.core.tables.OMFTabularExport
import org.semanticweb.owlapi.model.IRI
import org.apache.spark.sql.{SQLContext, SparkSession}

import scala.collection.immutable.{Seq, Set}
import scala.{Int, None, Option, Ordering, Some, StringContext}
import scala.Predef.ArrowAssoc
import scalaz._
import Scalaz._
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc

case object ConversionCommandFromOMLOntologySyntax extends ConversionCommand {

  override val filePredicate = FileSystemUtilities.OMLOntologyFilePredicate

  override def convert
  (omlCatalogScope: OMLCatalogScope,
   outputCatalog: Option[Path],
   options: OMLConverter.Options)
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ (Option[CatalogScope], Seq[(tables.taggedTypes.IRI, OMLSpecificationTables)])
  = {

    implicit val mOrder: Ordering[OWLAPIOMF#ImmutableModule] = new Ordering[OWLAPIOMF#ImmutableModule] {
      override def compare(x: ImmutableModule, y: ImmutableModule): Int = x.iri.toString.compareTo(y.iri.toString)
    }

    val props = new Properties()
    props.setProperty("useSSL", "false")

    props.setProperty("dumpQueriesOnException", "true")
    props.setProperty("enablePacketDebug", "true")

    implicit val iriOrdering: Ordering[IRI] = new Ordering[IRI] {
      override def compare(x: IRI, y: IRI): Int = x.toString.compareTo(y.toString)
    }

    val tuple = for {
      in_store_cat <- ConversionCommand.createOMFStoreAndLoadCatalog(omlCatalogScope.omlCatalogFile,
        excludeOMLImports = options.excludeOMLImports,
        excludeOMLContent = options.excludeOMLContent,
        excludePurlImports = options.excludePurlImports)
      (inStore, inCat) = in_store_cat

      drc <- inStore.loadBuiltinDatatypeMap()
      om <- inStore.initializeOntologyMapping(drc)

      m2i <- omlCatalogScope.omlFiles.foldLeft {
        om.right[OMFError.Throwables]
      } { case (acc, (_, inputFile)) =>
        for {
          prev <- acc
          loadResult <- inStore.loadModule(prev, inputFile.toIO)
          (_, next) = loadResult
        } yield next
      }

      is = m2i.values

      iorder <- if (!options.hierarchicalSort)
        is.right[OMFError.Throwables]
      else {
        val in_drc = inStore.getBuildInDatatypeMap

        val builtInEdges = in_drc.builtInDatatypeModules.flatMap { m =>
          m.sig.importedTerminologies(inStore.ops).map { i =>
            m.iri -> i
          }
        }

        val g0 = Graph[IRI, DiEdge](is.map(_.iri): _*)

        val g1 = (g0 /: builtInEdges) { case (gi, (fI, tI)) =>
          System.out.println(s"convert from OWL(builtin) $fI ~> $tI")
          gi + fI ~> tI
        }

        val g2 = m2i.values.find {
          _.iri == OWLAPIOMFLoader.omlIRI
        } match {
          case Some(iOML) =>
            (g1 /: builtInEdges.map(_._1)) { case (gi, fI) =>
              System.out.println(s"convert from OWL(oml) ${iOML.iri} ~> $fI")
              gi + iOML.iri ~> fI
            }
          case None =>
            g1
        }

        val g3 = (g2 /: is) { case (gi, i) =>
          val gk = (gi /: i.sig.importedTerminologies(inStore.ops)) {
            case (gj, j) =>
              System.out.println(s"convert from OWL(tbox) ${i.iri} ~> $j")
              gj + i.iri ~> j
          }
          val gl = (gk /: i.sig.importedDescriptions(inStore.ops)) {
            case (gh, j) =>
              System.out.println(s"convert from OWL(dbox) ${i.iri} ~> $j")
              gh + i.iri ~> j
          }
          gl
        }

        for {
          gorder <- GraphUtilities.hierarchicalTopologicalSort(Seq(g3)).map(_.reverse)

          _ = gorder.foreach { iri =>
            val omlIRI = iri.toString + ".oml"
            System.out.println(s"convert from OWL(gorder): $iri => $omlIRI")
          }

          order <- gorder.foldLeft(Seq.empty[ImmutableModule].right[OMFError.Throwables]) { case (acc, iri) =>
            acc.flatMap { prev =>
              is.find { i => i.iri == iri } match {
                case Some(i) =>
                  (prev :+ i).right[OMFError.Throwables]
                case None =>
                  Set[java.lang.Throwable](
                    OMFError.omfError(s"There should be an immutable module for: $iri")
                  ).left
              }
            }
          }
        } yield order
      }

      ts <- OMFTabularExport.toTables[OWLAPIOMF](iorder)(inStore, inStore.ops)

      iri2tables = ts.map { case (m, t) => tables.taggedTypes.iri(m.iri.toString) -> t }

      extents <- ResolverUtilities.resolveTables(ResolverUtilities.initializeResolver(),iri2tables)

    } yield (extents, iri2tables)

    internal.process(tuple, outputCatalog, options, props)
  }
}