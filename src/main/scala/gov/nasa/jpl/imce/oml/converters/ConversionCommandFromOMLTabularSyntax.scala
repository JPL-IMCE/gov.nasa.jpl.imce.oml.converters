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
import gov.nasa.jpl.imce.oml.covariantTag.@@
import gov.nasa.jpl.imce.oml.frameless.OMLSpecificationTypedDatasets
import gov.nasa.jpl.imce.oml.resolver.GraphUtilities
import gov.nasa.jpl.imce.oml.resolver.ResolverUtilities
import gov.nasa.jpl.imce.oml.resolver.TableUtilities
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.tables.{OMLSpecificationTables, taggedTypes}
import gov.nasa.jpl.imce.xml.catalog.scope.CatalogScope
import gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFGraphStore
import gov.nasa.jpl.omf.scala.core.OMFError
import org.apache.spark.sql.{SQLContext, SparkSession}
import org.apache.xml.resolver.Catalog

import scala.{Int, None, Ordering, Some, StringContext}
import scala.collection.immutable.{Map, Seq, Set}
import scala.util.{Failure, Success, Try}
import scala.Predef.{ArrowAssoc, String}
import scalaz._
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc

case object ConversionCommandFromOMLTabularSyntax extends ConversionCommand {

  override val filePredicate = FileSystemUtilities.OMLJsonZipFilePredicate

  implicit def toThrowables[T](v: Try[T]): OMFError.Throwables \/ T = v match {
    case Success(t) =>
      \/-(t)
    case Failure(t) =>
      -\/(Set[java.lang.Throwable](t))
  }

  implicit def covariantOrdering[Tag]: Ordering[String @@ Tag] = new Ordering[String @@ Tag] {

    override def compare(x: @@[String, Tag], y: @@[String, Tag])
    : Int
    = x.compareTo(y)

  }

  override def convert
  (omlCatalogScope: OMLCatalogScope,
   outputDir: Path,
   outCatalog: Path,
   conversions: ConversionCommand.OutputConversions)
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ (CatalogScope, Seq[(tables.taggedTypes.IRI, OMLSpecificationTables)])
  = for {
    in_store_cat <- ConversionCommand.createOMFStoreAndLoadCatalog(omlCatalogScope.omlCatalogFile)
    (inStore, inCat) = in_store_cat
    out_store_cat <- ConversionCommand.createOMFStoreAndLoadCatalog(outCatalog)
    (outStore, outCat) = out_store_cat
    result <- convert(inStore, inCat, omlCatalogScope, outputDir, outStore, outCat, outCatalog, conversions)
  } yield result

  def convert
  (inStore: OWLAPIOMFGraphStore,
   inCat: Catalog,
   omlCatalogScope: OMLCatalogScope,
   outputDir: Path,
   outStore: OWLAPIOMFGraphStore,
   outCat: CatalogScope,
   outCatalog: Path,
   conversions: ConversionCommand.OutputConversions)
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ (CatalogScope, Seq[(tables.taggedTypes.IRI, OMLSpecificationTables)])
  = {
    val props = new Properties()
    props.setProperty("useSSL", "false")

    props.setProperty("dumpQueriesOnException", "true")
    props.setProperty("enablePacketDebug", "true")

    // 1) Read OML Tables

    val allTables
    : Seq[OMLSpecificationTables]
    = omlCatalogScope.omlFiles.par.map(TableUtilities.readOMLZipFile).to[Seq]

    val allModules
    : Map[taggedTypes.IRI, OMLSpecificationTables]
    = allTables.foldLeft(Map.empty[taggedTypes.IRI, OMLSpecificationTables]) { _ ++ TableUtilities.tableModules(_) }

    val g0
    : Graph[taggedTypes.IRI, DiEdge]
    = Graph[taggedTypes.IRI, DiEdge](allModules.keys.toSeq: _*)

    System.out.println(s"Read ${g0.size} OML modules")

    val g1
    : Graph[taggedTypes.IRI, DiEdge]
    = (g0 /: allTables) { case (gi, ti) =>
      val gj = gi ++ TableUtilities.tableEdges(ti).map { case (src, dst) => src ~> dst }
      gj
    }

    for {
      gorder <- GraphUtilities.hierarchicalTopologicalSort(Seq(g1)).map(_.reverse)

      _ = gorder.foreach { m =>
        System.out.println(s"convert from OWL(tables): $m")
      }

      ts = gorder.map(iri => iri -> allModules(iri))

      // 2) Convert from OML Tables => OML Resolver

      extents <- ResolverUtilities.resolveTables(
        ResolverUtilities.initializeResolver(),
        ts)

      // 3) Convert from OML Resolver => OML Textual Concrete Syntax

      _ <- if (conversions.toText)
        internal
          .toText(outCatalog, extents)
          .leftMap(_.toThrowables)
      else
        \/-(())

      // 4) Convert from OML Resolver => OMF/OWLAPI again

      _ <- if (conversions.toOWL)
        internal
          .OMLResolver2Ontology
          .convert(extents, outStore)
      else
        \/-(())

      // 6) Convert from OML Tables => SQL

      _ <- conversions.toSQL match {
        case Some(server) =>
          val tables = ts.map(_._2).reduceLeft(OMLSpecificationTables.mergeTables)
          OMLSpecificationTypedDatasets
            .sqlWriteOMLSpecificationTables(tables, server, props) match {
            case Success(_) =>
              \/-(())
            case Failure(t) =>
              -\/(Set(t))
          }
        case None =>
          \/-(())
      }

    } yield outCat -> ts
  }

}
