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
import java.nio.file.Paths
import java.util.Properties

import ammonite.ops.Path
import gov.nasa.jpl.imce.oml.converters.utils.FileSystemUtilities
import gov.nasa.jpl.imce.oml.frameless.OMLSpecificationTypedDatasets
import gov.nasa.jpl.imce.oml.resolver.GraphUtilities
import gov.nasa.jpl.imce.oml.resolver.ResolverUtilities
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import gov.nasa.jpl.imce.xml.catalog.scope.CatalogScope
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.binding.owlapi.common.ImmutableModule
import gov.nasa.jpl.omf.scala.core.OMFError
import gov.nasa.jpl.omf.scala.core.tables.OMFTabularExport
import org.apache.spark.sql.{SQLContext, SparkSession}

import scala.collection.immutable.{Seq, Set}
import scala.{Int, None, Option, Ordering, Some, StringContext, Unit}
import scala.Predef.ArrowAssoc
import scalaz._
import Scalaz._
import scala.util.{Failure, Success}
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc

case object ConversionCommandFromOMLOntologySyntax extends ConversionCommand {

  override val filePredicate = FileSystemUtilities.OMLOntologyFilePredicate

  override def convert
  (omlCatalogScope: OMLCatalogScope,
   outCatalog: Path,
   conversions: ConversionCommand.OutputConversions)
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ (CatalogScope, Seq[(tables.taggedTypes.IRI, OMLSpecificationTables)])
  = for {
    in_store_cat <- ConversionCommand.createOMFStoreAndLoadCatalog(omlCatalogScope.omlCatalogFile)
    (inStore, inCat) = in_store_cat
    out_store_cat <- ConversionCommand.createOMFStoreAndLoadCatalog(outCatalog)
    (outStore, outCat) = out_store_cat
    result <- convert(inStore, omlCatalogScope, outStore, outCat, outCatalog, conversions)
  } yield result

  def convert
  (inStore: OWLAPIOMFGraphStore,
   omlCatalogScope: OMLCatalogScope,
   outStore: OWLAPIOMFGraphStore,
   outCat: CatalogScope,
   outCatalog: Path,
   conversions: ConversionCommand.OutputConversions)
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ (CatalogScope, Seq[(tables.taggedTypes.IRI, OMLSpecificationTables)])
  = {
    implicit val mOrder: Ordering[OWLAPIOMF#ImmutableModule] = new Ordering[OWLAPIOMF#ImmutableModule] {
      override def compare(x: ImmutableModule, y: ImmutableModule): Int = x.iri.toString.compareTo(y.iri.toString)
    }

    val props = new Properties()
    props.setProperty("useSSL", "false")

    props.setProperty("dumpQueriesOnException", "true")
    props.setProperty("enablePacketDebug", "true")

    val result
    : OMFError.Throwables \/ (CatalogScope, Seq[(tables.taggedTypes.IRI, OMLSpecificationTables)])
    = for {
      m2i <- omlCatalogScope.omlFiles.foldLeft {
        emptyMutable2ImmutableModuleMap.right[OMFError.Throwables]
      } { case (acc, (_, inputFile)) =>
        for {
          prev <- acc
          loadResult <- inStore.loadModule(prev, inputFile.toIO)
          (_, next) = loadResult
        } yield next
      }

      is = m2i.values

      in_drc = inStore.getBuildInDatatypeMap

      builtInEdges = in_drc.builtInDatatypeModules.flatMap { f =>
        val i: Option[OWLAPIOMF#ImmutableModule] = f match {
          case fI: OWLAPIOMF#ImmutableModule =>
            Some(fI)
          case fM: OWLAPIOMF#MutableModule =>
            m2i.get(fM) match {
              case Some(fI: OWLAPIOMF#ImmutableModule) =>
                Some(fI)
              case _ =>
                None
            }
        }

        i match {
          case Some(fI) =>
            fI.sig.importedTerminologies(inStore.ops).flatMap {
              case tI: OWLAPIOMF#ImmutableModule =>
                Some(fI -> tI)
              case tM: OWLAPIOMF#MutableModule =>
                m2i.get(tM) match {
                  case Some(tI: OWLAPIOMF#ImmutableModule) =>
                    Some(fI -> tI)
                  case _ =>
                    None
                }
            }
          case None =>
            None
        }
      }

      g0 = Graph[OWLAPIOMF#ImmutableModule, DiEdge](is: _*)

      g1 = (g0 /: builtInEdges) { case (gi, (fI, tI)) =>
        System.out.println(s"convert from OWL(builtin) ${fI.iri} ~> ${tI.iri}")
        gi + fI ~> tI
      }

      g2 = m2i.values.find { _.iri == OWLAPIOMFLoader.omlIRI } match {
        case Some(iOML) =>
          (g1 /: builtInEdges.map(_._1)) { case (gi, fI) =>
            System.out.println(s"convert from OWL(oml) ${iOML.iri} ~> ${fI.iri}")
            gi + iOML ~> fI
          }
        case None =>
          g1
      }

      g3 = (g2 /: is) { case (gi, i) =>
        val gk = (gi /: i.sig.importedTerminologies(inStore.ops)) {
          case (gj, itbox: OWLAPIOMF#ImmutableTerminologyBox) =>
            System.out.println(s"convert from OWL(tbox) ${i.iri} ~> ${itbox.iri}")
            gj + i ~> itbox
          case (gj, _) =>
            gj
        }
        val gl = (gk /: i.sig.importedDescriptions(inStore.ops)) {
          case (gh, idbox: OWLAPIOMF#ImmutableDescriptionBox) =>
            System.out.println(s"convert from OWL(dbox) ${i.iri} ~> ${idbox.iri}")
            gh + i ~> idbox
          case (gh, _) =>
            gh
        }
        gl
      }

      gorder <- GraphUtilities.hierarchicalTopologicalSort(Seq(g3)).map(_.reverse)

      _ = gorder.foreach { g =>
        val iri = g.iri.toString
        val omlIRI = if (iri.endsWith("/"))
          iri.replaceFirst("^(.*)/([a-zA-Z0-9.]+)/$", "$1/$2/$2.oml")
        else
          iri + ".oml"
        System.out.println(s"convert from OWL(gorder): ${g.iri} => $omlIRI")
      }

      ts <- OMFTabularExport.toTables[OWLAPIOMF](gorder)(inStore, inStore.ops)

      _ = {
        (gorder zip ts).foreach { case (gi, (im, ti)) =>
          if (gi.iri != im.iri) {
            System.out.println(s"convert from OWL(tables)  gi=${gi.iri}")
            System.out.println(s"convert from OWL(tables)  im=${im.iri}")
            System.out.println(s"convert from OWL(tables): mismatch!")
          }
        }
      }

      // List of module IRIs

      _ <- conversions.modules match {
        case Some(file) =>
          internal
            .writeModuleIRIs(ts.map { case (m, _) => tables.taggedTypes.iri(m.iri.toString) }, file)

        case None =>
          \/-(())
      }

      // 1) Convert from OMF/OWLAPI => OML Tables

      _ <- if (conversions.toOMLZip || conversions.isEmpty)
        toTables(outStore, ts)
      else
        \/-(())

      // 2) Convert from OML Tables => OML Resolver

      extents <- ResolverUtilities.resolveTables(
        ResolverUtilities.initializeResolver(),
        ts.map { case (m, t) => tables.taggedTypes.iri(m.iri.toString) -> t})

      // 3) Convert from OML Resolver => OML Textual Concrete Syntax

      _ <- if (conversions.toText) {
        internal
          .toText(outCatalog, extents)
          .leftMap(_.toThrowables)
      } else
        \/-(())

      // 4) Convert from OML Resolver => OMF/OWLAPI again

      _ <- if (conversions.toOWL) {
        for {
          _ <- internal
            .OMLResolver2Ontology
            .convert(extents, outStore)
          _ <- conversions.fuseki match {
            case None =>
              \/-(())
            case Some(fuseki) =>
              internal.tdbUpload(outCatalog, fuseki)
          }
        } yield ()
      } else
        \/-(())

      // 5) Convert from OML Tables => SQL

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

    } yield outCat -> ts.map { case (m, t) => tables.taggedTypes.iri(m.iri.toString) -> t }

    result
  }

  protected def toTables(outStore: OWLAPIOMFGraphStore, ts: Seq[(ImmutableModule, OMLSpecificationTables)])
  : OMFError.Throwables \/ Unit
  = ts.foldLeft {
    ().right[OMFError.Throwables]
  } { case (acc, (im, t)) =>

    for {
      _ <- acc
      tURI = outStore.catalogIRIMapper.resolveIRI(im.iri, saveResolutionStrategyForOMLTables).toURI
      tFile = Paths.get(tURI).toFile

      _ <- OMLSpecificationTables
        .saveOMLSpecificationTables(t, tFile)
        .toDisjunction
        .leftMap(Set[java.lang.Throwable](_))

      _ = System.out.println(s"Saved oml.tables in: $tFile")

    } yield ()
  }

}