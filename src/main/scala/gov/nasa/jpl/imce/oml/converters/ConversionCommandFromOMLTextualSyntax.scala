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

import ammonite.ops.{Path, up}
import org.eclipse.emf.ecore.util.EcoreUtil
import gov.nasa.jpl.imce.oml.converters.utils.{EMFProblems, FileSystemUtilities, OMLResourceSet}
import gov.nasa.jpl.imce.oml.frameless.OMLSpecificationTypedDatasets
import gov.nasa.jpl.imce.oml.model.extensions.OMLCatalog
import gov.nasa.jpl.imce.oml.resolver.{Extent2Tables, api, impl}
import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import gov.nasa.jpl.imce.oml.{tables, uuid}
import gov.nasa.jpl.imce.xml.catalog.scope.CatalogScope
import gov.nasa.jpl.omf.scala.core.OMFError
import org.apache.spark.sql.{SQLContext, SparkSession}

import scala.collection.immutable.{List, Seq, Set}
import scala.util.{Failure, Success}
import scala.util.control.Exception.nonFatalCatch
import scala.{None, Some, StringContext, Unit}
import scala.Predef.ArrowAssoc
import scalaz._
import Scalaz._

case object ConversionCommandFromOMLTextualSyntax extends ConversionCommand {

  override val filePredicate = FileSystemUtilities.OMLTextOrZipFilePredicate

  override def convert
  (omlCatalogScope: OMLCatalogScope,
   outputDir: Path,
   outCatalog: Path,
   conversions: ConversionCommand.OutputConversions)
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ (CatalogScope, Seq[(tables.taggedTypes.IRI, OMLSpecificationTables)])
  = nonFatalCatch[OMFError.Throwables \/ (OMLCatalog, Seq[(tables.taggedTypes.IRI, OMLSpecificationTables)])]
    .withApply {
      (t: java.lang.Throwable) =>
        -\/(Set(t))
    }
    .apply {

      val props = new Properties()
      props.setProperty("useSSL", "false")

      props.setProperty("dumpQueriesOnException", "true")
      props.setProperty("enablePacketDebug", "true")

      val inDir: Path = omlCatalogScope.omlCatalogFile / up

      val result = for {
        in_rs_cm_cat <- OMLResourceSet.initializeResourceSetWithCatalog(omlCatalogScope.omlCatalogFile)
        (in_rs, in_cm, in_cat) = in_rs_cm_cat

        out_store_cat <- ConversionCommand
          .createOMFStoreAndLoadCatalog(outCatalog)
          .leftMap(ts => EMFProblems(exceptions = ts.to[List]))
        (outStore, outCat) = out_store_cat

        fileExtents <- OMLResourceSet.loadOMLResources(in_rs, inDir, omlCatalogScope.omlFiles)
        _ = EcoreUtil.resolveAll(in_rs)

        omlUUIDg = uuid.JVMUUIDGenerator()
        factory: api.OMLResolvedFactory = impl.OMLResolvedFactoryImpl(omlUUIDg)

        o2rMap_sorted <- internal.OMLText2Resolver.convert(fileExtents)(factory)

        (_, sortedAPIModuleExtents) = o2rMap_sorted

        // Convert to tables
        _ <- if (conversions.toOMLZip)
          sortedAPIModuleExtents.foldLeft[EMFProblems \/ Unit](\/-(())) {
            case (acc, (m, extent)) =>
              for {
                _ <- acc
                m <- extent.singleModule match {
                  case Success(m) =>
                    \/-(m)
                  case Failure(t) =>
                    -\/(new EMFProblems(t))
                }

                outputFile <- internal
                  .resolveOutputCatalogFileWithExtension(outCat, m.iri, ".omlzip")
                  .leftMap(ts => EMFProblems(exceptions = ts.to[List]))

                tables = Extent2Tables.convert(extent)
                _ <- OMLSpecificationTables
                  .saveOMLSpecificationTables(tables, outputFile.toIO) match {
                  case Success(_) =>
                    System.out.println(s"... saved tables: ${m.iri} => $outputFile")
                    \/-(())
                  case Failure(t) =>
                    -\/(new EMFProblems(t))
                }
              } yield ()
          }
        else
          ().right[EMFProblems]

        // Convert to OWL

        _ <- if (conversions.toOWL)
          internal
            .OMLResolver2Ontology
            .convert(sortedAPIModuleExtents.map(_._2), outStore)
            .leftMap(ts => EMFProblems(exceptions = ts.to[List]))
        else
          ().right[EMFProblems]

        // Convert to OML

        _ <- if (conversions.toText)
          internal
            .toText(outCatalog, sortedAPIModuleExtents.map(_._2))
        else
          ().right[EMFProblems]

        // Convert to SQL

        _ <- conversions.toSQL match {
          case Some(server) =>
            val tables =
              sortedAPIModuleExtents
                .map { case (_, extent) => Extent2Tables.convert(extent) }
                .reduceLeft(OMLSpecificationTables.mergeTables)
            OMLSpecificationTypedDatasets
              .sqlWriteOMLSpecificationTables(tables, server, props) match {
              case Success(_) =>
                ().right[EMFProblems]
              case Failure(t) =>
                EMFProblems(exceptions = List(t)).left
            }
          case None =>
            ().right[EMFProblems]
        }

      } yield outCat -> sortedAPIModuleExtents.map { case (m, extent) => m.iri -> Extent2Tables.convert(extent) }

      result.leftMap(_.toThrowables)
    }

}
