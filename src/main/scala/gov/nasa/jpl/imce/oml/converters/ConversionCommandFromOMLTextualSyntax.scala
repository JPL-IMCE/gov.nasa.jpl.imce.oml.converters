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
import gov.nasa.jpl.imce.oml.converters.utils.{EMFProblems, FileSystemUtilities, OMLResourceSet}
import gov.nasa.jpl.imce.oml.frameless.OMLSpecificationTypedDatasets
import gov.nasa.jpl.imce.oml.model.extensions.OMLCatalog
import gov.nasa.jpl.imce.oml.resolver.{Extent2Tables, api, impl}
import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import gov.nasa.jpl.imce.oml.{tables, uuid}
import gov.nasa.jpl.imce.xml.catalog.scope.CatalogScope
import gov.nasa.jpl.omf.scala.core.OMFError
import org.apache.spark.sql.{SQLContext, SparkSession}

import scala.collection.immutable.{Iterable, List, Seq, Set}
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

        omlFileScope = omlCatalogScope.omlFiles.values.to[Iterable]

        fileModules <- OMLResourceSet.loadOMLResources(in_rs, inDir, omlFileScope)
        _ = System.out.println(s"# Loaded all OML resources (no EcoreUtil.resolveAll!).")

        omlUUIDg = uuid.JVMUUIDGenerator()
        factory: api.OMLResolvedFactory = impl.OMLResolvedFactoryImpl(omlUUIDg)

        o2rMap_sorted <- internal.OMLText2Resolver.convert(fileModules)(factory)

        (_, sortedAPIModuleExtents) = o2rMap_sorted

        // List of module IRIs

        _ <- conversions.modules match {
          case Some(file) =>
            internal
              .writeModuleIRIs(sortedAPIModuleExtents.map { case (m, _) => m.iri }, file)
              .leftMap(ts => EMFProblems(exceptions = ts.to[List]))

          case None =>
            ().right[EMFProblems]
        }

        // Convert to tables

        _ <- if (conversions.toOMLZip)
          sortedAPIModuleExtents.foldLeft[EMFProblems \/ Unit](\/-(())) {
            case (acc, (_, extent)) =>
              for {
                _ <- acc
                m <- extent.singleModule match {
                  case Success(module) =>
                    \/-(module)
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

        _ <- if (conversions.toOWL) {
          for {
           _ <- internal
            .OMLResolver2Ontology
            .convert(sortedAPIModuleExtents.map(_._2), outStore)
            .leftMap(ts => EMFProblems(exceptions = ts.to[List]))
            _ <- conversions.fuseki match {
              case None =>
                ().right[EMFProblems]
              case Some(fuseki) =>
                internal
                  .tdbUpload(outCatalog, fuseki)
                  .leftMap(ts => EMFProblems(exceptions = ts.to[List]))
            }
          } yield ()
        } else
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
