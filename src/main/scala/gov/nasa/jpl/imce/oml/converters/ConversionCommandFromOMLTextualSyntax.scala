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
import gov.nasa.jpl.imce.oml.converters.utils.{EMFProblems, OMLResourceSet}
import gov.nasa.jpl.imce.oml.frameless.OMLSpecificationTypedDatasets
import gov.nasa.jpl.imce.oml.resolver.{Extent2Tables, FileSystemUtilities, api, impl}
import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import gov.nasa.jpl.imce.oml.uuid
import gov.nasa.jpl.omf.scala.core.OMFError
import org.apache.spark.SparkConf
import org.apache.spark.sql.SparkSession

import scala.collection.immutable.{List, Seq, Set}
import scala.util.{Failure, Success}
import scala.util.control.Exception.nonFatalCatch
import scala.{None, Some, StringContext, Unit}
import scalaz._
import Scalaz._

case object ConversionCommandFromOMLTextualSyntax extends ConversionCommand {

  override val filePredicate = FileSystemUtilities.omlTextFilePredicate _

  override def convert
  (inCatalog: Path,
   inputFiles: Seq[Path],
   outputDir: Path,
   outCatalog: Path,
   conversions: ConversionCommand.OutputConversions)
  : OMFError.Throwables \/ Unit
  = nonFatalCatch[OMFError.Throwables \/ Unit]
    .withApply {
      (t: java.lang.Throwable) =>
        -\/(Set(t))
    }
    .apply {
      val conf = new SparkConf()
        .setMaster("local")
        .setAppName(this.getClass.getSimpleName)

      implicit val spark = SparkSession
        .builder()
        .config(conf)
        .getOrCreate()
      implicit val sqlContext = spark.sqlContext

      val props = new Properties()
      props.setProperty("useSSL", "false")

      props.setProperty("dumpQueriesOnException", "true")
      props.setProperty("enablePacketDebug", "true")

      val inDir: Path = inCatalog / up

      val result = for {
        in_rs_cm_cat <- OMLResourceSet.initializeResourceSetWithCatalog(inCatalog)
        (in_rs, in_cm, in_cat) = in_rs_cm_cat

        out_store_cat <- ConversionCommand
          .createOMFStoreAndLoadCatalog(outCatalog)
          .leftMap(ts => EMFProblems(exceptions = ts.to[List]))
        (outStore, outCat) = out_store_cat

        fileExtents <- OMLResourceSet.loadOMLResources(in_rs, inDir, inputFiles)
        _ = EcoreUtil.resolveAll(in_rs)

        omlUUIDg = uuid.JVMUUIDGenerator()
        factory: api.OMLResolvedFactory = impl.OMLResolvedFactoryImpl(omlUUIDg)

        o2rMap_sorted <- internal.OMLText2Resolver.convert(fileExtents)(factory)

        (o2rMap, sortedModuleExtents) = o2rMap_sorted
        extents = sortedModuleExtents.map(_._2)

        // Convert to tables
        _ <- if (conversions.toOMLZip)
          o2rMap.foldLeft[EMFProblems \/ Unit](\/-(())) {
            case (acc, (_, o2r)) =>
              for {
                _ <- acc
                apiExtent = o2r.rextent
                tables = Extent2Tables.convert(apiExtent)
                outputFile = Path.apply(o2r.toOMLTablesFile, base = outputDir)
                _ <- OMLSpecificationTables
                  .saveOMLSpecificationTables(tables, outputFile.toIO) match {
                  case Success(_) =>
                    System.out.println(s"... saved tables: ${o2r.toOMLTablesFile} => $outputFile")
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
            .convert(extents, outStore)
            .leftMap(ts => EMFProblems(exceptions = ts.to[List]))
        else
          ().right[EMFProblems]

        // Convert to OML

        _ <- if (conversions.toText)
          internal
            .toText(outCatalog, sortedModuleExtents.map(_._2))
        else
          ().right[EMFProblems]

        // Convert to Parquet

        _ <- if (conversions.toParquet)
          internal
            .toParquet(outCatalog / up, o2rMap.map { case (_, o2r) => Extent2Tables.convert(o2r.rextent) }.to[Seq])
            .leftMap(ts => EMFProblems(exceptions = ts.to[List]))
        else
          ().right[EMFProblems]

        // Convert to SQL

        _ <- conversions.toSQL match {
          case Some(server) =>
            val tables =
              o2rMap
                .map { case (_, o2r) => Extent2Tables.convert(o2r.rextent) }
                .to[Seq]
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

      } yield ()

      result.leftMap(_.toThrowables)
    }

}
