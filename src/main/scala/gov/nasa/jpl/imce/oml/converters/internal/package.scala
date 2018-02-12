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

import java.io.File
import java.lang.{IllegalArgumentException, System}
import java.net.URL

import ammonite.ops.{%%, Path, cp, mkdir, pwd, rm, up, write}
import gov.nasa.jpl.imce.oml.model
import gov.nasa.jpl.imce.oml.frameless.OMLSpecificationTypedDatasets
import gov.nasa.jpl.imce.oml.converters.utils.{
  EMFProblems,
  FileSystemUtilities,
  OMLResourceSet
}
import gov.nasa.jpl.imce.oml.model.extensions.OMLExtensions
import gov.nasa.jpl.imce.oml.resolver
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import gov.nasa.jpl.imce.xml.catalog.scope.CatalogScope
import gov.nasa.jpl.omf.scala.core.OMFError
import org.apache.spark.SparkConf
import org.apache.spark.sql.{SQLContext, SparkSession}
import org.eclipse.emf.common.util.{URI => EURI}

import scala.collection.immutable.{Seq, Set}
import scala.{Boolean, Left, None, Option, Right, Some, StringContext, Unit}
import scala.Predef.{String, augmentString, require}
import scalaz._
import Scalaz._
import scala.util.control.Exception.nonFatalCatch

package object internal {

  def showErrors(ts: Set[java.lang.Throwable]): Unit = {
    System.err.println(s"### ${ts.size} Conversion Errors! ###")
    ts.foreach { t =>
      System.err.println(t.getMessage)
      t.printStackTrace(System.err)
    }
    System.exit(-1)
  }

  /**
    * See https://spark.apache.org/docs/latest/monitoring.html
    */
  protected[converters] def sparkConf(appName: String): SparkConf = {
    val dir = new File("/tmp/spark-events")
    if (!dir.exists()) {
      dir.mkdir()
    }

    if (!dir.canWrite)
      throw new IllegalArgumentException(
        "The folder /tmp/spark-events must be writable!")

    new SparkConf()
      .setMaster("local")
      .setAppName(appName)
      .set("spark.eventLog.enabled", "true")
  }

  protected[converters] def makeOutputDirectoryAndCopyCatalogIfNoOutputCatalog(
      deleteIfExists: Boolean,
      outDir: Option[Path],
      inCatalog: Path,
      outCatalog: Option[Path]): OMFError.Throwables \/ Path =
    nonFatalCatch[OMFError.Throwables \/ Path]
      .withApply { (t: java.lang.Throwable) =>
        -\/(Set(t))
      }
      .apply {
        (outDir, outCatalog) match {
          case (Some(dir), None) =>
            if (dir.toIO.exists) {
              if (deleteIfExists) {
                rm(dir)
                mkdir(dir)
                val outCat = dir / inCatalog.segments.last
                cp(inCatalog, outCat)
                \/-(outCat)
              } else
                -\/(
                  Set[java.lang.Throwable](new IllegalArgumentException(
                    s"Output directory already exists: $dir")))
            } else {
              mkdir(dir)
              val outCat = dir / inCatalog.segments.last
              cp(inCatalog, outCat)
              \/-(outCat)
            }

          case (None, Some(outCat)) =>
            val tmp = Path(File.createTempFile("omlConverter", "xml"))
            cp(outCat, tmp)
            val dir = outCat / up
            rm(dir)
            mkdir(dir)
            cp(tmp, outCat)
            \/-(outCat)

          case (Some(dir), Some(outCat)) =>
            -\/(Set[java.lang.Throwable](new IllegalArgumentException(
              s"Output is ambiguous: --output $dir and --output:catalog $outCat")))

          case (None, None) =>
            -\/(Set[java.lang.Throwable](new IllegalArgumentException(
              s"Must specify either --output <dir> or --output:catalog <catalog>")))

        }
      }

  protected[converters] def makeOutputCatalogIfNeeded(
      deleteIfExists: Boolean,
      outDir: Option[Path],
      outCatalog: Option[Path]): OMFError.Throwables \/ Path =
    nonFatalCatch[OMFError.Throwables \/ Path]
      .withApply { (t: java.lang.Throwable) =>
        -\/(Set(t))
      }
      .apply {
        (outDir, outCatalog) match {
          case (Some(dir), None) =>
            if (dir.toIO.exists) {
              if (deleteIfExists) {
                rm(dir)
                mkdir(dir)
                val outCat = dir / "oml.catalog.xml"
                write(outCat, defaultOMLCatalog)
                \/-(outCat)
              } else
                -\/(
                  Set[java.lang.Throwable](new IllegalArgumentException(
                    s"Output directory already exists: $dir")))
            } else {
              mkdir(dir)
              val outCat = dir / "oml.catalog.xml"
              write(outCat, defaultOMLCatalog)
              \/-(outCat)
            }

          case (None, Some(outCat)) =>
            val tmp = Path(File.createTempFile("omlConverter", "xml"))
            cp(outCat, tmp)
            val dir = outCat / up
            rm(dir)
            mkdir(dir)
            cp(tmp, outCat)
            \/-(outCat)

          case (Some(dir), Some(outCat)) =>
            -\/(Set[java.lang.Throwable](new IllegalArgumentException(
              s"Output is ambiguous: --output $dir and --output:catalog $outCat")))

          case (None, None) =>
            -\/(Set[java.lang.Throwable](new IllegalArgumentException(
              s"Must specify either --output <dir> or --output:catalog <catalog>")))

        }
      }

  protected[converters] val defaultOMLCatalog: String =
    """<?xml version='1.0'?>
      |<catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog" prefer="public">|
      |	 <rewriteURI rewritePrefix="file:./" 							uriStartString="http://"/>
      |</catalog>
    """.stripMargin

  protected[converters] def toText(
      outCatalog: Path,
      extents: Seq[resolver.api.Extent]): EMFProblems \/ Unit =
    for {
      rs_cm_cat <- OMLResourceSet.initializeResourceSetWithCatalog(outCatalog)
      (rs, _, outCat) = rs_cm_cat

      r2t <- extents.foldLeft {
        internal.OMLResolver2Text().right[EMFProblems]
      } {
        case (acc, apiExtent) =>
          for {
            prev <- acc
            next <- internal.OMLResolver2Text.convert(apiExtent, rs, prev)
          } yield next
      }

      extentResources = {
        r2t.mappings.map {
          case (iri, (_, omlExtent)) =>
            import scala.compat.java8.FunctionConverters.asJavaConsumer

            val moduleNormalizer =
              (m: model.common.Module) => OMLExtensions.normalize(m)

            val omlIRI =
              if (iri.endsWith("/"))
                iri.replaceFirst("^(.*)/([a-zA-Z0-9.]+)/$", "$1/$2.oml")
              else
                iri + ".oml"
            val resolvedIRI = outCat.resolveURI(omlIRI)
            val uri: EURI = EURI.createURI(resolvedIRI)
            val r = rs.createResource(uri)
            omlExtent.getModules.forEach(asJavaConsumer(moduleNormalizer))
            r.getContents.add(omlExtent)
            r
        }
      }

      _ <- (().right[EMFProblems] /: extentResources) {
        case (acc, r) =>
          for {
            _ <- acc
            _ <- nonFatalCatch[EMFProblems \/ Unit]
              .withApply { (t: java.lang.Throwable) =>
                System.err.println(
                  s"OMLConverterFromOntologySyntax (Error while saving to OML): ${t.getMessage}")
                t.printStackTrace(System.err)
                new EMFProblems(t).left[Unit]
              }
              .apply {
                r.save(null)
                System.out.println(s"Saved ${r.getURI}")
                ().right[EMFProblems]
              }
          } yield ()
      }
    } yield ()

  protected[converters] def toParquet(
      conversions: ConversionCommand.OutputConversions,
      outCat: CatalogScope,
      folder: Path,
      ts: Seq[(tables.taggedTypes.IRI, OMLSpecificationTables)])(
      implicit spark: SparkSession,
      sqlContext: SQLContext): Unit = {
    if (conversions.toParquetAggregate) {
      val omlTables =
        ts.map(_._2).reduceLeft(OMLSpecificationTables.mergeTables)
      val parquetFolder = folder / "oml.parquet"
      parquetFolder.toIO.mkdirs()
      System.out.println(
        s"Saving aggregate of all OML tables in $parquetFolder")
      OMLSpecificationTypedDatasets.parquetWriteOMLSpecificationTables(
        omlTables,
        parquetFolder)
    }

    if (conversions.toParquetEach) {
      ts.foreach {
        case (iri, t) =>
          val resolved = outCat.resolveURI(iri)
          require(resolved.startsWith("file:"))

          val tDir = new File(resolved.stripPrefix("file:") + "/oml.parquet")
          tDir.mkdirs()

          System.out.println(s"Saving $iri as $tDir")
          OMLSpecificationTypedDatasets.parquetWriteOMLSpecificationTables(
            t,
            Path(tDir))
      }
    }
  }

  protected[converters] def resolveOutputCatalogFileWithExtension(
      outCat: CatalogScope,
      iri: tables.taggedTypes.IRI,
      extension: String): OMFError.Throwables \/ Path =
    nonFatalCatch[OMFError.Throwables \/ Path]
      .withApply { t =>
        -\/(Set[java.lang.Throwable](t))
      }
      .apply {
        outCat.resolveURIWithExtension(iri.toString, extension) match {
          case Some(resolved) =>
            resolved.right
          case None =>
            Set[java.lang.Throwable](
              new IllegalArgumentException(
                s"No catalog rewrite for: $iri"
              )).left
        }
      }

  protected[converters] def writeModuleIRIs(
      iris: Seq[tables.taggedTypes.IRI],
      file: Path): OMFError.Throwables \/ Unit =
    nonFatalCatch[OMFError.Throwables \/ Unit]
      .withApply(t => -\/(Set[java.lang.Throwable](t)))
      .apply {
        implicit val iriOrdering: scala.Ordering[tables.taggedTypes.IRI] =
          new scala.Ordering[tables.taggedTypes.IRI] {
            def compare(x: tables.taggedTypes.IRI,
                        y: tables.taggedTypes.IRI): scala.Int = x.compareTo(y)
          }

        write.over(file, iris.sorted.mkString("\n"))
        System.out.println(s"# Wrote ${iris.size} OML module IRIs to $file")
        ().right
      }

  protected[converters] def tdbUpload(
      catalog: Path,
      fuseki_dataset: URL): OMFError.Throwables \/ Unit = {
    implicit val here: Path = pwd
    import io.circe.optics.JsonPath._

    for {
      url <- fuseki_dataset.toExternalForm.right[OMFError.Throwables]
      last = url.lastIndexOf('/')
      dsSlashName = url.substring(last)
      dsQuery = s"${url.substring(0, 1 + last)}$$/datasets$dsSlashName"

      dsInfo <- io.circe.parser.parse(%%("curl", "--silent", "-X", "GET", dsQuery).out.string) match {
        case Right(j) =>
          \/-(j)
        case Left(error) =>
          -\/(Set[java.lang.Throwable](error))
      }
      _ = System.out.println(s"# Fuseki dataset info for upload:\n${dsInfo.noSpaces}\n")
      // get the value of the field ".ds.name" and verify it's identical to dsSlashName
      _ <- root.`ds.name`.string.getOption(dsInfo) match {
        case Some(ds_name) =>
          if (ds_name == dsSlashName)
            \/-(())
          else
            -\/(Set[java.lang.Throwable](new IllegalArgumentException(
              s"Fuseki server $fuseki_dataset => dataset mismatch, got: $ds_name, but expected: $dsSlashName"
            )))
        case None =>
          -\/(
            Set[java.lang.Throwable](new IllegalArgumentException(
              s"Fuseki server $fuseki_dataset does not have a dataset: $dsSlashName"
            )))
      }
      owlCatalogScope <- OMLCatalogScope.toOMLCatalogScope(
        catalog,
        FileSystemUtilities.OMLOntologyFilePredicate,
        None)

      _ <- owlCatalogScope.omlFiles.par.aggregate[OMFError.Throwables \/ Unit](
        z = ().right[OMFError.Throwables])(
        seqop = tdbSeqUpload(url + "/data"),
        combop = tdbComboUpload
      )
    } yield ()
  }

  protected[converters] def tdbSeqUpload(dsData: String)(
      acc: OMFError.Throwables \/ Unit,
      iri2path: (tables.taggedTypes.IRI, Path)): OMFError.Throwables \/ Unit =
    nonFatalCatch[OMFError.Throwables \/ Unit]
      .withApply(t => -\/(Set[java.lang.Throwable](t)))
      .apply {
        implicit val here: Path = pwd
        for {
          _ <- acc
          (iri, omlFile) = (iri2path._1, iri2path._2)
          _ = System.out.println(
            s"# s-put -v $dsData ${iri.toString} ${omlFile.toString()}")
          res = %%("s-put",
                   "-v",
                   dsData,
                   iri.toString,
                   omlFile.toString())
          _ <- if (0 == res.exitCode)
            \/-(())
          else
            -\/(
              Set[java.lang.Throwable](
                new java.io.IOException(
                  s"""Error in command:
                 |
                 |s-put $dsData $iri $omlFile
                 |
                 |$res
               """.stripMargin
                )))
        } yield ()
      }

  protected[converters] def tdbComboUpload(
      acc1: OMFError.Throwables \/ Unit,
      acc2: OMFError.Throwables \/ Unit): OMFError.Throwables \/ Unit =
    (acc1, acc2) match {
      case (\/-(_), \/-(_)) =>
        \/-(())
      case (-\/(errors1), \/-(_)) =>
        -\/(errors1)
      case (\/-(_), -\/(errors2)) =>
        -\/(errors2)
      case (-\/(errors1), -\/(errors2)) =>
        -\/(errors1 ++ errors2)
    }
}
