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

import java.lang.{IllegalArgumentException, System}

import ammonite.ops.{Path, cp, mkdir, rm, write}
import gov.nasa.jpl.imce.oml.model
import gov.nasa.jpl.imce.oml.frameless.OMLSpecificationTypedDatasets
import gov.nasa.jpl.imce.oml.converters.utils.{EMFProblems, OMLResourceSet}
import gov.nasa.jpl.imce.oml.model.extensions.{OMLCatalog, OMLExtensions}
import gov.nasa.jpl.imce.oml.resolver
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import gov.nasa.jpl.omf.scala.core.OMFError
import org.apache.spark.SparkConf
import org.apache.spark.sql.SparkSession
import org.eclipse.emf.common.util.{URI => EURI}

import scala.collection.immutable.{Seq, Set}
import scala.{Boolean, StringContext, Unit}
import scala.Predef.{String, augmentString, require}
import scalaz._
import Scalaz._
import scala.util.{Failure, Success}
import scala.util.control.Exception.nonFatalCatch

package object internal {

  protected[converters] def makeOutputDirectoryAndCopyCatalog(deleteIfExists: Boolean, outDir: Path, inCatalog: Path)
  : OMFError.Throwables \/ Path
  = nonFatalCatch[OMFError.Throwables \/ Path]
    .withApply {
      (t: java.lang.Throwable) =>
        -\/(Set(t))
    }
    .apply {
      for {
        _ <- if (outDir.toIO.exists()) {
          if (deleteIfExists) {
            rm(outDir)
            \/-(())
          } else
            -\/(Set[java.lang.Throwable](new IllegalArgumentException(s"Output directory already exists: $outDir")))
        } else
          \/-(())
        _ = mkdir(outDir)
        outCatalog = outDir / inCatalog.segments.last
        _ = cp(inCatalog, outCatalog)
      } yield outCatalog
    }

  protected[converters] def makeOutputCatalog(deleteIfExists: Boolean, outDir: Path)
  : OMFError.Throwables \/ Path
  = nonFatalCatch[OMFError.Throwables \/ Path]
    .withApply {
      (t: java.lang.Throwable) =>
        -\/(Set(t))
    }
    .apply {
      for {
        _ <- if (outDir.toIO.exists()) {
          if (deleteIfExists) {
            rm(outDir)
            \/-(())
          } else
            -\/(Set[java.lang.Throwable](new IllegalArgumentException(s"Output directory already exists: $outDir")))
        } else
          \/-(())
        _ = mkdir(outDir)
        outCatalog = outDir / "oml.catalog.xml"
        _ = write(outCatalog, defaultOMLCatalog)
      } yield outCatalog
    }

  protected[converters] val defaultOMLCatalog
  : String
  = """<?xml version='1.0'?>
      |<catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog" prefer="public">|
      |	 <rewriteURI rewritePrefix="file:./" 							uriStartString="http://"/>
      |</catalog>
    """.stripMargin

  protected[converters] def toText(outCatalog: Path, extents: Seq[resolver.api.Extent])
  : EMFProblems \/ Unit
  = for {
    rs_cm_cat <- OMLResourceSet.initializeResourceSetWithCatalog(outCatalog)
    (rs, _, outCat) = rs_cm_cat

    r2t <- extents.foldLeft {
      internal.OMLResolver2Text().right[EMFProblems]
    } { case (acc, apiExtent) =>
      for {
        prev <- acc
        next <- internal.OMLResolver2Text.convert(apiExtent, rs, prev)
      } yield next
    }

    extentResources = {
      r2t.mappings.map { case (iri, (_, omlExtent)) =>

        import scala.compat.java8.FunctionConverters.asJavaConsumer

        val moduleNormalizer = (m: model.common.Module) =>
          OMLExtensions.normalize(m)

        val omlIRI = if (iri.endsWith("/"))
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

    _ <- (().right[EMFProblems] /: extentResources) { case (acc, r) =>
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

  protected[converters] def toParquet(folder: Path, tables: Seq[OMLSpecificationTables])
  : Set[java.lang.Throwable] \/ Unit
  = {
    val conf = new SparkConf()
      .setMaster("local")
      .setAppName(this.getClass.getSimpleName)

    implicit val spark = SparkSession
      .builder()
      .config(conf)
      .getOrCreate()
    implicit val sqlContext = spark.sqlContext

    val omlTables = tables.reduceLeft(OMLSpecificationTables.mergeTables)

    val parquetFolder = folder / "oml.parquet"
    parquetFolder.toIO.mkdirs()

    OMLSpecificationTypedDatasets
      .parquetWriteOMLSpecificationTables(omlTables, parquetFolder) match {
      case Success(_) =>
        \/-(())
      case Failure(t) =>
        -\/(Set(t))
    }
  }

  protected[converters] def resolveOutputCatalogFileWithExtension
  (outCat: OMLCatalog,
   iri: tables.taggedTypes.IRI,
   extension: String)
  : OMFError.Throwables \/ Path
  = nonFatalCatch[OMFError.Throwables \/ Path]
    .withApply { t =>
      -\/(Set[java.lang.Throwable](t))
    }
    .apply {
      val resolved = outCat.resolveURI(iri + extension)
      require(resolved.startsWith("file:"))
      \/-(Path(resolved.stripPrefix("file:")))
    }
}
