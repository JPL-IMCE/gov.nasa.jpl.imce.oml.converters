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
import gov.nasa.jpl.imce.oml.converters.utils.{FileSystemUtilities, OMLResourceSet}
import gov.nasa.jpl.imce.oml.resolver.{Extent2Tables, api, impl}
import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import gov.nasa.jpl.imce.oml.{tables, uuid}
import gov.nasa.jpl.imce.xml.catalog.scope.CatalogScope
import gov.nasa.jpl.omf.scala.core.OMFError
import org.apache.spark.sql.{SQLContext, SparkSession}
import org.eclipse.emf.ecore.util.EcoreUtil

import scala.collection.immutable.{Iterable, Seq, Set}
import scala.util.control.Exception.nonFatalCatch
import scala.{Option, StringContext}
import scala.Predef.ArrowAssoc
import scalaz._

case object ConversionCommandFromOMLTextualSyntax extends ConversionCommand {

  override val filePredicate = FileSystemUtilities.OMLTextOrZipFilePredicate

  override def convert
  (omlCatalogScope: OMLCatalogScope,
   outputCatalog: Option[Path],
   options: OMLConverter.Options)
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ (Option[CatalogScope], Seq[(tables.taggedTypes.IRI, OMLSpecificationTables)])
  = nonFatalCatch[OMFError.Throwables \/ (Option[CatalogScope], Seq[(tables.taggedTypes.IRI, OMLSpecificationTables)])]
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

      org.eclipse.emf.ecore.resource.Resource.Factory.Registry.INSTANCE.getContentTypeToFactoryMap.put("omlzip", new gov.nasa.jpl.imce.oml.zip.OMLZipResourceFactory())
      org.eclipse.emf.ecore.resource.Resource.Factory.Registry.INSTANCE.getExtensionToFactoryMap.put("omlzip", new gov.nasa.jpl.imce.oml.zip.OMLZipResourceFactory())

      val tuple = for {
        in_rs_cm_cat <- OMLResourceSet.initializeResourceSetWithCatalog(omlCatalogScope.omlCatalogFile)
        (in_rs, in_cm, in_cat) = in_rs_cm_cat


        omlFileScope = omlCatalogScope.omlFiles.values.to[Iterable]

        beforeLoad = System.currentTimeMillis()

        fileModules <- OMLResourceSet.loadOMLResources(in_rs, inDir, omlFileScope)

        load_delta = System.currentTimeMillis() - beforeLoad
        load_ms = load_delta % 1000
        load_s = load_delta / 1000

        _ = if (options.resolveAll) {

          System.out.println(s"# Loaded all OML resources in ${load_s}s, ${load_ms}ms.")
          val beforeResolve = System.currentTimeMillis()
          EcoreUtil.resolveAll(in_rs)

          val resolve_delta = System.currentTimeMillis() - beforeResolve
          val resolve_ms = resolve_delta % 1000
          val resolve_s = resolve_delta / 1000
          System.out.println(s"# Resolved all OML resources in ${resolve_s}s, ${resolve_ms}ms.")

        } else
          System.out.println(s"# Loaded all OML resources (no EcoreUtil.resolveAll!) in ${load_s}s, ${load_ms}ms.")

        omlUUIDg = uuid.JVMUUIDGenerator()
        factory: api.OMLResolvedFactory = impl.OMLResolvedFactoryImpl(omlUUIDg)

        o2rMap_sorted <- internal.OMLText2Resolver.convert(fileModules)(factory)

        (_, sortedAPIModuleExtents) = o2rMap_sorted

        iri2tables = sortedAPIModuleExtents.map { case (m, extent) => m.iri -> Extent2Tables.convert(extent) }

        extents = sortedAPIModuleExtents.map(_._2)

      } yield (extents, iri2tables)

      internal.process(tuple.leftMap(_.toThrowables), outputCatalog, options, props)

    }
}
