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

import ammonite.ops.{up,Path}
import org.eclipse.emf.common.util.{URI => EURI}
import org.eclipse.emf.ecore.util.EcoreUtil
import gov.nasa.jpl.imce.oml.converters.utils.{EMFProblems, OMLResourceSet}
import gov.nasa.jpl.imce.oml.resolver._
import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import gov.nasa.jpl.imce.oml.{filesystem, uuid}
import gov.nasa.jpl.omf.scala.core.OMFError

import scala.collection.immutable.{List, Map, Seq, Set}
import scala.util.{Failure, Success}
import scala.util.control.Exception.nonFatalCatch
import scala.{StringContext, Unit}
import scala.Predef.ArrowAssoc
import scalaz._
import Scalaz._

case object ConversionCommandFromOMLTextualSyntax extends ConversionCommand {

  override val filePredicate = filesystem.omlTextFilePredicate _

  override def convert(inCatalog: Path, inputFiles: Seq[Path], outputDir: Path, outCatalog: Path)
  : OMFError.Throwables \/ Unit
  = nonFatalCatch[OMFError.Throwables \/ Unit]
    .withApply {
      (t: java.lang.Throwable) =>
        -\/(Set(t))
    }
    .apply {
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

        o2rMap <- internal.OMLText2Resolver.convert(fileExtents)(factory)

        module2Extent = o2rMap.map { case (_, t2r) =>
          val modules
          : Seq[api.Module]
          = Seq.empty[api.Module] ++
            t2r.rextent.terminologyGraphs.values ++
            t2r.rextent.bundles.values ++
            t2r.rextent.descriptionBoxes.values

          scala.Predef.require(modules.size == 1)
          val m = modules.head
          m -> t2r.rextent
        }

        // Convert to tables
        _ <- o2rMap.foldLeft[EMFProblems \/ Unit](\/-(())) {
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

        modules = o2rMap.values.foldLeft(Map.empty[api.Module, api.Extent]) { case (acc, o2r) =>
          val ext = o2r.rextent
          acc ++
            ext.terminologyGraphs.values.map(_ -> ext) ++
            ext.bundles.values.map(_ -> ext) ++
            ext.descriptionBoxes.values.map(_ -> ext)
        }

        moduleEdges = o2rMap.values.foldLeft(Map.empty[api.ModuleEdge, api.Extent]) { case (acc, o2r) =>
          val ext = o2r.rextent
          acc ++
            ext.terminologyBoxAxiomByUUID.values.map(_ -> ext) ++
            ext.terminologyBundleAxiomByUUID.values.map(_ -> ext) ++
            ext.descriptionBoxExtendsClosedWorldDefinitionsByUUID.values.map(_ -> ext) ++
            ext.descriptionBoxRefinementByUUID.values.map(_ -> ext)
        }

        // Convert to OWL

        sortedModuleExtents <-
        internal
          .OMLResolver2Ontology.sortExtents(modules, moduleEdges)
          .leftMap(ts => EMFProblems(exceptions = ts.to[List]))

        // Convert to OML
        out_rs_cm_cat <- OMLResourceSet.initializeResourceSetWithCatalog(outCatalog)

        (out_rs, _, _) = out_rs_cm_cat

        r2t <- sortedModuleExtents.foldLeft {
          internal.OMLResolver2Text().right[EMFProblems]
        } { case (acc, (_, ext)) =>
          for {
            prev <- acc
            next <- internal.OMLResolver2Text.convert(ext, out_rs, prev)
          } yield next
        }

        extentResources = {
          r2t.mappings.map { case (iri, (_, omlExtent)) =>

            val omlIRI = if (iri.endsWith("/"))
              iri.replaceFirst("^(.*)/([a-zA-Z0-9.]+)/$","$1/$2.oml")
            else
              iri + ".oml"
            val resolvedIRI = outCat.resolveURI(omlIRI)

            System.out.println(s" OML IRI = $iri")
            System.out.println(s"resolved = $resolvedIRI")

            val uri: EURI = EURI.createURI(resolvedIRI)
            val r = out_rs.createResource(uri)
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
                new EMFProblems(t).left
              }
              .apply {
                r.save(null)
                System.out.println(s"Saved ${r.getURI}")
                ().right[EMFProblems]
              }
          } yield ()
        }

      } yield ()

      result.leftMap(_.toThrowables)
    }

}