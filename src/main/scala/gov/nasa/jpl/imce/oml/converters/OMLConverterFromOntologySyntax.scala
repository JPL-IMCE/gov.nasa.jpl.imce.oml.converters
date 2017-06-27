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
import java.lang.System
import java.nio.file.Paths

import gov.nasa.jpl.imce.oml.converters.utils.OMLResourceSet
import gov.nasa.jpl.imce.oml.model.extensions.{OMLCatalog, OMLCatalogManager, OMLExtensions}
import gov.nasa.jpl.imce.oml.resolver
import gov.nasa.jpl.imce.oml.resolver.impl.OMLResolvedFactoryImpl
import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import gov.nasa.jpl.imce.oml.uuid.JVMUUIDGenerator
import gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFLoader
import gov.nasa.jpl.omf.scala.binding.owlapi.descriptions.ImmutableDescriptionBox
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{ImmutableBundle, ImmutableTerminologyGraph}
import gov.nasa.jpl.omf.scala.binding.owlapi.{OWLAPIOMF, OWLAPIOMFGraphStore, OWLAPIOMFModule, emptyMutable2ImmutableModuleMap}
import gov.nasa.jpl.omf.scala.core.OMFError
import gov.nasa.jpl.omf.scala.core.tables.OMFTabularExport
import org.apache.xml.resolver.tools.CatalogResolver
import org.eclipse.emf.common.util.{URI => EURI}
import org.eclipse.xtext.resource.XtextResourceSet
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{IRI, OWLOntologyAlreadyExistsException, OWLOntologyLoaderListener}

import scalax.collection.Graph
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._

import scala.collection.immutable._
import scala.util.control.Exception._
import scala.{Boolean, None, Option, Some, StringContext, Unit}
import scala.Predef.{augmentString,ArrowAssoc,String}
import scalaz._
import Scalaz._

object OMLConverterFromOntologySyntax {

  def convertIRIs(omlCatalogFile: File,
                  metadataFile: Option[File],
                  omlIRIs: List[String]): Unit = {
    val rs = OMLResourceSet.initializeResourceSet()

    (Option.apply(OMLExtensions.getOrCreateCatalogManager(rs)),
      Option.apply(OMLExtensions.getOrCreateCatalogResolver(rs)),
      Option.apply(OMLExtensions.getCatalog(rs))) match {
      case (Some(cm: OMLCatalogManager),
      Some(cr: CatalogResolver),
      Some(cat: OMLCatalog)) =>
        nonFatalCatch[Unit]
          .withApply { (t: java.lang.Throwable) =>
            System.err.println(
              s"OMLConverterFromOntologySyntax: ${t.getMessage}")
            t.printStackTrace(System.err)
            System.exit(-1)
          }
          .apply {
            implicit val omfStore = OWLAPIOMFGraphStore.initGraphStore(
              OWLAPIOMFModule
                .owlAPIOMFModule(cm, withOMFMetadata = false)
                .valueOr { (errors: Set[java.lang.Throwable]) =>
                  val message = s"${errors.size} errors" + errors
                    .map(_.getMessage)
                    .toList
                    .mkString("\n => ", "\n => ", "\n")
                  throw new scala.IllegalArgumentException(message)
                },
              OWLManager.createOWLOntologyManager(),
              cr,
              cat
            )

            omfStore.catalogIRIMapper
              .parseCatalog(omlCatalogFile.toURI)
              .valueOr { (errors: Set[java.lang.Throwable]) =>
                val message = s"${errors.size} errors" + errors
                  .map(_.getMessage)
                  .toList
                  .mkString("\n => ", "\n => ", "\n")
                throw new scala.IllegalArgumentException(message)
              }

            convert(rs, omlIRIs.map(IRI.create), cat, omlCatalogFile, metadataFile) match {
              case -\/(errors) =>
                System.err.println(s"${errors.size} errors:")
                errors.foreach { e =>
                  System.err.println(e.getMessage)
                  e.printStackTrace(System.err)
                  System.err.println()
                }
                throw errors.head
              case _ =>
                ()
            }
          }
      case _ =>
        System.err.println(
          s"There should have been a catalog on the resource set!")
        System.exit(-1)
    }
  }

  def isBundle(m: OWLAPIOMF#Module) : Boolean = m match {
    case _: OWLAPIOMF#Bundle =>
      true
    case _ =>
      false
  }

  def isBuiltin(m: OWLAPIOMF#Module): Boolean = m match {
    case _: OWLAPIOMF#Bundle =>
      false
    case _: OWLAPIOMF#DescriptionBox =>
      false
    case _ =>
      val siri = m.iri.toString
      siri.startsWith("http://imce.jpl.nasa.gov/oml") ||
        "http://www.w3.org/2002/07/owl" == siri ||
        "http://www.w3.org/2001/XMLSchema" == siri ||
        "http://www.w3.org/2000/01/rdf-schema" == siri ||
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#" == siri ||
        "http://www.w3.org/2003/11/swrl" == siri
  }

  def convert
  (rs: XtextResourceSet,
   omlIRIs: Seq[IRI],
   cat: OMLCatalog,
   catalogFile: File,
   metadataFile: Option[File])
  (implicit omfStore: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ Unit
  = {
    implicit val ops = omfStore.ops

    val convertibleIRIs = omlIRIs.filterNot(omfStore.isBuiltInIRI)
    for {
      m2i <-
      convertibleIRIs
        .foldLeft {
          emptyMutable2ImmutableModuleMap.right[OMFError.Throwables]
        } { case (acc, omlIRI) =>
          for {
            m2i <- acc
            loadResult <- omfStore.loadModule(m2i, omlIRI)
            (_, next) = loadResult
          } yield next
        }

      is = m2i.values

      drc = omfStore.getBuildInDatatypeMap

      builtInEdges = drc.builtInDatatypeModules.flatMap { f =>
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
            fI.sig.importedTerminologies.flatMap {
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
        val gk = (gi /: i.sig.importedTerminologies) {
          case (gj, itbox: OWLAPIOMF#ImmutableTerminologyBox) =>
            System.out.println(s"convert from OWL(tbox) ${i.iri} ~> ${itbox.iri}")
            gj + i ~> itbox
          case (gj, _) =>
            gj
        }
        val gl = (gk /: i.sig.importedDescriptions) {
          case (gh, idbox: OWLAPIOMF#ImmutableDescriptionBox) =>
            System.out.println(s"convert from OWL(dbox) ${i.iri} ~> ${idbox.iri}")
            gh + i ~> idbox
          case (gh, _) =>
            gh
        }
        gl
      }

      metadataNodes =
      g3
        .nodes
        .map { n =>
          val m = n.value
          val prov = if (isBuiltin(m))
            metadata.OMLMetadataProvenance.OMLBuiltinModuleProvenance
          else if (isBundle(m) || n.findPredecessor(x => isBundle(x.value)).isDefined)
            metadata.OMLMetadataProvenance.OMLBundleModuleProvenance
          else if (n.findSuccessor(x => isBundle(x.value)).isDefined)
            metadata.OMLMetadataProvenance.OMLExtensionModuleProvenance
          else
            metadata.OMLMetadataProvenance.OMLOtherModuleProvenance

          val catalogDir: String = catalogFile.getAbsoluteFile.getParent

          val filename = cat.resolveURI(m.iri.toString).stripPrefix("file:").stripPrefix(catalogDir).stripPrefix("/")

          metadata.OMLConvertedModule(
            iri = metadata.OMLMetadataString.ModuleIRI(m.iri.toString),
            filename = metadata.OMLMetadataString.RelativeFilename(filename),
            provenance = prov)
        }
        .to[Seq]
        .sorted

      metadataEdges =
      g3
        .edges
        .map { e =>
          val (s,t) = (e.from.value, e.to.value)
          metadata.OMLConvertedModuleEdge(
            importing = metadata.OMLMetadataString.ImportingModuleIRI(s.iri.toString),
            imported = metadata.OMLMetadataString.ImportedModuleIRI(t.iri.toString))
        }
        .to[Seq]
        .sorted

      metadataGraph = metadata.OMLMetadataGraph(nodes = metadataNodes, edges = metadataEdges)

      _ <- metadataFile.fold(().right[OMFError.Throwables]) { f =>
        val s = new java.io.PrintWriter(f)
        try {
          val json = metadata.OMLMetadataGraph.encoder(metadataGraph)
          s.write(json.toString)
          System.out.println(s"# Wrote OML Metadata Graph: $metadataFile")
          ().right[OMFError.Throwables]
        } catch {
          case e: java.io.IOException =>
            Set[java.lang.Throwable](e).left
        } finally {
          s.close()
        }
      }

      gorder <- OWLAPIOMFLoader.hierarchicalTopologicalSort(Seq(g3), Seq.empty).map(_.reverse)

      _ = gorder.foreach { g =>
        val iri = g.iri.toString
        val omlIRI = if (iri.endsWith("/"))
          iri.replaceFirst("^(.*)/([a-zA-Z0-9.]+)/$", "$1/$2/$2.oml")
        else
          iri + ".oml"
        System.out.println(s"convert from OWL(gorder): ${g.iri} => $omlIRI")
      }

      tables <- OMFTabularExport.toTables[OWLAPIOMF](gorder)

      _ = {
        (gorder zip tables).foreach { case (gi, (im, ti)) =>
          if (gi.iri != im.iri) {
            System.out.println(s"convert from OWL(tables)  gi=${gi.iri}")
            System.out.println(s"convert from OWL(tables)  im=${im.iri}")
            System.out.println(s"convert from OWL(tables): mismatch!")
          }
        }
      }

      _ <- tables.foldLeft {
        ().right[OMFError.Throwables]
      } { case (acc, (im, table)) =>

        for {
          _ <- acc
          tablesURI = omfStore.catalogIRIMapper.resolveIRI(im.iri, saveResolutionStrategyForOMLTables).toURI
          tablesFile = Paths.get(tablesURI).toFile

          _ <- OMLSpecificationTables
            .saveOMLSpecificationTables(table, tablesFile)
            .toDisjunction
            .leftMap(Set[java.lang.Throwable](_))

          _ = System.out.println(s"Saved oml.tables in: $tablesFile")

        } yield ()
      }

      omlUUIDg = JVMUUIDGenerator()

      factory = OMLResolvedFactoryImpl(omlUUIDg)

      resolved <- tables.foldLeft {
        resolver.OMLTablesResolver.initializeTablesResolver(factory).right[OMFError.Throwables]
      } { case (acc, (im, table)) =>

        for {
          prev <- acc
          current = prev.copy(queue = table)

          _ = System.out.println(s"OMLTablesResolver: ${im.iri}")

          res <- resolver.OMLTablesResolver.resolve(current)
            .toDisjunction
            .leftMap(Set[java.lang.Throwable](_))

          extent = res.context

          _ <- if (!res.queue.isEmpty)
            Set[java.lang.Throwable](OMFError.omfError(
              s"Conversion of ${im.iri} incomplete:\n"+res.queue.show
            )).left[Unit]
          else
            ().right[OMFError.Throwables]

          nB = extent.bundles.size
          nG = extent.terminologyGraphs.size
          nD = extent.descriptionBoxes.size

          toBundle = nB == 1 && nG == 0 && nD == 0
          toGraph = nB == 0 && nG == 1 && nD == 0
          toDescription = nB == 0 && nG == 0 && nD == 1

          _ <- im match {
            case _: ImmutableBundle =>
              if (toBundle)
                ().right[OMFError.Throwables]
              else
                Set[java.lang.Throwable](OMFError.omfError(
                  s"Bundle conversion of ${im.iri} incomplete"
                )).left[Unit]

            case _: ImmutableTerminologyGraph =>
              if (toGraph)
                ().right[OMFError.Throwables]
              else
                Set[java.lang.Throwable](OMFError.omfError(
                  s"TerminologyGraph conversion of ${im.iri} incomplete"
                )).left[Unit]


            case _: ImmutableDescriptionBox =>
              if (toDescription)
                ().right[OMFError.Throwables]
              else
                Set[java.lang.Throwable](OMFError.omfError(
                  s"DescriptionBox conversion of ${im.iri} incomplete"
                )).left[Unit]


          }

          next = resolver.OMLTablesResolver.accumulateResultContext(res)
        } yield next
      }

      extents = resolved.otherContexts

      r2t <- extents.foldLeft {
        OMLResolver2Text().right[OMFError.Throwables]
      } { case (acc, apiExtent) =>
        for {
          prev <- acc
          next <- OMLResolver2Text.convert(apiExtent, rs, prev).leftMap(_.toThrowables)
        } yield next
      }

      extentResources = {
        r2t.mappings.map { case (iri, (_, omlExtent)) =>

          val omlIRI = if (iri.endsWith("/"))
            iri.replaceFirst("^(.*)/([a-zA-Z0-9.]+)/$","$1/$2.oml")
          else
            iri + ".oml"
          val resolvedIRI = cat.resolveURI(omlIRI)

          System.out.println(s" OML IRI = $iri")
          System.out.println(s"resolved = $resolvedIRI")

          val uri: EURI = EURI.createURI(resolvedIRI)
          val r = rs.createResource(uri)
          r.getContents.add(omlExtent)
          r
        }
      }

      _ <- (().right[OMFError.Throwables] /: extentResources) { case (acc, r) =>
        for {
          _ <- acc
          _ <- nonFatalCatch[OMFError.Throwables \/ Unit]
            .withApply { (t: java.lang.Throwable) =>
              System.err.println(
                s"OMLConverterFromOntologySyntax (Error while saving to OML): ${t.getMessage}")
              t.printStackTrace(System.err)
              Set(t).left[Unit]
            }
            .apply {
              r.save(null)
              System.out.println(s"Saved ${r.getURI}")
              ().right[OMFError.Throwables]
            }
        } yield ()
      }

    } yield ()

  }

  def convertFiles(omlCatalogFile: File,
                   metadataFile: Option[File],
                   owlFiles: List[String]): Unit = {
    val rs = OMLResourceSet.initializeResourceSet()

    (Option.apply(OMLExtensions.getOrCreateCatalogManager(rs)),
      Option.apply(OMLExtensions.getOrCreateCatalogResolver(rs)),
      Option.apply(OMLExtensions.getCatalog(rs))) match {
      case (Some(cm: OMLCatalogManager),
      Some(cr: CatalogResolver),
      Some(cat: OMLCatalog)) =>
        nonFatalCatch[Unit]
          .withApply { (t: java.lang.Throwable) =>
            System.err.println(
              s"OMLConverterFromOntologySyntax: ${t.getMessage}")
            t.printStackTrace(System.err)
            System.exit(-1)
          }
          .apply {

            implicit val omfStore = OWLAPIOMFGraphStore.initGraphStore(
              OWLAPIOMFModule
                .owlAPIOMFModule(cm, withOMFMetadata = false)
                .valueOr { (errors: Set[java.lang.Throwable]) =>
                  val message = s"${errors.size} errors" + errors
                    .map(_.getMessage)
                    .toList
                    .mkString("\n => ", "\n => ", "\n")
                  throw new scala.IllegalArgumentException(message)
                },
              OWLManager.createOWLOntologyManager(),
              cr,
              cat
            )

            omfStore.catalogIRIMapper
              .parseCatalog(omlCatalogFile.toURI)
              .valueOr { (errors: Set[java.lang.Throwable]) =>
                val message = s"${errors.size} errors" + errors
                  .map(_.getMessage)
                  .toList
                  .mkString("\n => ", "\n => ", "\n")
                throw new scala.IllegalArgumentException(message)
              }

            val owlLoaderListener = new OWLOntologyLoaderListener() {
              override def finishedLoadingOntology(event: OWLOntologyLoaderListener.LoadingFinishedEvent)
              : Unit
              = System.out.println(s"\n => Loaded ${event.getDocumentIRI} ${event.getOntologyID.getDefaultDocumentIRI}")

              override def startedLoadingOntology(event: OWLOntologyLoaderListener.LoadingStartedEvent)
              : Unit
              = System.out.println(s"\n => Loading ${event.getDocumentIRI} ...")
            }
            omfStore.ontManager.addOntologyLoaderListener(owlLoaderListener)

            val owlIRIs: Seq[IRI]
            = owlFiles.foldLeft {
              Seq.empty[IRI]
            } { case (acc, owlFile) =>
              val f = new File(owlFile)
              if (!f.exists() || !f.canRead)
                throw new java.lang.IllegalArgumentException(
                  s"omlConverter: cannot read owl file: $owlFile"
                )

              import scala.compat.java8.FunctionConverters.asJavaSupplier
              val fSupplier = asJavaSupplier[java.lang.Throwable](() =>
                throw new java.lang.IllegalArgumentException(
                  s"omlConverter: $f results in an anonymous ontology"
                ))

              val owlIRI = nonFatalCatch[Option[IRI]]
                .withApply {
                  case _: OWLOntologyAlreadyExistsException =>
                    System.out.println(s"\n => Already loaded: $f")
                    None
                  case (t: java.lang.Throwable) =>
                    System.err.println(
                      s"OMLConverterFromOntologySyntax: ${t.getMessage}")
                    t.printStackTrace(System.err)
                    System.exit(-1)
                    None
                }
                .apply {
                  val ont = omfStore.ontManager.loadOntologyFromOntologyDocument(f)
                  val docIRI = omfStore.ontManager.getOntologyDocumentIRI(ont)
                  val iri = ont.getOntologyID.getOntologyIRI.orElseThrow(fSupplier)
                  System.out.println(s"\n Ontology: $iri\n Document: $docIRI")
                  if (omfStore.isBuiltInIRI(iri))
                    None
                  else
                    Some(iri)
                }
              owlIRI match {
                case Some(iri) =>
                  acc :+ iri
                case None =>
                  acc
              }
            }

            convert(rs, owlIRIs, cat, omlCatalogFile, metadataFile) match {
              case -\/(errors) =>
                System.err.println(s"${errors.size} errors:")
                errors.foreach { e =>
                  System.err.println(e.getMessage)
                  e.printStackTrace(System.err)
                  System.err.println()
                }
                if (errors.nonEmpty)
                  throw errors.head
                else
                  System.err.println("Failed result without errors!")
              case _ =>
                ()
            }
          }
      case _ =>
        System.err.println(
          s"There should have been a catalog on the resource set!")
        System.exit(-1)
    }
  }
}
