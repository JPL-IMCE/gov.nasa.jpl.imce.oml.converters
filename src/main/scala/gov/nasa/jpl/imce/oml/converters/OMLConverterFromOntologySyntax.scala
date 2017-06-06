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
import gov.nasa.jpl.imce.oml.resolver.OMLTablesResolver
import gov.nasa.jpl.imce.oml.resolver.impl.OMLResolvedFactoryImpl
import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import gov.nasa.jpl.imce.oml.uuid.JVMUUIDGenerator
import gov.nasa.jpl.omf.scala.binding.owlapi.descriptions.ImmutableDescriptionBox
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{ImmutableBundle, ImmutableTerminologyGraph}
import gov.nasa.jpl.omf.scala.binding.owlapi.{OWLAPIOMF, OWLAPIOMFGraphStore, OWLAPIOMFModule, emptyMutable2ImmutableModuleMap}
import gov.nasa.jpl.omf.scala.core.OMFError
import gov.nasa.jpl.omf.scala.core.tables.OMFTabularExport
import org.apache.xml.resolver.tools.CatalogResolver
import org.eclipse.xtext.resource.XtextResourceSet
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{IRI, OWLOntologyAlreadyExistsException, OWLOntologyLoaderListener}

import scala.collection.immutable._
import scala.util.control.Exception._
import scala.{None, Option, Some, StringContext, Unit}
import scala.Predef.String
import scalaz._
import Scalaz._

object OMLConverterFromOntologySyntax {

  def convertIRIs(omlCatalog: String,
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
            val omlCatalogFile = new File(omlCatalog)

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

            convert(rs, omlIRIs.map(IRI.create)) match {
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


  def convert(rs: XtextResourceSet, omlIRIs: Seq[IRI])(
      implicit omfStore: OWLAPIOMFGraphStore)
    : OMFError.Throwables \/ Unit = {
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

      tables <- OMFTabularExport.toTables[OWLAPIOMF](m2i.values)

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

      _ <- tables.foldLeft {
        ().right[OMFError.Throwables]
      } { case (acc, (im, table)) =>

        for {
          _ <- acc
          resolver <- OMLTablesResolver
            .resolve(table, factory, null.asInstanceOf[java.util.UUID]) // @TODO Fix the API (3rd arg. isn't used)
            .toDisjunction
            .leftMap(Set[java.lang.Throwable](_))

          extent = resolver.context.extent

          _ <- if (!resolver.queue.isEmpty)
            Set[java.lang.Throwable](OMFError.omfError(
              s"Conversion of ${im.iri} incomplete:\n"+resolver.queue.show
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
        } yield ()
      }


    } yield ()

  }

  def convertFiles(omlCatalog: String,
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
            val omlCatalogFile = new File(omlCatalog)

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
              = System.out.println(s"\n => Loaded ${event.getDocumentIRI}")

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
                  val iri = ont.getOntologyID.getOntologyIRI.orElseThrow(fSupplier)
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

            convert(rs, owlIRIs) match {
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
}
