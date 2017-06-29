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

import gov.nasa.jpl.imce.oml.converters.utils.{EMFProblems, OMLResourceSet}
import gov.nasa.jpl.imce.oml.model.common.Extent
import gov.nasa.jpl.imce.oml.model.extensions.{OMLCatalog, OMLCatalogManager, OMLExtensions}
import gov.nasa.jpl.imce.oml.resolver._
import gov.nasa.jpl.imce.oml.tables.{ClosedWorldDesignations, Final, OMLSpecificationTables, OpenWorldDefinitions, Partial, AnnotationProperty => TAnnotationProperty}
import gov.nasa.jpl.imce.oml.uuid
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{ImmutableTerminologyBox => OWLAPIImmutableTerminologyBox}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{MutableBundle => OWLAPIMutableBundle}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{MutableTerminologyBox => OWLAPIMutableTerminologyBox}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{MutableTerminologyGraph => OWLAPIMutableTerminologyGraph}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.{Term => OWLAPITerm}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms.{Concept => OWLAPIConcept, ConceptualEntity => OWLAPIConceptualEntity, DataRange => OWLAPIDataRange, Entity => OWLAPIEntity, EntityScalarDataProperty => OWLAPIEntityScalarDataProperty, EntityStructuredDataProperty => OWLAPIEntityStructuredDataProperty, ReifiedRelationship => OWLAPIReifiedRelationship, ScalarDataProperty => OWLAPIScalarDataProperty, Structure => OWLAPIStructure, StructuredDataProperty => OWLAPIStructuredDataProperty, UnreifiedRelationship => OWLAPIUnreifiedRelationship}
import gov.nasa.jpl.omf.scala.binding.owlapi.descriptions.{ConceptInstance => OWLAPIConceptInstance, ConceptualEntitySingletonInstance => OWLAPIConceptualEntitySingletonInstance, ImmutableDescriptionBox => OWLAPIImmutableDescriptionBox, MutableDescriptionBox => OWLAPIMutableDescriptionBox, ReifiedRelationshipInstance => OWLAPIReifiedRelationshipInstance, SingletonInstanceScalarDataPropertyValue => OWLAPISingletonInstanceScalarDataPropertyValue, SingletonInstanceStructuredDataPropertyContext => OWLAPISingletonInstanceStructuredDataPropertyContext}
import gov.nasa.jpl.omf.scala.core.{OMFError, OMLString, RelationshipCharacteristics, TerminologyKind}
import gov.nasa.jpl.omf.scala.core.OMLString._
import org.apache.xml.resolver.tools.CatalogResolver
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.xtext.resource.XtextResourceSet
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI

import scala.collection.immutable._
import scala.util.{Failure, Success, Try}
import scala.{Boolean, Int, None, Option, Some, StringContext, Tuple2, Tuple5, Tuple6, Unit}
import scala.Predef.{augmentString, ArrowAssoc, String}
import scalax.collection.immutable.Graph
import scalaz._
import Scalaz._
import scalax.collection.GraphEdge.NodeProduct

object OMLConverterFromTextualConcreteSyntax {

  def convert(catalogFile: File, metadataFile: Option[File], omlFiles: List[String]): Unit = {
    val rs = OMLResourceSet.initializeResourceSet()
    ( Option.apply(OMLExtensions.getOrCreateCatalogManager(rs)),
      Option.apply(OMLExtensions.getCatalog(rs))) match {
      case (Some(cm: OMLCatalogManager), Some(cat: OMLCatalog)) =>

        cat.parseCatalog(catalogFile.toURI.toURL)

        val result =
          omlFiles.foldLeft(Map.empty[Extent, File].right[EMFProblems]) {
            case (acc, omlFilename) =>
              for {
                extents <- acc
                omlFile <- {
                  val f = new File(omlFilename).getAbsoluteFile
                  if (f.exists() && f.canRead)
                    \/-(f)
                  else
                    new EMFProblems(new java.lang.IllegalArgumentException(
                      s"OMLConverterFromTextualConcreteSyntax: Cannot read OML textual concrete syntax file: " +
                      omlFilename
                    )).left
                }
                extent <- OMLResourceSet.loadOMLResource(
                  rs,
                  URI.createFileURI(omlFile.toString))
                _ <- {
                  val nbModules = extent.getModules.size
                  if (1 == nbModules)
                    ().right
                  else if (nbModules > 1)
                    new EMFProblems(new java.lang.IllegalArgumentException(
                      s"OMLConverterFromTextualConcreteSyntax: read $nbModules instead of 1 modules for $omlFile"
                    )).left
                  else
                    new EMFProblems(new java.lang.IllegalArgumentException(
                      s"OMLConverterFromTextualConcreteSyntax: no module read for $omlFile"
                    )).left
                }
              } yield extents + (extent -> omlFile)
          }

        result match {
          case -\/(emfProblems) =>
            System.out.println(emfProblems.show)
            System.exit(-1)

          case \/-(fileExtents) =>

            EcoreUtil.resolveAll(rs)

            val omlUUIDg = uuid.JVMUUIDGenerator()
            implicit val factory: api.OMLResolvedFactory =
              impl.OMLResolvedFactoryImpl(omlUUIDg)

            OMLText2Resolver.convert(fileExtents) match {
              case -\/(errors) =>
                System.out.println(errors.show)

              case \/-(o2rMap) =>
                val conversions = for {
                  _ <- o2rMap.foldLeft(Try.apply(())) {
                    case (acc, (_, o2r)) =>
                      for {
                        _ <- acc
                        apiExtent = o2r.rextent
                        tables = Extent2Tables.convert(apiExtent)
                        _ <- convertToTables(rs, cm, cat, tables, o2r.toOMLTablesFile)
                      } yield ()
                  }
                  _ <- convertToOWL(cm, cat, o2rMap, catalogFile, metadataFile)
                } yield ()
                conversions match {
                  case Success(_) =>
                    System.out.println(s"... Finished.")

                  case Failure(t) =>
                    System.err.println(t.getMessage)
                    t.printStackTrace(System.err)
                    System.exit(-1)
                }
            }
        }
      case _ =>
        System.err.println(
          s"There should have been a catalog on the resource set!")
        System.exit(-1)
    }
  }

  def convertToTables
  (rs: XtextResourceSet,
   cm: OMLCatalogManager,
   cat: OMLCatalog,
   tables: OMLSpecificationTables,
   tablesJsonZip: File): Try[Unit] = {

    System.out.println(s"... output tables: $tablesJsonZip")

    OMLSpecificationTables.saveOMLSpecificationTables(tables, tablesJsonZip)
  }

  def isBundle(m: api.Module) : Boolean = m match {
    case _: api.Bundle =>
      true
    case _ =>
      false
  }

  def isBuiltin(m: api.Module): Boolean = m match {
    case _: api.Bundle =>
      false
    case _: api.DescriptionBox =>
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


  def convertToOWL(cm: OMLCatalogManager,
                   cat: OMLCatalog,
                   o2rMap: Map[Extent, OMLText2Resolver],
                   catalogFile: File,
                   metadataFile: Option[File]): Try[Unit] = {

    System.out.println("... creating OMF Store")

    implicit val omfStore = {
      OWLAPIOMFGraphStore.initGraphStore(
        OWLAPIOMFModule.owlAPIOMFModule(cm, withOMFMetadata = false).valueOr {
          (errors: Set[java.lang.Throwable]) =>
            val message = s"${errors.size} errors" + errors
              .map(_.getMessage)
              .toList
              .mkString("\n => ", "\n => ", "\n")
            throw new scala.IllegalArgumentException(message)
        },
        OWLManager.createOWLOntologyManager(),
        new CatalogResolver(cm),
        cat
      )
    }

    implicit val ops = omfStore.ops

    import OMLOps._

    val g0: Graph[api.Module, ModuleGraphEdge] =
      Graph[api.Module, ModuleGraphEdge]()

    val g1: Graph[api.Module, ModuleGraphEdge] =
      o2rMap.foldLeft(g0) {
        case (gi, (_, o2r)) =>
          (o2r.tboxes.values ++ o2r.dboxes.values).foldLeft(gi) {
            case (gj: Graph[api.Module, ModuleGraphEdge], mi: api.Module) =>
              System.out.println(s"Node: ${mi.iri}")
              gj + mi
          }
      }

    val g2: Graph[api.Module, ModuleGraphEdge] =
      o2rMap.foldLeft(g1) { case (gi, (_, o2r)) =>
        o2r.moduleEdges.values.foldLeft(gi) {
          case (gk: Graph[api.Module, ModuleGraphEdge], me: api.ModuleEdge) =>
            implicit val ext: api.Extent = o2r.rextent
            val source: api.Module = me.sourceModule().get
            val targetIRI = me.targetModule()
            gk.toOuterNodes.find(_.iri == targetIRI).fold(gk) { target: api.Module =>
              val edge = new ModuleGraphEdge[api.Module](NodeProduct(source, target), me)
              System.out.println(s"Edge: ${source.iri} ~> ${target.iri}")
              gk + edge
            }
        }
      }

    val g = g2

    val catalogDir: String = catalogFile.getAbsoluteFile.getParent

    val metadataNodes
    : Seq[metadata.OMLConvertedModule]
    = g
      .nodes
      .map { n =>
        val m: api.Module = n.value
        val prov: metadata.OMLMetadataProvenance = if (isBuiltin(m))
          metadata.OMLMetadataProvenance.OMLBuiltinModuleProvenance
        else if (isBundle(n) || n.findPredecessor(x => isBundle(x.value)).isDefined)
          metadata.OMLMetadataProvenance.OMLBundleModuleProvenance
        else if (n.findSuccessor(x => isBundle(x.value)).isDefined)
          metadata.OMLMetadataProvenance.OMLExtensionModuleProvenance
        else
          metadata.OMLMetadataProvenance.OMLOtherModuleProvenance

        val filename = cat
          .resolveURI(m.iri)
          .stripPrefix("file:")
          .stripPrefix(catalogDir)
          .stripPrefix("/")
          .stripSuffix("/")

        metadata.OMLConvertedModule(
          iri = metadata.OMLMetadataString.ModuleIRI(m.iri),
          filename = metadata.OMLMetadataString.RelativeFilename(filename),
          provenance = prov)
      }
      .to[Seq]
      .sorted

    val metadataEdges
    : Seq[metadata.OMLConvertedModuleEdge]
    = g
      .edges
      .map { e =>
        val (s,t) = (e.from.value, e.to.value)
        metadata.OMLConvertedModuleEdge(
          importing = metadata.OMLMetadataString.ImportingModuleIRI(s.iri),
          imported = metadata.OMLMetadataString.ImportedModuleIRI(t.iri))
      }
      .to[Seq]
      .sorted

    val metadataGraph = metadata.OMLMetadataGraph(nodes = metadataNodes, edges = metadataEdges)

    metadataFile.foreach { f =>
      val s = new java.io.PrintWriter(f)
      try {
        val json = metadata.OMLMetadataGraph.encoder(metadataGraph)
        s.write(json.toString)
        System.out.println(s"# Wrote OML Metadata Graph: $metadataFile")
      } finally {
        s.close()
      }
    }

    val moduleSort
    : Seq[api.Module]
    = OWLAPIOMFLoader
      .hierarchicalTopologicalSort[api.Module, ModuleGraphEdge](Seq(g), Seq.empty) match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    val drc = omfStore.loadBuiltinDatatypeMap() match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    // map TerminologyBox
    val tbox2ont
    : Map[api.TerminologyBox, (api.Extent, OWLAPIMutableTerminologyBox)]
    = o2rMap.foldLeft {
      Map.empty[api.TerminologyBox, (api.Extent, OWLAPIMutableTerminologyBox)].right[OMFError.Throwables]
    } { case (acc1, (_, o2r)) =>
      o2r.tboxes.foldLeft(acc1) { case (acc2, (_, tbox)) =>
        for {
          prev2 <- acc2
          k = tbox.kind match {
            case OpenWorldDefinitions =>
              TerminologyKind.isDefinition
            case ClosedWorldDesignations =>
              TerminologyKind.isDesignation
          }
          i = IRI.create(tbox.iri)
          bm = drc.lookupBuiltInModule(i).flatMap {
            case mbox: OWLAPIMutableTerminologyGraph =>
              Some(mbox)
            case _ =>
              None
          }
          m <- bm.fold {
            tbox match {
              case tgraph: api.TerminologyGraph =>
                ops.makeTerminologyGraph(tgraph.uuid, LocalName(tgraph.name()), i, k)
              case tbundle: api.Bundle =>
                ops.makeBundle(tbundle.uuid, LocalName(tbundle.name()), i, k)
            }
          } { mbox =>
            mbox.right[OMFError.Throwables]
          }
          next2 = prev2 + (tbox -> (o2r.rextent -> m))
        } yield next2
      }
    } match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    // map DescriptionBox
    val dbox2ont
    : Map[api.DescriptionBox, (api.Extent, OWLAPIMutableDescriptionBox)]
    = o2rMap.foldLeft {
      Map.empty[api.DescriptionBox, (api.Extent, OWLAPIMutableDescriptionBox)].right[OMFError.Throwables]
    } { case (acc1, (_, o2r)) =>
      o2r.dboxes.foldLeft(acc1) { case (acc2, (_, dbox)) =>
        for {
          prev2 <- acc2
          k = dbox.kind match {
            case Final =>
              gov.nasa.jpl.omf.scala.core.DescriptionKind.isFinal
            case Partial =>
              gov.nasa.jpl.omf.scala.core.DescriptionKind.isPartial
          }
          i = dbox.iri
          m <- ops.makeDescriptionBox(dbox.uuid,
            LocalName(dbox.name()),
            IRI.create(i),
            k)
          next2 = prev2 + (dbox -> (o2r.rextent -> m))
        } yield next2
      }
    } match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    // Atomic terms
    val terms0
    : Map[api.Term, OWLAPITerm]
    = tbox2ont.foldLeft {
      Map.empty[api.Term, OWLAPITerm].right[OMFError.Throwables]
    } { case (acc1, (tbox, (e, mg))) =>
      implicit val ext: api.Extent = e
      for {
        step0 <- acc1
        step1 <- tbox.aspects
          .foldLeft(step0.right[OMFError.Throwables]) {
            case (acc2, a0) =>
              acc2.flatMap { prev =>
                ops.addAspect(mg, LocalName(a0.name)).flatMap { a1 =>
                  if (a0.uuid == ops.getTermUUID(a1))
                    (prev + (a0 -> a1)).right
                  else
                    Set[java.lang.Throwable](OMFError.omfError(
                      s"OMF Schema table aspect $a1 conversion " +
                        s"results in UUID mismatch: ${ops.getTermUUID(a1)}")).left
                }
              }
          }
        step2 <- tbox.concepts
          .foldLeft(step1.right[OMFError.Throwables]) {
            case (acc2, c0) =>
              acc2.flatMap { prev =>
                ops.addConcept(mg, LocalName(c0.name)).flatMap { c1 =>
                  if (c0.uuid == ops.getTermUUID(c1))
                    (prev + (c0 -> c1)).right
                  else
                    Set[java.lang.Throwable](OMFError.omfError(
                      s"OMF Schema table concept conversion from $c0 to $c1 " +
                        s"results in UUID mismatch: ${ops.getTermUUID(c1)}")).left
                }
              }
          }
        step3 <- tbox.dataranges
          .foldLeft(step2.right[OMFError.Throwables]) {
            case (acc2, dr) =>
              acc2.flatMap { prev =>
                dr match {
                  case sc0: api.Scalar =>
                    val osc1
                    : OMFError.Throwables \/ Option[OWLAPIDataRange]
                    = sc0.iri() match {
                      case Some(ti) =>
                        val oi = IRI.create(ti)
                        mg.lookupTerm(oi, recursively=false) match {
                          case Some(sc1: OWLAPIDataRange) =>
                            Some(sc1).right[OMFError.Throwables]
                          case Some(sc1) =>
                            Set[java.lang.Throwable](
                              OMFError.omfError(
                                s"OMF Schema table scalar $sc0 lookup should have produced an OWLAPIDataRange, got: $sc1"
                              )).left
                          case None =>
                            None.right[OMFError.Throwables]
                        }
                      case None =>
                        None.right[OMFError.Throwables]
                    }

                    osc1.flatMap {
                      _.fold {
                        ops.addScalarDataType(mg, LocalName(sc0.name)).flatMap {
                          sc1 =>
                            if (sc0.uuid == ops.getTermUUID(sc1))
                              (prev + (sc0 -> sc1)).right
                            else
                              Set[java.lang.Throwable](
                                OMFError.omfError(
                                  s"OMF Schema table scalar $sc0 conversion " +
                                    s"results in UUID mismatch: ${ops.getTermUUID(sc1)}")).left
                        }
                      } { sc1 =>
                        if (sc0.uuid == ops.getTermUUID(sc1))
                          (prev + (sc0 -> sc1)).right
                        else
                          Set[java.lang.Throwable](
                            OMFError.omfError(
                              s"OMF Schema table scalar $sc0 conversion " +
                                s"results in UUID mismatch: ${ops.getTermUUID(sc1)}")).left
                      }
                    }
                  case _ =>
                    // delay converting restricted data ranges until module edges have been converted
                    prev.right
                }
              }
          }

        step4 <- tbox.structures
          .foldLeft(step3.right[OMFError.Throwables]) {
            case (acc2, st0) =>
              acc2.flatMap { prev =>
                ops.addStructuredDataType(mg, LocalName(st0.name)).flatMap {
                  st1 =>
                    if (st0.uuid == ops.getTermUUID(st1))
                      (prev + (st0 -> st1)).right
                    else
                      Set[java.lang.Throwable](OMFError.omfError(
                        s"OMF Schema table structure $st0 conversion " +
                          s"results in UUID mismatch: ${ops.getTermUUID(st1)}")).left
                }
              }
          }
      } yield step4
    } match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    val dataRangeRestrictions
    : Seq[(api.DataRange, api.RestrictedDataRange, api.TerminologyBox, api.Extent, OWLAPIMutableTerminologyBox)]
    = tbox2ont.to[Seq].flatMap { case (tbox, (e, m)) =>
      import utils.EMFFilterable._
      implicit val ext: api.Extent = e
      val rdrs = tbox.dataranges.selectByKindOf { case rdr: api.RestrictedDataRange => rdr }
      rdrs.map { rdr =>
        Tuple5(rdr.restrictedRange, rdr, tbox, e, m)
      }
    }

    val terms1 = convertDataRanges(terms0, dataRangeRestrictions) match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    g.toOuterEdges.foreach {

      // ModuleEdge (TerminologyExtensionAxiom)
      case ModuleGraphEdge(
      (es: api.TerminologyBox,
       et: api.TerminologyBox),
      _: api.TerminologyExtensionAxiom) =>
        val s = tbox2ont(es)._2
        val t = tbox2ont(et)._2
        s
          .sig
          .extensions
          .find(_.extendedTerminology == t)
          .fold {
          ops.addTerminologyExtension(extendingG = s, extendedG = t) match {
            case \/-(_) =>
              ()
            case -\/(errors) =>
              return Failure(errors.head)
          }
        }{ _ =>
          ()
        }

      // ModuleEdge (TerminologyNestingAxiom)
      case ModuleGraphEdge(
      (es: api.TerminologyBox, et: api.TerminologyGraph),
      ax: api.TerminologyNestingAxiom) =>
        val s = tbox2ont(es)._2.asInstanceOf[OWLAPIMutableTerminologyGraph]
        val t = tbox2ont(et)._2.asInstanceOf[OWLAPIMutableTerminologyGraph]
        val c = terms1(ax.nestingContext).asInstanceOf[OWLAPIConcept]
        ops.addNestedTerminology(
          nestedGraph = s,
          nestingContext = c,
          nestingGraph = t) match {
          case \/-(_) =>
            ()
          case -\/(errors) =>
            return Failure(errors.head)
        }

      // ModuleEdge (ConceptDesignationTerminologyAxiom)
      case ModuleGraphEdge(
      (es: api.TerminologyGraph, et: api.TerminologyBox),
      ax: api.ConceptDesignationTerminologyAxiom) =>
        val s = tbox2ont(es)._2.asInstanceOf[OWLAPIMutableTerminologyGraph]
        val t = tbox2ont(et)._2
        val c = terms1(ax.designatedConcept).asInstanceOf[OWLAPIConcept]
        ops.addEntityConceptDesignationTerminologyAxiom(
          graph = s,
          entityConceptDesignation = c,
          designationTerminologyGraph = t) match {
          case \/-(_) =>
            ()
          case -\/(errors) =>
            return Failure(errors.head)
        }

      // ModuleEdge (BundledTerminologyAxiom)
      case ModuleGraphEdge(
      (es: api.Bundle, et: api.TerminologyBox),
      _: api.BundledTerminologyAxiom) =>
        val s = tbox2ont(es)._2.asInstanceOf[OWLAPIMutableBundle]
        val t = tbox2ont(et)._2
        ops.addBundledTerminologyAxiom(terminologyBundle = s, bundledTerminology = t) match {
          case \/-(_) =>
            ()
          case -\/(errors) =>
            return Failure(errors.head)
        }

      // ModuleEdge (DescriptionBoxExtendsClosedWorldDefinitions)
      case ModuleGraphEdge(
      (es: api.DescriptionBox, et: api.TerminologyBox),
      _: api.DescriptionBoxExtendsClosedWorldDefinitions) =>
        val s = dbox2ont(es)._2
        val t = tbox2ont(et)._2
        ops.addDescriptionBoxExtendsClosedWorldDefinitions(dbox = s, closedWorldDefinitions = t) match {
          case \/-(_) =>
            ()
          case -\/(errors) =>
            return Failure(errors.head)
        }

      // ModuleEdge (DescriptionBoxRefinement)
      case ModuleGraphEdge(
      (es: api.DescriptionBox, et: api.DescriptionBox),
      _: api.DescriptionBoxRefinement) =>
        val s = dbox2ont(es)._2
        val t = dbox2ont(et)._2
        ops.addDescriptionBoxRefinement(refiningDescriptionBox = s, refinedDescriptionBox = t) match {
          case \/-(_) =>
            ()
          case -\/(errors) =>
            return Failure(errors.head)
        }
    }

    val reifiedRelationships
    : Seq[(api.Term, api.Term, api.ReifiedRelationship, api.TerminologyBox, api.Extent, OWLAPIMutableTerminologyBox)]
    = tbox2ont.to[Seq].flatMap { case (tbox, (e, m)) =>
      implicit val ext: api.Extent = e
      tbox.reifiedRelationships.map { rr =>
        Tuple6(rr.relationDomain(), rr.relationRange(), rr, tbox, e, m)
      }
    }

    val terms2 = convertReifiedRelationships(terms1, reifiedRelationships) match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    val terms3 = convertUnreifiedRelationships(terms2, tbox2ont) match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    // Data Properties

    val e2sc
    : Map[api.EntityScalarDataProperty, OWLAPIEntityScalarDataProperty]
    = tbox2ont.foldLeft {
      Map.empty[api.EntityScalarDataProperty, OWLAPIEntityScalarDataProperty].right[OMFError.Throwables]
    } { case (acc1, (tbox, (e, mg))) =>
      implicit val ext: api.Extent = e
      tbox.entityScalarDataProperties.foldLeft(acc1) { case (acc2, e2sc0) =>
        for {
          prev <- acc2
          next <- (terms3.get(e2sc0.source()), terms3.get(e2sc0.range)) match {
            case (Some(s: OWLAPIEntity), Some(t: OWLAPIDataRange)) =>
              ops
                .addEntityScalarDataProperty(mg,
                  s,
                  t,
                  LocalName(e2sc0.name),
                  e2sc0.isIdentityCriteria).flatMap { e2sc1 =>
                if (e2sc0.uuid == ops.getTermUUID(e2sc1))
                  (prev + (e2sc0 -> e2sc1)).right
                else
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"OMF Schema table EntityScalarDataProperty $e2sc0 conversion " +
                      s"results in UUID mismatch: ${ops.getTermUUID(e2sc1)}"
                  )).left
              }
            case (s, t) =>
              Set[java.lang.Throwable](OMFError.omfError(
                s"OMF Schema table EntityScalarDataProperty $e2sc0 conversion " +
                  s.fold(s" failed to resolve conceptual entity: ${e2sc0.source().abbrevIRI()}")(_ => "") +
                  t.fold(s" failed to resolve data range: ${e2sc0.range.abbrevIRI()}")(_ => "")
              )).left
          }
        } yield next
      }
    } match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    val e2st
    : Map[api.EntityStructuredDataProperty, OWLAPIEntityStructuredDataProperty]
    = tbox2ont.foldLeft {
      Map.empty[api.EntityStructuredDataProperty, OWLAPIEntityStructuredDataProperty].right[OMFError.Throwables]
    } { case (acc1, (tbox, (e, mg))) =>
      implicit val ext: api.Extent = e
      tbox.entityStructuredDataProperties.foldLeft(acc1) { case (acc2, e2st0) =>
        for {
          prev <- acc2
          next <- (terms3.get(e2st0.source()), terms3.get(e2st0.range)) match {
            case (Some(s: OWLAPIEntity), Some(t: OWLAPIStructure)) =>
              ops
                .addEntityStructuredDataProperty(mg,
                  s,
                  t,
                  LocalName(e2st0.name),
                  e2st0.isIdentityCriteria).flatMap { e2sc1 =>
                if (e2st0.uuid == ops.getTermUUID(e2sc1))
                  (prev + (e2st0 -> e2sc1)).right
                else
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"OMF Schema table EntityStructuredDataProperty $e2st0 conversion " +
                      s"results in UUID mismatch: ${ops.getTermUUID(e2sc1)}"
                  )).left
              }
            case (s, t) =>
              Set[java.lang.Throwable](OMFError.omfError(
                s"OMF Schema table EntityStructuredDataProperty $e2st0 conversion " +
                  s.fold(s" failed to resolve conceptual entity: ${e2st0.source().abbrevIRI()}")(_ => "") +
                  t.fold(s" failed to resolve structure: ${e2st0.range.abbrevIRI()}")(_ => "")
              )).left
          }
        } yield next
      }
    } match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    val s2sc
    : Map[api.ScalarDataProperty, OWLAPIScalarDataProperty]
    = tbox2ont.foldLeft {
      Map.empty[api.ScalarDataProperty, OWLAPIScalarDataProperty].right[OMFError.Throwables]
    } { case (acc1, (tbox, (e, mg))) =>
      implicit val ext: api.Extent = e
      tbox.scalarDataProperties.foldLeft(acc1) { case (acc2, s2sc0) =>
        for {
          prev <- acc2
          next <- (terms3.get(s2sc0.source()), terms3.get(s2sc0.range)) match {
            case (Some(s: OWLAPIStructure), Some(t: OWLAPIDataRange)) =>
              ops
                .addScalarDataProperty(mg,
                  s,
                  t,
                  LocalName(s2sc0.name)).flatMap { s2sc1 =>
                if (s2sc0.uuid == ops.getTermUUID(s2sc1))
                  (prev + (s2sc0 -> s2sc1)).right
                else
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"OMF Schema table ScalarDataProperty $s2sc0 conversion " +
                      s"results in UUID mismatch: ${ops.getTermUUID(s2sc1)}"
                  )).left
              }
            case (s, t) =>
              Set[java.lang.Throwable](OMFError.omfError(
                s"OMF Schema table ScalarDataProperty $s2sc0 conversion " +
                  s.fold(s" failed to resolve structure: ${s2sc0.source().abbrevIRI()}")(_ => "") +
                  t.fold(s" failed to resolve data range: ${s2sc0.range.abbrevIRI()}")(_ => "")
              )).left
          }
        } yield next
      }
    } match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    val s2st
    : Map[api.StructuredDataProperty, OWLAPIStructuredDataProperty]
    = tbox2ont.foldLeft {
      Map.empty[api.StructuredDataProperty, OWLAPIStructuredDataProperty].right[OMFError.Throwables]
    } { case (acc1, (tbox, (e, mg))) =>
      implicit val ext: api.Extent = e
      tbox.structuredDataProperties.foldLeft(acc1) { case (acc2, s2st0) =>
        for {
          prev <- acc2
          next <- (terms3.get(s2st0.source()), terms3.get(s2st0.range)) match {
            case (Some(s: OWLAPIStructure), Some(t: OWLAPIStructure)) =>
              ops
                .addStructuredDataProperty(mg,
                  s,
                  t,
                  LocalName(s2st0.name)).flatMap { s2sc1 =>
                if (s2st0.uuid == ops.getTermUUID(s2sc1))
                  (prev + (s2st0 -> s2sc1)).right
                else
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"OMF Schema table StructuredDataProperty $s2st0 conversion " +
                      s"results in UUID mismatch: ${ops.getTermUUID(s2sc1)}"
                  )).left
              }
            case (s, t) =>
              Set[java.lang.Throwable](OMFError.omfError(
                s"OMF Schema table StructuredDataProperty $s2st0 conversion " +
                  s.fold(s" failed to resolve structure: ${s2st0.source().abbrevIRI()}")(_ => "") +
                  t.fold(s" failed to resolve structure: ${s2st0.range.abbrevIRI()}")(_ => "")
              )).left
          }
        } yield next
      }
    } match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    val allTboxElements: Map[api.Element, common.Element]
    = terms3 ++ e2sc ++ e2st ++ s2sc ++ s2st ++ tbox2ont.map { case (tbox, (_, ont_tbox)) => tbox -> ont_tbox }

    val allTboxElementsWithAxioms: Map[api.Element, common.Element]
    = tbox2ont.foldLeft(allTboxElements.right[OMFError.Throwables]) { case (acc1, (tbox, (e, ont_tbox))) =>
      tbox.boxStatements(e).foldLeft(acc1) { case (acc2, s) =>
        s match {
          case ax: api.AspectSpecializationAxiom =>
            for {
              prev <- acc2
              ont_sub <- prev.get(ax.subEntity) match {
                case Some(e: types.terms.Entity) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"AspectSpecializationAxiom subEntity: ${ax.subEntity} conversion"
                  )).left
              }
              ont_sup <- prev.get(ax.superAspect) match {
                case Some(e: types.terms.Aspect) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"AspectSpecializationAxiom superAspect: ${ax.superAspect} conversion"
                  )).left
              }
              ont_ax <- ops.addAspectSpecializationAxiom(ont_tbox, ont_sub, ont_sup)
              next = prev + (ax -> ont_ax)
            } yield next
          case ax: api.ConceptSpecializationAxiom =>
            for {
              prev <- acc2
              ont_sub <- prev.get(ax.subConcept) match {
                case Some(e: types.terms.Concept) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"ConceptSpecializationAxiom subConcept: ${ax.subConcept} conversion"
                  )).left
              }
              ont_sup <- prev.get(ax.superConcept) match {
                case Some(e: types.terms.Concept) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"ConceptSpecializationAxiom superConcept: ${ax.superConcept} conversion"
                  )).left
              }
              ont_ax <- ops.addConceptSpecializationAxiom(ont_tbox, ont_sub, ont_sup)
              next = prev + (ax -> ont_ax)
            } yield next
          case ax: api.ReifiedRelationshipSpecializationAxiom =>
            for {
              prev <- acc2
              ont_sub <- prev.get(ax.subRelationship) match {
                case Some(e: types.terms.ReifiedRelationship) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"ReifiedRelationshipSpecializationAxiom subRelationship: ${ax.subRelationship} conversion"
                  )).left
              }
              ont_sup <- prev.get(ax.superRelationship) match {
                case Some(e: types.terms.ReifiedRelationship) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"ReifiedRelationshipSpecializationAxiom superRelationship: ${ax.superRelationship} conversion"
                  )).left
              }
              ont_ax <- ops.addReifiedRelationshipSpecializationAxiom(ont_tbox, ont_sub, ont_sup)
              next = prev + (ax -> ont_ax)
            } yield next
          case ax: api.ScalarOneOfLiteralAxiom =>
            for {
              prev <- acc2
              ont_r <- prev.get(ax.axiom) match {
                case Some(e: types.terms.ScalarOneOfRestriction) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"ScalarOneOfLiteralAxiom axiom: ${ax.axiom} conversion"
                  )).left
              }
              ont_ax <- ops.addScalarOneOfLiteralAxiom(ont_tbox, ont_r, ax.value)
              next = prev + (ax -> ont_ax)
            } yield next
          case ax: api.EntityScalarDataPropertyExistentialRestrictionAxiom =>
            for {
              prev <- acc2
              ont_e <- prev.get(ax.restrictedEntity) match {
                case Some(e: types.terms.Entity) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"EntityScalarDataPropertyExistentialRestrictionAxiom restrictedEntity: ${ax.restrictedEntity} conversion"
                  )).left
              }
              ont_dp <- prev.get(ax.scalarProperty) match {
                case Some(e: types.terms.EntityScalarDataProperty) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"EntityScalarDataPropertyExistentialRestrictionAxiom scalarProperty: ${ax.scalarProperty} conversion"
                  )).left
              }
              ont_dr <- prev.get(ax.scalarRestriction) match {
                case Some(e: types.terms.DataRange) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"EntityScalarDataPropertyExistentialRestrictionAxiom scalarRestriction: ${ax.scalarRestriction} conversion"
                  )).left
              }
              ont_ax <- ops.addEntityScalarDataPropertyExistentialRestrictionAxiom(ont_tbox, ont_e, ont_dp, ont_dr)
              next = prev + (ax -> ont_ax)
            } yield next
          case ax: api.EntityScalarDataPropertyParticularRestrictionAxiom =>
            for {
              prev <- acc2
              ont_e <- prev.get(ax.restrictedEntity) match {
                case Some(e: types.terms.Entity) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"EntityScalarDataPropertyParticularRestrictionAxiom restrictedEntity: ${ax.restrictedEntity} conversion"
                  )).left
              }
              ont_dp <- prev.get(ax.scalarProperty) match {
                case Some(e: types.terms.EntityScalarDataProperty) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"EntityScalarDataPropertyExistentialRestrictionAxiom scalarProperty: ${ax.scalarProperty} conversion"
                  )).left
              }
              ont_ax <- ops.addEntityScalarDataPropertyParticularRestrictionAxiom(ont_tbox, ont_e, ont_dp, OMLString.LexicalValue(ax.literalValue))
              next = prev + (ax -> ont_ax)
            } yield next
          case ax: api.EntityScalarDataPropertyUniversalRestrictionAxiom =>
            for {
              prev <- acc2
              ont_e <- prev.get(ax.restrictedEntity) match {
                case Some(e: types.terms.Entity) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"EntityScalarDataPropertyUniversalRestrictionAxiom restrictedEntity: ${ax.restrictedEntity} conversion"
                  )).left
              }
              ont_dp <- prev.get(ax.scalarProperty) match {
                case Some(e: types.terms.EntityScalarDataProperty) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"EntityScalarDataPropertyExistentialRestrictionAxiom scalarProperty: ${ax.scalarProperty} conversion"
                  )).left
              }
              ont_dr <- prev.get(ax.scalarRestriction) match {
                case Some(e: types.terms.DataRange) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"EntityScalarDataPropertyUniversalRestrictionAxiom scalarRestriction: ${ax.scalarRestriction} conversion"
                  )).left
              }
              ont_ax <- ops.addEntityScalarDataPropertyUniversalRestrictionAxiom(ont_tbox, ont_e, ont_dp, ont_dr)
              next = prev + (ax -> ont_ax)
            } yield next
          case ax: api.EntityExistentialRestrictionAxiom =>
            for {
              prev <- acc2
              ont_s <- prev.get(ax.restrictedDomain) match {
                case Some(e: types.terms.Entity) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"EntityExistentialRestrictionAxiom restrictedDomain: ${ax.restrictedDomain} conversion"
                  )).left
              }
              ont_r <- prev.get(ax.restrictedRelation) match {
                case Some(e: types.terms.ReifiedRelationship) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"EntityExistentialRestrictionAxiom restrictedRelation: ${ax.restrictedRelation} conversion"
                  )).left
              }
              ont_t <- prev.get(ax.restrictedRange) match {
                case Some(e: types.terms.Entity) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"EntityExistentialRestrictionAxiom restrictedRange: ${ax.restrictedRange} conversion"
                  )).left
              }
              ont_ax <- ops.addEntityExistentialRestrictionAxiom(ont_tbox, ont_s, ont_r, ont_t)
              next = prev + (ax -> ont_ax)
            } yield next
          case ax: api.EntityUniversalRestrictionAxiom =>
            for {
              prev <- acc2
              ont_s <- prev.get(ax.restrictedDomain) match {
                case Some(e: types.terms.Entity) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"EntityUniversalRestrictionAxiom restrictedDomain: ${ax.restrictedDomain} conversion"
                  )).left
              }
              ont_r <- prev.get(ax.restrictedRelation) match {
                case Some(e: types.terms.ReifiedRelationship) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"EntityUniversalRestrictionAxiom restrictedRelation: ${ax.restrictedRelation} conversion"
                  )).left
              }
              ont_t <- prev.get(ax.restrictedRange) match {
                case Some(e: types.terms.Entity) =>
                  e.right
                case _ =>
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"EntityUniversalRestrictionAxiom restrictedRange: ${ax.restrictedRange} conversion"
                  )).left
              }
              ont_ax <- ops.addEntityUniversalRestrictionAxiom(ont_tbox, ont_s, ont_r, ont_t)
              next = prev + (ax -> ont_ax)
            } yield next
          case ax =>
            System.out.println(s"... $ax")
            acc2
        }
      }
    } match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    tbox2ont.foldLeft(().right[OMFError.Throwables]) { case (acc1, (_, (e, md))) =>
      implicit val ext: api.Extent = e
      e.annotationProperties.foldLeft(acc1) { case (acc2, (_, ap)) =>
        for {
          _ <- acc2
          tap = TAnnotationProperty(ap.uuid.toString, ap.iri, ap.abbrevIRI)
          _ <- md.addAnnotationProperty(tap)
        } yield ()
      }
    } match {
      case \/-(_) =>
        ()
      case -\/(errors) =>
        return Failure(errors.head)
    }

    tbox2ont.foldLeft(().right[OMFError.Throwables]) { case (acc1, (_, (e, md))) =>
      implicit val ext: api.Extent = e
      e.annotations.foldLeft(acc1) {
        case (acc2, (mtbox: api.TerminologyBox, as)) =>
          for {
            _ <- acc2
            ont_mtbox <- tbox2ont.get(mtbox) match {
              case Some((_, om)) =>
                if (om == md)
                  om.right[OMFError.Throwables]
                else
                  Set[java.lang.Throwable](
                    OMFError.omfError(
                      s"tbox2ont.addAnnotation: the tbox, $mtbox, is not the expected: $om instead of $md")
                  ).left
              case None =>
                Set[java.lang.Throwable](
                  OMFError.omfError(
                    s"tbox2ont.addAnnotation: the tbox, $mtbox, is not mapped to the expected: $md")
                ).left
            }
            _ <- addModuleAnnotations(allTboxElementsWithAxioms, ont_mtbox, as)
          } yield ()
        case (_, (m, _)) =>
          Set[java.lang.Throwable](
            OMFError.omfError(
              s"tbox2ont.addAnnotation: the module should have been a TBOX: $m")
          ).left
      }
    } match {
      case \/-(_) =>
        ()
      case -\/(errors) =>
        return Failure(errors.head)
    }

    // Atomic instances

    val conceptInstances
    : Map[api.ConceptInstance, OWLAPIConceptInstance]
    = dbox2ont.foldLeft {
      Map.empty[api.ConceptInstance, OWLAPIConceptInstance].right[OMFError.Throwables]
    } { case (acc1, (dbox, (e, md))) =>
      implicit val ext: api.Extent = e
      e.conceptInstances.getOrElse(dbox, Set.empty).foldLeft(acc1) { case (acc2, ci0) =>
        for {
          prev <- acc2
          next <- terms3.get(ci0.singletonConceptClassifier) match {
            case Some(scl: OWLAPIConcept) =>
              ops
                .addConceptInstance(md, scl, LocalName(ci0.name))
                .flatMap { ci1 =>
                  if (ci0.uuid == ops.getElementUUID(ci1))
                    (prev + (ci0 -> ci1)).right
                  else
                    Set[java.lang.Throwable](OMFError.omfError(
                      s"OMF Schema table conceptInstance $ci1 conversion from $ci0 " +
                        s"results in UUID mismatch: ${
                          ops
                            .getElementUUID(ci1)
                        }")).left
                }
            case _ =>
              Set[java.lang.Throwable](OMFError.omfError(
                s"OMF Schema table conceptInstance from $ci0 " +
                  s"unresolved singleton concept classifier: " +
                  ci0.singletonConceptClassifier.abbrevIRI()
              )).left
          }
        } yield next
      }
    } match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    val reifiedRelationshipInstances
    : Map[api.ReifiedRelationshipInstance, OWLAPIReifiedRelationshipInstance]
    = dbox2ont.foldLeft {
      Map.empty[api.ReifiedRelationshipInstance, OWLAPIReifiedRelationshipInstance].right[OMFError.Throwables]
    } { case (acc1, (dbox, (e, md))) =>
      implicit val ext: api.Extent = e
      e.reifiedRelationshipInstances.getOrElse(dbox, Set.empty).foldLeft(acc1) { case (acc2, rri0) =>
        for {
          prev <- acc2
          next <- terms3.get(rri0.singletonReifiedRelationshipClassifier) match {
            case Some(scl: OWLAPIReifiedRelationship) =>
              ops
                .addReifiedRelationshipInstance(md, scl, LocalName(rri0.name))
                .flatMap { rri1 =>
                  if (rri0.uuid == ops.getElementUUID(rri1))
                    (prev + (rri0 -> rri1)).right
                  else
                    Set[java.lang.Throwable](OMFError.omfError(
                      s"OMF Schema table reifiedRelationshipInstance $rri1 conversion from $rri0 " +
                        s"results in UUID mismatch: ${ops.getElementUUID(rri1)}"
                    )).left
                }
            case _ =>
              Set[java.lang.Throwable](OMFError.omfError(
                s"OMF Schema table reifiedRelationshipInstance from $rri0 " +
                  s"unresolved singleton reified relationship classifier: " +
                  rri0.singletonReifiedRelationshipClassifier.abbrevIRI()
              )).left
          }
        } yield next
      }
    } match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    val conceptualInstances
    : Map[api.ConceptualEntitySingletonInstance, OWLAPIConceptualEntitySingletonInstance]
    = conceptInstances ++ reifiedRelationshipInstances

    val allDboxElements0: Map[api.Element, common.Element]
    = conceptualInstances ++ dbox2ont.map { case (dbox, (_, ont_dbox)) => dbox -> ont_dbox }

    val allDboxElements1
    = dbox2ont.foldLeft(allDboxElements0.right[OMFError.Throwables]) {
      case (acc1, (dbox, (e, md))) =>
        implicit val ext: api.Extent = e
        e.reifiedRelationshipInstanceDomains.getOrElse(dbox, Set.empty).foldLeft(acc1) { case (acc2, rrid) =>
          for {
            prev <- acc2
            ont_rri <- reifiedRelationshipInstances.get(rrid.reifiedRelationshipInstance).fold(
              Set[java.lang.Throwable](
                OMFError.omfError(
                  s"OMF Schema table reifiedRelationshipInstanceDomains " +
                    s"unresolved singleton reified relationship: " +
                    rrid.reifiedRelationshipInstance.abbrevIRI())
              ).left[OWLAPIReifiedRelationshipInstance]
            )(_.right[OMFError.Throwables])
            ont_source = conceptualInstances(rrid.domain)
            rrjd <- ops.addReifiedRelationshipInstanceDomain(md, ont_rri, ont_source)
            next = prev + (rrid -> rrjd)
          } yield next
        }
    } match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    val allDboxElements2
    = dbox2ont.foldLeft(allDboxElements1.right[OMFError.Throwables]) {
      case (acc1, (dbox, (e, md))) =>
        implicit val ext: api.Extent = e
        e.reifiedRelationshipInstanceRanges.getOrElse(dbox, Set.empty).foldLeft(acc1) { case (acc2, rrir) =>
          for {
            prev <- acc2
            ont_rri <- reifiedRelationshipInstances.get(rrir.reifiedRelationshipInstance).fold(
              Set[java.lang.Throwable](
                OMFError.omfError(
                  s"OMF Schema table reifiedRelationshipInstanceRanges " +
                    s"unresolved singleton reified relationship: " +
                    rrir.reifiedRelationshipInstance.abbrevIRI())
              ).left[OWLAPIReifiedRelationshipInstance]
            )(_.right[OMFError.Throwables])
            ont_target = conceptualInstances(rrir.range)
            rrjr <- ops.addReifiedRelationshipInstanceRange(md, ont_rri, ont_target)
            next = prev + (rrir -> rrjr)
          } yield next
        }
    } match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    val allDboxElements3
    = dbox2ont.foldLeft(allDboxElements2.right[OMFError.Throwables]) {
      case (acc1, (dbox, (e, md))) =>
        implicit val ext: api.Extent = e
        e.unreifiedRelationshipInstanceTuples.getOrElse(dbox, Set.empty).foldLeft(acc1) { case (acc2, uri0) =>
          for {
            prev <- acc2
            ont_uri <- terms3.get(uri0.unreifiedRelationship) match {
              case Some(ur: OWLAPIUnreifiedRelationship) =>
                ur.right
              case _ =>
                Set[java.lang.Throwable](OMFError.omfError(
                  s"OMF Schema table unreifiedRelationshipInstanceTuple from $uri0 " +
                    s"unresolved unreified relationship: " +
                    uri0.unreifiedRelationship.abbrevIRI()
                )).left
            }
            ont_source = conceptualInstances(uri0.domain)
            ont_target = conceptualInstances(uri0.range)
            uri1 <- ops.addUnreifiedRelationshipInstanceTuple(md, ont_uri, ont_source, ont_target)
            next = prev + (uri0 -> uri1)
          } yield next
        }
    } match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    val allDboxElements4
    = dbox2ont.foldLeft(allDboxElements3.right[OMFError.Throwables]) {
      case (acc1, (dbox, (e, md))) =>
        implicit val ext: api.Extent = e
        e.singletonScalarDataPropertyValues.getOrElse(dbox, Set.empty).foldLeft(acc1) { case (acc2, scv0) =>
          for {
            prev <- acc2
            scv1 <- convertEntitySingletonInstanceScalarDataPropertyValue(e2sc, conceptualInstances, e, md, scv0)
            next = prev + (scv0 -> scv1)
          } yield next
        }
    } match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    val allDboxElements5
    = dbox2ont.foldLeft(allDboxElements4.right[OMFError.Throwables]) { case (acc1, (dbox, (e, md))) =>
      implicit val ext: api.Extent = e
      e.singletonStructuredDataPropertyValues.getOrElse(dbox, Set.empty).foldLeft(acc1) { case (acc2, stv) =>
        for {
          prev <- acc2
          next <- convertEntitySingletonInstanceStructuredDataPropertyValue(prev, e2st, s2sc, s2st, conceptualInstances, e, md, stv)
        } yield next
      }
    } match {
      case \/-(map) =>
        map
      case -\/(errors) =>
        return Failure(errors.head)
    }

    dbox2ont.foldLeft(().right[OMFError.Throwables]) { case (acc1, (_, (e, md))) =>
      implicit val ext: api.Extent = e
      e.annotationProperties.foldLeft(acc1) { case (acc2, (_, ap)) =>
        for {
          _ <- acc2
          tap = TAnnotationProperty(ap.uuid.toString, ap.iri, ap.abbrevIRI)
          _ <- md.addAnnotationProperty(tap)
        } yield ()
      }
    } match {
      case \/-(_) =>
        ()
      case -\/(errors) =>
        return Failure(errors.head)
    }

    dbox2ont.foldLeft(().right[OMFError.Throwables]) { case (acc1, (_, (e, md))) =>
      implicit val ext: api.Extent = e
      e.annotations.foldLeft(acc1) {
        case (acc2, (mdbox: api.DescriptionBox, as)) =>
          for {
            _ <- acc2
            ont_mdbox <- dbox2ont.get(mdbox) match {
              case Some((_, om)) =>
                if (om == md)
                  om.right[OMFError.Throwables]
                else
                  Set[java.lang.Throwable](
                    OMFError.omfError(s"dbox2ont.addAnnotation: the dbox, $mdbox, is not the expected: $om instead of $md")
                  ).left
              case None =>
                Set[java.lang.Throwable](
                  OMFError.omfError(s"dbox2ont.addAnnotation: the dbox, $mdbox, is not mapped to the expected: $md")
                ).left
            }
            _ <- addModuleAnnotations(allDboxElements5, ont_mdbox, as)
          } yield ()
        case (_, (m, _)) =>
          Set[java.lang.Throwable](
            OMFError.omfError(s"dbox2ont.addAnnotation: the module should have been a DBOX: $m")
          ).left
      }
    } match {
      case \/-(_) =>
        ()
      case -\/(errors) =>
        return Failure(errors.head)
    }

    val (tboxConvs, m2iTboxConv) =
      moduleSort.foldLeft[OMFError.Throwables \/ (Seq[(api.Module, OWLAPIImmutableTerminologyBox)], Mutable2ImmutableModuleMap)](
        (Seq.empty[(api.Module, OWLAPIImmutableTerminologyBox)], emptyMutable2ImmutableModuleMap).right
      ) {
        case (acc, m: api.TerminologyBox) =>
          for {
            prev <- acc
            (convs, m2i) = prev
            next <- tbox2ont.get(m) match {
              case Some((_, mg)) =>
                System.out.println(s"... Converting terminology ${mg.sig.kind}: ${mg.iri}")
                ops.asImmutableTerminologyBox(mg, m2i).map { case (conv, m2iWithConv) =>
                  Tuple2(convs :+ (m -> conv), m2iWithConv)
                }
              case _ =>
                acc
            }
          } yield next
        case (acc, _) =>
          acc
      } match {
        case \/-(map) =>
          map
        case -\/(errors) =>
          return Failure(errors.head)
      }

    tboxConvs.foreach { case (_, itbox) =>
      System.out.println(s"... Saving terminology ${itbox.iri}")
      ops.saveTerminology(itbox) match {
        case \/-(_) =>
          ()
        case -\/(errors) =>
          return Failure(errors.head)
      }
    }

    val (dboxConvs, _) =
      moduleSort.foldLeft[OMFError.Throwables \/ (Seq[(api.Module, OWLAPIImmutableDescriptionBox)], Mutable2ImmutableModuleMap)](
        (Seq.empty[(api.Module, OWLAPIImmutableDescriptionBox)], m2iTboxConv).right
      ) {
        case (acc, m: api.DescriptionBox) =>
          for {
            prev <- acc
            (convs, m2i) = prev
            next <- dbox2ont.get(m) match {
              case Some((_, mg)) =>
                System.out.println(s"... Converting description ${mg.sig.kind}: ${mg.iri}")
                ops.asImmutableDescription(mg, m2i).map { case (conv, m2iWithConv) =>
                  Tuple2(convs :+ (m -> conv), m2iWithConv)
                }
              case _ =>
                acc
            }
          } yield next
        case (acc, _) =>
          acc
      } match {
        case \/-(map) =>
          map
        case -\/(errors) =>
          return Failure(errors.head)
      }

    dboxConvs.foreach { case (_, idbox) =>
      System.out.println(s"... Saving description ${idbox.iri}")
      ops.saveDescriptionBox(idbox) match {
        case \/-(_) =>
          ()
        case -\/(errors) =>
          return Failure(errors.head)
      }
    }
    Success(())
  }

  final protected def addModuleAnnotations
  (allTboxElements: Map[api.Element, common.Element],
   md: OWLAPIMutableTerminologyBox,
   as: Set[api.Annotation])
  (implicit ext: api.Extent, store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ Unit
  = as.foldLeft(().right[OMFError.Throwables]) { case (acc, a) =>
    for {
      _ <- acc
      ap = a.property
      tap = TAnnotationProperty(ap.uuid.toString, ap.iri, ap.abbrevIRI)
      ont_subject <- allTboxElements.get(a.subject) match {
        case Some(e) =>
          e.right[OMFError.Throwables]
        case None =>
          Set[java.lang.Throwable](
            OMFError.omfError(s"addModuleAnnotations(tbox: ${md.iri}) missing element mapping for ${a.subject}")
          ).left
      }
      _ <- md.addAnnotation(ont_subject, tap, a.value)
    } yield ()
  }

  final protected def addModuleAnnotations
  (allDboxElements: Map[api.Element, common.Element],
   md: OWLAPIMutableDescriptionBox,
   as: Set[api.Annotation])
  (implicit ext: api.Extent, store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ Unit
  = as.foldLeft(().right[OMFError.Throwables]) { case (acc, a) =>
    for {
      _ <- acc
      ap = a.property
      tap = TAnnotationProperty(ap.uuid.toString, ap.iri, ap.abbrevIRI)
      aSubject = a.subject
      ont_subject <- allDboxElements.get(aSubject) match {
        case Some(e) =>
          e.right[OMFError.Throwables]
        case None =>
          Set[java.lang.Throwable](
            OMFError.omfError(s"addModuleAnnotations(dbox: ${md.iri}) missing element mapping for ${a.subject}")
          ).left
      }
      _ <- ont_subject match {
        case _: common.Resource =>
          md.addAnnotation(ont_subject, tap, a.value)
        case _ =>
          // Not yet supported.
          ().right
      }
    } yield ()
  }

  final protected def convertEntitySingletonInstanceScalarDataPropertyValue
  (e2sc: Map[api.EntityScalarDataProperty, OWLAPIEntityScalarDataProperty],
   conceptualInstances: Map[api.ConceptualEntitySingletonInstance, OWLAPIConceptualEntitySingletonInstance],
   e: api.Extent,
   md: OWLAPIMutableDescriptionBox,
   scv: api.SingletonInstanceScalarDataPropertyValue)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPISingletonInstanceScalarDataPropertyValue
  = {
    implicit val apiExtent: api.Extent = e
    for {
      ont_scp <- e2sc.get(scv.scalarDataProperty) match {
        case Some(sc: OWLAPIEntityScalarDataProperty) =>
          sc.right
        case _ =>
          Set[java.lang.Throwable](OMFError.omfError(
            s"OMF Schema table convertEntitySingletonInstanceScalarDataPropertyValue from $scv " +
              s"unresolved scalarDataProperty: " +
              scv.scalarDataProperty.abbrevIRI()
          )).left
      }
      ont_s = conceptualInstances(scv.singletonInstance)
      ont_scv <- ops.addSingletonInstanceScalarDataPropertyValue(md, ont_s, ont_scp, LexicalValue(scv.scalarPropertyValue))
    } yield ont_scv
  }

  final protected def convertEntitySingletonInstanceStructuredDataPropertyValue
  (prev: Map[api.Element, common.Element],
   e2st: Map[api.EntityStructuredDataProperty, OWLAPIEntityStructuredDataProperty],
   s2sc: Map[api.ScalarDataProperty, OWLAPIScalarDataProperty],
   s2st: Map[api.StructuredDataProperty, OWLAPIStructuredDataProperty],
   conceptualInstances: Map[api.ConceptualEntitySingletonInstance, OWLAPIConceptualEntitySingletonInstance],
   e: api.Extent,
   md: OWLAPIMutableDescriptionBox,
   stv: api.SingletonInstanceStructuredDataPropertyValue)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ Map[api.Element, common.Element]
  = {
    implicit val apiExtent: api.Extent = e
    for {
      ont_stp <- e2st.get(stv.structuredDataProperty.asInstanceOf[api.EntityStructuredDataProperty]) match {
        case Some(st: OWLAPIEntityStructuredDataProperty) =>
          st.right
        case _ =>
          Set[java.lang.Throwable](OMFError.omfError(
            s"OMF Schema table convertEntitySingletonInstanceStructuredDataPropertyValue from $stv " +
              s"unresolved scalarDataProperty: " +
              stv.structuredDataProperty.abbrevIRI()
          )).left
      }
      ont_s = conceptualInstances(stv.singletonInstance)
      ont_stv <- ops.addSingletonInstanceStructuredDataPropertyValue(md, ont_s, ont_stp)
      next1 = prev + (stv -> ont_stv)
      next2 <- convertSingletonInstanceScalarDataPropertyValues(next1, s2sc, e, md, stv, ont_stv)
      next3 <- convertSingletonInstanceStructuredDataPropertyValues(next2, s2st, s2sc, e, md, stv, ont_stv)
    } yield next3
  }

  final protected def convertSingletonInstanceStructuredDataPropertyValues
  (e2e: Map[api.Element, common.Element],
   s2st: Map[api.StructuredDataProperty, OWLAPIStructuredDataProperty],
   s2sc: Map[api.ScalarDataProperty, OWLAPIScalarDataProperty],
   e: api.Extent,
   md: OWLAPIMutableDescriptionBox,
   context: api.SingletonInstanceStructuredDataPropertyContext,
   ont_context: OWLAPISingletonInstanceStructuredDataPropertyContext)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ Map[api.Element, common.Element]
  = e.structuredPropertyTuples.getOrElse(context, Set.empty).foldLeft(e2e.right[OMFError.Throwables]) {
    case (acc, stv) =>
    implicit val apiExtent: api.Extent = e
    for {
      prev <- acc
      ont_st2st <- s2st.get(stv.structuredDataProperty.asInstanceOf[api.StructuredDataProperty]) match {
        case Some(st: OWLAPIStructuredDataProperty) =>
          st.right
        case _ =>
          Set[java.lang.Throwable](OMFError.omfError(
            s"OMF Schema table convertSingletonInstanceStructuredDataPropertyValues from $stv " +
              s"unresolved structuredDataProperty: " +
              stv.structuredDataProperty.abbrevIRI()
          )).left
      }
      ont_stv <- ops.makeStructuredDataPropertyTuple(md, ont_context, ont_st2st)
      next1 = prev + (stv -> ont_stv)
      next2 <- convertSingletonInstanceScalarDataPropertyValues(next1, s2sc, e, md, stv, ont_stv)
      next3 <- convertSingletonInstanceStructuredDataPropertyValues(next2, s2st, s2sc, e, md, stv, ont_stv)
    } yield next3
  }

  final protected def convertSingletonInstanceScalarDataPropertyValues
  (e2e: Map[api.Element, common.Element],
   s2sc: Map[api.ScalarDataProperty, OWLAPIScalarDataProperty],
   e: api.Extent,
   md: OWLAPIMutableDescriptionBox,
   context: api.SingletonInstanceStructuredDataPropertyContext,
   ont_context: OWLAPISingletonInstanceStructuredDataPropertyContext)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ Map[api.Element, common.Element]
  = e.scalarDataPropertyValues.getOrElse(context, Set.empty).foldLeft(e2e.right[OMFError.Throwables]) {
    case (acc, sdv) =>
    implicit val apiExtent: api.Extent = e
    for {
      prev <- acc
      ont_st2sc <- s2sc.get(sdv.scalarDataProperty.asInstanceOf[api.ScalarDataProperty]) match {
        case Some(st2sc: OWLAPIScalarDataProperty) =>
          st2sc.right
        case _ =>
          Set[java.lang.Throwable](OMFError.omfError(
            s"OMF Schema table convertSingletonInstanceScalarDataPropertyValues from $sdv " +
              s"unresolved scalarDataProperty: " +
              sdv.scalarDataProperty.abbrevIRI()
          )).left
      }
      ont_sdv <- ops.makeScalarDataPropertyValue(md, ont_context, ont_st2sc, LexicalValue(sdv.scalarPropertyValue))
    } yield prev + (sdv -> ont_sdv)
  }

  final protected def convertDataRanges
  (terms: Map[api.Term, OWLAPITerm],
   drs: Seq[(api.DataRange, api.RestrictedDataRange, api.TerminologyBox, api.Extent, OWLAPIMutableTerminologyBox)],
   queue: Seq[(api.DataRange, api.RestrictedDataRange, api.TerminologyBox, api.Extent, OWLAPIMutableTerminologyBox)] = Seq.empty,
   progress: Int = 0)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ Map[api.Term, OWLAPITerm]
  = if (drs.isEmpty) {
    if (queue.isEmpty)
      terms.right
    else if (0 == progress)
      Set[java.lang.Throwable](
        OMFError.omfError("No-progress in convertDataRanges!")).left
    else
      convertDataRanges(terms, queue)
  } else {
    val (dr, rdr0, _, e, mg) = drs.head
    val ordr1
    : OMFError.Throwables \/ Option[OWLAPIDataRange]
    = rdr0.iri()(e) match {
      case Some(ti) =>
        val oi = IRI.create(ti)
        mg.lookupTerm(oi, recursively=false) match {
          case Some(rdr1: OWLAPIDataRange) =>
            Some(rdr1).right[OMFError.Throwables]
          case Some(rdr1) =>
            Set[java.lang.Throwable](
              OMFError.omfError(
                s"OMF Schema table scalar $rdr0 lookup should have produced an OWLAPIDataRange, got: $rdr1"
              )).left
          case None =>
            None.right[OMFError.Throwables]
        }
      case None =>
        None.right[OMFError.Throwables]
    }

    ordr1 match {
      case \/-(None) =>
        terms.get(dr) match {
          case Some(r: OWLAPIDataRange) =>
            val conv = rdr0 match {
              case sr: api.BinaryScalarRestriction =>
                ops
                  .addBinaryScalarRestriction(mg,
                    LocalName(sr.name),
                    r,
                    sr.length,
                    sr.minLength,
                    sr.maxLength)
                  .flatMap { osr =>
                    if (sr.uuid == ops.getTermUUID(osr)) osr.right
                    else
                      Set[java.lang.Throwable](OMFError.omfError(
                        s"OMF Schema table BinaryScalarRestriction $sr conversion " +
                          s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                  }
              case sr: api.IRIScalarRestriction =>
                ops
                  .addIRIScalarRestriction(mg,
                    LocalName(sr.name),
                    r,
                    sr.length,
                    sr.minLength,
                    sr.maxLength,
                    sr.pattern.map(Pattern(_)))
                  .flatMap { osr =>
                    if (sr.uuid == ops.getTermUUID(osr)) osr.right
                    else
                      Set[java.lang.Throwable](OMFError.omfError(
                        s"OMF Schema table IRIScalarRestriction $sr conversion " +
                          s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                  }
              case sr: api.NumericScalarRestriction =>
                ops
                  .addNumericScalarRestriction(
                    mg,
                    LocalName(sr.name),
                    r,
                    sr.minInclusive.map(LexicalValue(_)),
                    sr.maxInclusive.map(LexicalValue(_)),
                    sr.minExclusive.map(LexicalValue(_)),
                    sr.maxExclusive.map(LexicalValue(_))
                  )
                  .flatMap { osr =>
                    if (sr.uuid == ops.getTermUUID(osr)) osr.right
                    else
                      Set[java.lang.Throwable](OMFError.omfError(
                        s"OMF Schema table NumericScalarRestriction $sr conversion " +
                          s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                  }
              case sr: api.PlainLiteralScalarRestriction =>
                ops
                  .addPlainLiteralScalarRestriction(
                    mg,
                    LocalName(sr.name),
                    r,
                    sr.length,
                    sr.minLength,
                    sr.maxLength,
                    sr.pattern.map(Pattern(_)))
                  .flatMap { osr =>
                    if (sr.uuid == ops.getTermUUID(osr)) osr.right
                    else
                      Set[java.lang.Throwable](OMFError.omfError(
                        s"OMF Schema table PlainLiteralScalarRestriction $sr conversion " +
                          s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                  }
              case sr: api.ScalarOneOfRestriction =>
                ops
                  .addScalarOneOfRestriction(mg, LocalName(sr.name), r)
                  .flatMap { osr =>
                    if (sr.uuid == ops.getTermUUID(osr)) osr.right
                    else
                      Set[java.lang.Throwable](OMFError.omfError(
                        s"OMF Schema table ScalarOneOfRestriction $sr conversion " +
                          s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                  }
              case sr: api.StringScalarRestriction =>
                ops
                  .addStringScalarRestriction(mg,
                    LocalName(sr.name),
                    r,
                    sr.length,
                    sr.minLength,
                    sr.maxLength,
                    sr.pattern.map(Pattern(_)))
                  .flatMap { osr =>
                    if (sr.uuid == ops.getTermUUID(osr)) osr.right
                    else
                      Set[java.lang.Throwable](OMFError.omfError(
                        s"OMF Schema table StringScalarRestriction $sr conversion " +
                          s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                  }
              case sr: api.SynonymScalarRestriction =>
                ops
                  .addSynonymScalarRestriction(mg,
                    LocalName(sr.name),
                    r)
                  .flatMap { osr =>
                    if (sr.uuid == ops.getTermUUID(osr)) osr.right
                    else
                      Set[java.lang.Throwable](OMFError.omfError(
                        s"OMF Schema table SynonymScalarRestriction $sr conversion " +
                          s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                  }
              case sr: api.TimeScalarRestriction =>
                ops
                  .addTimeScalarRestriction(
                    mg,
                    LocalName(sr.name),
                    r,
                    sr.minInclusive.map(LexicalValue(_)),
                    sr.maxInclusive.map(LexicalValue(_)),
                    sr.minExclusive.map(LexicalValue(_)),
                    sr.maxExclusive.map(LexicalValue(_))
                  )
                  .flatMap { osr =>
                    if (sr.uuid == ops.getTermUUID(osr)) osr.right
                    else
                      Set[java.lang.Throwable](OMFError.omfError(
                        s"OMF Schema table TimeScalarRestriction $sr conversion " +
                          s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                  }
            }
            conv match {
              case -\/(errors) =>
                Set[java.lang.Throwable](
                  OMFError.omfException("Errors in convertDataRanges",
                    errors)).left
              case \/-(rdr1) =>
                convertDataRanges(
                  terms + (rdr0 -> rdr1),
                  drs.tail,
                  queue, 1 + progress)
            }
          case _ =>
            convertDataRanges(terms, drs.tail, queue :+ drs.head, progress)
        }
      case \/-(Some(rdr1)) =>
        if (rdr0.uuid == ops.getTermUUID(rdr1))
          convertDataRanges(
            terms + (rdr0 -> rdr1),
            drs.tail,
            queue, 1 + progress)
        else
          Set[java.lang.Throwable](
            OMFError.omfError(
              s"OMF Schema table scalar $rdr0 conversion " +
                s"results in UUID mismatch: ${ops.getTermUUID(rdr1)}")).left
      case -\/(errors) =>
        -\/(errors)
    }
  }

  final protected def convertReifiedRelationships
  (terms: Map[api.Term, OWLAPITerm],
   rrs: Seq[(api.Term, api.Term, api.ReifiedRelationship, api.TerminologyBox, api.Extent, OWLAPIMutableTerminologyBox)],
   queue: Seq[(api.Term, api.Term, api.ReifiedRelationship, api.TerminologyBox, api.Extent, OWLAPIMutableTerminologyBox)] = Seq.empty,
   progress: Int = 0)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ Map[api.Term, OWLAPITerm]
  = if (rrs.isEmpty) {
    if (queue.isEmpty)
      terms.right
    else if (0 == progress)
      Set[java.lang.Throwable](OMFError.omfError(
          "No progress in convertReifiedRelationships!")).left
    else
      convertReifiedRelationships(terms, queue)
    } else {
    val (rs, rt, rr0, _, _, mg) = rrs.head
    (terms.get(rs), terms.get(rt)) match {
      case (Some(s: OWLAPIEntity), Some(t: OWLAPIEntity)) =>
        ops.addReifiedRelationship(
          mg,
          s,
          t,
          Iterable() ++
            (if (rr0.isAsymmetric)
              Iterable(RelationshipCharacteristics.isAsymmetric)
            else Iterable()) ++
            (if (rr0.isEssential)
              Iterable(RelationshipCharacteristics.isEssential)
            else Iterable()) ++
            (if (rr0.isFunctional)
              Iterable(RelationshipCharacteristics.isFunctional)
            else Iterable()) ++
            (if (rr0.isInverseEssential)
              Iterable(RelationshipCharacteristics.isInverseEssential)
            else Iterable()) ++
            (if (rr0.isInverseFunctional)
              Iterable(RelationshipCharacteristics.isInverseFunctional)
            else Iterable()) ++
            (if (rr0.isIrreflexive)
              Iterable(RelationshipCharacteristics.isIrreflexive)
            else Iterable()) ++
            (if (rr0.isReflexive)
              Iterable(RelationshipCharacteristics.isReflexive)
            else Iterable()) ++
            (if (rr0.isSymmetric)
              Iterable(RelationshipCharacteristics.isSymmetric)
            else Iterable()) ++
            (if (rr0.isTransitive)
              Iterable(RelationshipCharacteristics.isTransitive)
            else Iterable()),
          LocalName(rr0.name),
          LocalName(rr0.unreifiedPropertyName),
          rr0.unreifiedInversePropertyName.map(LocalName(_))
        )
          .flatMap { rr1 =>
            if (rr0.uuid == ops.getTermUUID(rr1))
              convertReifiedRelationships(terms + (rr0 -> rr1), rrs.tail, queue, 1 + progress)
            else
              Set[java.lang.Throwable](OMFError.omfError(
                s"OMF Schema table ReifiedRelationship $rr0 conversion " +
                  s"results in UUID mismatch: ${ops.getTermUUID(rr1)}")).left
          }
      case _ =>
        convertReifiedRelationships(terms, rrs.tail, queue :+ rrs.head, progress)
    }
  }

  final protected def convertUnreifiedRelationships
  (terms: Map[api.Term, OWLAPITerm],
   tbox2ont: Map[api.TerminologyBox, (api.Extent, OWLAPIMutableTerminologyBox)])
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ Map[api.Term, OWLAPITerm]
  = tbox2ont.foldLeft(terms.right[OMFError.Throwables]) { case (acc1, (tbox, (e, mg))) =>
    implicit val ext: api.Extent = e
    import OMLOps._
    tbox.unreifiedRelationships.foldLeft(acc1) { case (acc2, ur0) =>
      acc2.flatMap { prev =>
        (terms.get(ur0.source), terms.get(ur0.target)) match {
          case (Some(s: OWLAPIConceptualEntity), Some(t: OWLAPIConceptualEntity)) =>
            ops
              .addUnreifiedRelationship(
                mg,
                s,
                t,
                Iterable() ++
                  (if (ur0.isAsymmetric)
                    Iterable(RelationshipCharacteristics.isAsymmetric)
                  else Iterable()) ++
                  (if (ur0.isEssential)
                    Iterable(RelationshipCharacteristics.isEssential)
                  else Iterable()) ++
                  (if (ur0.isFunctional)
                    Iterable(RelationshipCharacteristics.isFunctional)
                  else Iterable()) ++
                  (if (ur0.isInverseEssential)
                    Iterable(RelationshipCharacteristics.isInverseEssential)
                  else Iterable()) ++
                  (if (ur0.isInverseFunctional)
                    Iterable(
                      RelationshipCharacteristics.isInverseFunctional)
                  else Iterable()) ++
                  (if (ur0.isIrreflexive)
                    Iterable(RelationshipCharacteristics.isIrreflexive)
                  else Iterable()) ++
                  (if (ur0.isReflexive)
                    Iterable(RelationshipCharacteristics.isReflexive)
                  else Iterable()) ++
                  (if (ur0.isSymmetric)
                    Iterable(RelationshipCharacteristics.isSymmetric)
                  else Iterable()) ++
                  (if (ur0.isTransitive)
                    Iterable(RelationshipCharacteristics.isTransitive)
                  else Iterable()),
                LocalName(ur0.name)
              )
              .flatMap { ur1 =>
                if (ur0.uuid == ops.getTermUUID(ur1))
                  (prev + (ur0 -> ur1)).right
                else
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"OMF Schema table UnreifiedRelationship $ur0 conversion " +
                      s"results in UUID mismatch: ${ops.getTermUUID(ur1)}")).left
              }
          case (_, _) =>
            Set[java.lang.Throwable](OMFError.omfError(
              s"Unresolved unreifiedRelationship: $ur0")).left
        }
      }
    }
  }

}
