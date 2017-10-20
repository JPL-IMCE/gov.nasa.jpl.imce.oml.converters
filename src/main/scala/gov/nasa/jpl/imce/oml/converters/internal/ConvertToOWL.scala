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

package gov.nasa.jpl.imce.oml.converters.internal

import java.lang.System

import ammonite.ops.{up,Path}

import gov.nasa.jpl.imce.oml.converters.{metadata, utils}
import gov.nasa.jpl.imce.oml.model.extensions.OMLCatalog
import gov.nasa.jpl.imce.oml.resolver._
import gov.nasa.jpl.imce.oml.tables.{ClosedWorldDesignations, Final, OpenWorldDefinitions, Partial, AnnotationProperty => TAnnotationProperty}
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{ImmutableTerminologyBox => OWLAPIImmutableTerminologyBox}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{MutableBundle => OWLAPIMutableBundle}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{MutableTerminologyBox => OWLAPIMutableTerminologyBox}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{MutableTerminologyGraph => OWLAPIMutableTerminologyGraph}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.{Term => OWLAPITerm}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms.{Concept => OWLAPIConcept, DataRange => OWLAPIDataRange, Entity => OWLAPIEntity, EntityScalarDataProperty => OWLAPIEntityScalarDataProperty, EntityStructuredDataProperty => OWLAPIEntityStructuredDataProperty, ReifiedRelationship => OWLAPIReifiedRelationship, ScalarDataProperty => OWLAPIScalarDataProperty, Structure => OWLAPIStructure, StructuredDataProperty => OWLAPIStructuredDataProperty, UnreifiedRelationship => OWLAPIUnreifiedRelationship}
import gov.nasa.jpl.omf.scala.binding.owlapi.descriptions.{ConceptInstance => OWLAPIConceptInstance, ConceptualEntitySingletonInstance => OWLAPIConceptualEntitySingletonInstance, ImmutableDescriptionBox => OWLAPIImmutableDescriptionBox, MutableDescriptionBox => OWLAPIMutableDescriptionBox, ReifiedRelationshipInstance => OWLAPIReifiedRelationshipInstance, SingletonInstanceScalarDataPropertyValue => OWLAPISingletonInstanceScalarDataPropertyValue, SingletonInstanceStructuredDataPropertyContext => OWLAPISingletonInstanceStructuredDataPropertyContext}
import gov.nasa.jpl.omf.scala.core.{OMFError, RelationshipCharacteristics, TerminologyKind}
import gov.nasa.jpl.omf.scala.core.OMLString.{LocalName, Pattern}
import org.semanticweb.owlapi.model.IRI

import scala.collection.immutable.{Iterable, Map, Seq, Set}
import scala.{Boolean, Int, None, Option, Some, StringContext, Tuple2, Tuple5, Tuple6, Unit}
import scala.Predef.{augmentString, ArrowAssoc}
import scalax.collection.GraphEdge.NodeProduct
import scalax.collection.immutable.Graph
import scalaz._
import Scalaz._


object ConvertToOWL {

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

  def convertToOWL
  (modules: Map[api.Module, api.Extent],
   moduleEdges: Map[api.ModuleEdge, api.Extent],
   cat: OMLCatalog,
   outCatalog: Path)
  (implicit omfStore: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ Seq[api.Module]
  = {
    implicit val ops = omfStore.ops
    import OMLOps._

    val g0
    : Graph[api.Module, ModuleGraphEdge]
    = Graph[api.Module, ModuleGraphEdge]()

    val g1
    : Graph[api.Module, ModuleGraphEdge]
    = modules.foldLeft(g0) {
      case (gi, (mi, _)) =>
        System.out.println(s"Node: ${mi.iri}")
        gi + mi
    }

    val g2
    : Graph[api.Module, ModuleGraphEdge]
    = moduleEdges.foldLeft(g1) { case (gi, (me, ext)) =>
      val source = me.sourceModule()(ext).get // TODO
      val targetIRI = me.targetModule()(ext)
      gi.toOuterNodes.find(_.iri == targetIRI).fold(gi) { target: api.Module =>
        val edge = new ModuleGraphEdge[api.Module](NodeProduct(source, target), me)
        System.out.println(s"Edge: ${source.iri} ~> ${target.iri}")
        gi + edge
      }
    }

    val g = g2

    val catalogDir = outCatalog / up
    val metadataFile = (catalogDir / "oml.metadata.json").toIO

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
          .stripPrefix(catalogDir.ext)
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
        val (s, t) = (e.from.value, e.to.value)
        metadata.OMLConvertedModuleEdge(
          importing = metadata.OMLMetadataString.ImportingModuleIRI(s.iri),
          imported = metadata.OMLMetadataString.ImportedModuleIRI(t.iri))
      }
      .to[Seq]
      .sorted

    val metadataGraph = metadata.OMLMetadataGraph(nodes = metadataNodes, edges = metadataEdges)

    {
      val s = new java.io.PrintWriter(metadataFile)
      try {
        val json = metadata.OMLMetadataGraph.encoder(metadataGraph)
        s.write(json.toString)
        System.out.println(s"# Wrote OML Metadata Graph: $metadataFile")
      } finally {
        s.close()
      }
    }

    import api.moduleOrdering

    for {
      moduleSort // : Seq[api.Module]
      <- OWLAPIOMFLoader
        .hierarchicalTopologicalSort[api.Module, ModuleGraphEdge](Seq(g), Seq.empty)
        .map(_.reverse)

      _ = System.out.println(moduleSort.map(_.iri).mkString("\n# Sorted modules:\n## ","\n## ","\n"))

      drc <- omfStore.loadBuiltinDatatypeMap()

      // map TerminologyBox
      tbox2ont
      <- modules.foldLeft {
        Map.empty[api.TerminologyBox, (api.Extent, OWLAPIMutableTerminologyBox)].right[OMFError.Throwables]
      } {
        case (acc1, (tbox: api.TerminologyBox, ext)) =>
          for {
            prev2 <- acc1
            k = tbox.kind match {
              case OpenWorldDefinitions =>
                TerminologyKind.isOpenWorld
              case ClosedWorldDesignations =>
                TerminologyKind.isClosedWorld
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
                  ops.makeTerminologyGraph(i, k)
                case tbundle: api.Bundle =>
                  ops.makeBundle(i, k)
              }
            } { mbox =>
              mbox.right[OMFError.Throwables]
            }
            next2 = prev2 + (tbox -> (ext -> m))
          } yield next2

        case (acc1, _) =>
          acc1
      }

      // map DescriptionBox
      dbox2ont
      <- modules.foldLeft {
        Map.empty[api.DescriptionBox, (api.Extent, OWLAPIMutableDescriptionBox)].right[OMFError.Throwables]
      } {
        case (acc1, (dbox: api.DescriptionBox, ext)) =>
          for {
            prev2 <- acc1
            k = dbox.kind match {
              case Final =>
                gov.nasa.jpl.omf.scala.core.DescriptionKind.isFinal
              case Partial =>
                gov.nasa.jpl.omf.scala.core.DescriptionKind.isPartial
            }
            i = dbox.iri
            m <- ops.makeDescriptionBox(
              LocalName(dbox.name()),
              IRI.create(i),
              k)
            next2 = prev2 + (dbox -> (ext -> m))
          } yield next2
        case (acc1, _) =>
          acc1
      }

      // Atomic terms
      terms0 // : Map[api.Term, OWLAPITerm]
      <- tbox2ont.foldLeft {
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
                          mg.lookupTerm(oi, recursively = false) match {
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
      }

      dataRangeRestrictions
      // : Seq[(api.DataRange, api.RestrictedDataRange, api.TerminologyBox, api.Extent, OWLAPIMutableTerminologyBox)]
      = tbox2ont.to[Seq].flatMap { case (tbox, (e, m)) =>
        import utils.EMFFilterable._
        implicit val ext: api.Extent = e
        val rdrs = tbox.dataranges.selectByKindOf { case rdr: api.RestrictedDataRange => rdr }
        rdrs.map { rdr =>
          Tuple5(rdr.restrictedRange, rdr, tbox, e, m)
        }
      }

      terms1 <- convertDataRanges(terms0, dataRangeRestrictions)

      _ <- g.toOuterEdges.foldLeft[OMFError.Throwables \/ Unit](\/-(())) { case (acc, edge) =>

        for {
          _ <- acc
          _ <- edge match {

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
                  ops.addTerminologyExtension(extendingG = s, extendedG = t).map(_ => ())
                } { _ =>
                  ().right[OMFError.Throwables]
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
                nestingGraph = t)

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
                designationTerminologyGraph = t)

            // ModuleEdge (BundledTerminologyAxiom)
            case ModuleGraphEdge(
            (es: api.Bundle, et: api.TerminologyBox),
            _: api.BundledTerminologyAxiom) =>
              val s = tbox2ont(es)._2.asInstanceOf[OWLAPIMutableBundle]
              val t = tbox2ont(et)._2
              ops.addBundledTerminologyAxiom(terminologyBundle = s, bundledTerminology = t)

            // ModuleEdge (DescriptionBoxExtendsClosedWorldDefinitions)
            case ModuleGraphEdge(
            (es: api.DescriptionBox, et: api.TerminologyBox),
            _: api.DescriptionBoxExtendsClosedWorldDefinitions) =>
              val s = dbox2ont(es)._2
              val t = tbox2ont(et)._2
              ops.addDescriptionBoxExtendsClosedWorldDefinitions(dbox = s, closedWorldDefinitions = t)

            // ModuleEdge (DescriptionBoxRefinement)
            case ModuleGraphEdge(
            (es: api.DescriptionBox, et: api.DescriptionBox),
            _: api.DescriptionBoxRefinement) =>
              val s = dbox2ont(es)._2
              val t = dbox2ont(et)._2
              ops.addDescriptionBoxRefinement(refiningDescriptionBox = s, refinedDescriptionBox = t)
          }
        } yield ()
      }

      reifiedRelationships
      // : Seq[(api.Term, api.Term, api.ReifiedRelationship, api.TerminologyBox, api.Extent, OWLAPIMutableTerminologyBox)]
      = tbox2ont.to[Seq].flatMap { case (tbox, (e, m)) =>
        implicit val ext: api.Extent = e
        tbox.reifiedRelationships.map { rr =>
          Tuple6(rr.relationDomain(), rr.relationRange(), rr, tbox, e, m)
        }
      }

      terms2 <- convertReifiedRelationships(terms1, reifiedRelationships)
      terms3 <- convertUnreifiedRelationships(terms2, tbox2ont)

      // Data Properties

      e2sc
      // : Map[api.EntityScalarDataProperty, OWLAPIEntityScalarDataProperty]
      <- tbox2ont.foldLeft {
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
      }

      e2st
      // : Map[api.EntityStructuredDataProperty, OWLAPIEntityStructuredDataProperty]
      <- tbox2ont.foldLeft {
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
      }

      s2sc
      // : Map[api.ScalarDataProperty, OWLAPIScalarDataProperty]
      <- tbox2ont.foldLeft {
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
      }

      s2st
      // : Map[api.StructuredDataProperty, OWLAPIStructuredDataProperty]
      <- tbox2ont.foldLeft {
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
      }

      allTboxElements
      = Map.empty[api.Element, common.Element] ++
        terms3 ++
        e2sc ++
        e2st ++
        s2sc ++
        s2st ++
        tbox2ont.map { case (tbox, (_, ont_tbox)) => tbox -> ont_tbox }

      allTboxElementsWithAxioms
      <- tbox2ont
        .foldLeft[OMFError.Throwables \/ Map[api.Element, common.Element]] {
        allTboxElements.right[OMFError.Throwables]
      } { case (acc1, (tbox, (e, ont_tbox))) =>
        tbox.boxStatements(e).foldLeft[OMFError.Throwables \/ Map[api.Element, common.Element]](acc1) { case (acc2, s) =>
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
                ont_vt <- ax.valueType match {
                  case Some(vt: types.terms.DataRange) =>
                    Some(vt).right
                  case Some(_) =>
                    Set[java.lang.Throwable](OMFError.omfError(
                      s"ScalarOneOfLiteralAxiom axiom: ${ax.axiom} conversion"
                    )).left
                  case None =>
                    None.right
                }
                ont_ax <- ops.addScalarOneOfLiteralAxiom(ont_tbox, ont_r, ax.value, ont_vt)
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
                ont_vt <- ax.valueType match {
                  case Some(vt: types.terms.DataRange) =>
                    Some(vt).right
                  case Some(_) =>
                    Set[java.lang.Throwable](OMFError.omfError(
                      s"ScalarOneOfLiteralAxiom axiom: ${ax.valueType} conversion"
                    )).left
                  case None =>
                    None.right
                }
                ont_ax <- ops.addEntityScalarDataPropertyParticularRestrictionAxiom(ont_tbox, ont_e, ont_dp, ax.literalValue, ont_vt)
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
      }

      _ <- tbox2ont.foldLeft(().right[OMFError.Throwables]) { case (acc1, (_, (e, md))) =>
        implicit val ext: api.Extent = e
        e.annotationProperties.foldLeft(acc1) { case (acc2, (_, ap)) =>
          for {
            _ <- acc2
            tap = TAnnotationProperty(ap.uuid.toString, ap.iri, ap.abbrevIRI)
            _ <- md.addAnnotationProperty(tap)
          } yield ()
        }
      }

      _ <- tbox2ont.foldLeft(().right[OMFError.Throwables]) { case (acc1, (_, (extent, md))) =>
        implicit val ext: api.Extent = extent
        ext.annotations.foldLeft(acc1) {
          case (acc2, (elt: api.Element, as: Set[api.AnnotationPropertyValue])) =>
            for {
              _ <- acc2
              _ <- addElementAnnotations(allTboxElementsWithAxioms, md, elt, as)
            } yield ()
        }
      }

      // Atomic instances

      conceptInstances
      <- dbox2ont.foldLeft[OMFError.Throwables \/ Map[api.ConceptInstance, OWLAPIConceptInstance]] {
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
      }

      reifiedRelationshipInstances
      <- dbox2ont
        .foldLeft[OMFError.Throwables \/ Map[api.ReifiedRelationshipInstance, OWLAPIReifiedRelationshipInstance]] {
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
      }

      conceptualInstances
      = Map.empty[api.ConceptualEntitySingletonInstance, OWLAPIConceptualEntitySingletonInstance] ++
        conceptInstances ++ reifiedRelationshipInstances

      allDboxElements0
      = Map.empty[api.Element, common.Element] ++
        conceptualInstances ++
        dbox2ont.map { case (dbox, (_, ont_dbox)) => dbox -> ont_dbox }

      allDboxElements1
      <- dbox2ont
        .foldLeft[OMFError.Throwables \/ Map[api.Element, common.Element]] {
        allDboxElements0.right[OMFError.Throwables]
      } {
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
      }

      allDboxElements2
      <- dbox2ont.foldLeft(allDboxElements1.right[OMFError.Throwables]) {
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
      }

      allDboxElements3
      <- dbox2ont.foldLeft(allDboxElements2.right[OMFError.Throwables]) {
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
      }

      allDboxElements4
      <- dbox2ont.foldLeft(allDboxElements3.right[OMFError.Throwables]) {
        case (acc1, (dbox, (e, md))) =>
          implicit val ext: api.Extent = e
          e.singletonScalarDataPropertyValues.getOrElse(dbox, Set.empty).foldLeft(acc1) { case (acc2, scv0) =>
            for {
              prev <- acc2
              scv1 <- convertEntitySingletonInstanceScalarDataPropertyValue(prev, e2sc, conceptualInstances, e, md, scv0)
              next = prev + (scv0 -> scv1)
            } yield next
          }
      }

      allDboxElements5
      <- dbox2ont.foldLeft(allDboxElements4.right[OMFError.Throwables]) { case (acc1, (dbox, (e, md))) =>
        implicit val ext: api.Extent = e
        e.singletonStructuredDataPropertyValues.getOrElse(dbox, Set.empty).foldLeft(acc1) { case (acc2, stv) =>
          for {
            prev <- acc2
            next <- convertEntitySingletonInstanceStructuredDataPropertyValue(prev, e2st, s2sc, s2st, conceptualInstances, e, md, stv)
          } yield next
        }
      }

      _ <- dbox2ont.foldLeft(().right[OMFError.Throwables]) { case (acc1, (_, (e, md))) =>
        implicit val ext: api.Extent = e
        e.annotationProperties.foldLeft(acc1) { case (acc2, (_, ap)) =>
          for {
            _ <- acc2
            tap = TAnnotationProperty(ap.uuid.toString, ap.iri, ap.abbrevIRI)
            _ <- md.addAnnotationProperty(tap)
          } yield ()
        }
      }

      _ <- dbox2ont.foldLeft(().right[OMFError.Throwables]) { case (acc1, (_, (extent, md))) =>
        implicit val ext: api.Extent = extent
        ext.annotations.foldLeft(acc1) {
          case (acc2, (e: api.Element, as: Set[api.AnnotationPropertyValue])) =>
            for {
              _ <- acc2
              _ <- addElementAnnotations(allDboxElements5, md, e, as)
            } yield ()
          case (_, (m, _)) =>
            Set[java.lang.Throwable](
              OMFError.omfError(s"dbox2ont.addAnnotation: the module should have been a DBOX: $m")
            ).left
        }
      }

      m2i_tboxConvs <-
      moduleSort
        .foldLeft[OMFError.Throwables \/ (Seq[(api.Module, OWLAPIImmutableTerminologyBox)], Mutable2ImmutableModuleMap)] {
        (Seq.empty[(api.Module, OWLAPIImmutableTerminologyBox)], emptyMutable2ImmutableModuleMap).right
      } {
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
      }

      (tboxConvs, m2iTboxConv) = m2i_tboxConvs
      _ <- tboxConvs.foldLeft(().right[OMFError.Throwables]) { case (acc, (_, itbox)) =>
        for {
          _ <- acc
          _ = System.out.println(s"... Saving terminology ${itbox.iri}")
          _ <- ops.saveTerminology(itbox)
        } yield ()
      }

      dboxConvs_m2i <-
      moduleSort
        .foldLeft[OMFError.Throwables \/ (Seq[(api.Module, OWLAPIImmutableDescriptionBox)], Mutable2ImmutableModuleMap)] {
        (Seq.empty[(api.Module, OWLAPIImmutableDescriptionBox)], m2iTboxConv).right
      } {
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
      }

      (dboxConvs, _) = dboxConvs_m2i
      _ <- dboxConvs.foldLeft(().right[OMFError.Throwables]) { case (acc, (_, idbox)) =>
        for {
          _ <- acc
          _ = System.out.println(s"... Saving description ${idbox.iri}")
          _ <- ops.saveDescriptionBox(idbox)
        } yield ()
      }
    } yield moduleSort
  }

  final protected def addElementAnnotations
  (allTboxElements: Map[api.Element, common.Element],
   md: OWLAPIMutableTerminologyBox,
   e: api.Element,
   as: Set[api.AnnotationPropertyValue])
  (implicit ext: api.Extent, store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ Unit
  = as.foldLeft(().right[OMFError.Throwables]) { case (acc, a) =>
    for {
      _ <- acc
      ap = a.property
      tap = TAnnotationProperty(ap.uuid.toString, ap.iri, ap.abbrevIRI)
      ont_subject <- allTboxElements.get(e) match {
        case Some(e) =>
          e.right[OMFError.Throwables]
        case None =>
          Set[java.lang.Throwable](
            OMFError.omfError(s"addModuleAnnotations(tbox: ${md.iri}) missing element mapping for $e")
          ).left
      }
      _ <- md.addAnnotation(ont_subject, tap, a.value)
    } yield ()
  }

  final protected def addElementAnnotations
  (allDboxElements: Map[api.Element, common.Element],
   md: OWLAPIMutableDescriptionBox,
   e: api.Element,
   as: Set[api.AnnotationPropertyValue])
  (implicit ext: api.Extent, store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ Unit
  = as.foldLeft(().right[OMFError.Throwables]) { case (acc, a) =>
    for {
      _ <- acc
      ap = a.property
      tap = TAnnotationProperty(ap.uuid.toString, ap.iri, ap.abbrevIRI)
      ont_subject <- allDboxElements.get(e) match {
        case Some(e) =>
          e.right[OMFError.Throwables]
        case None =>
          Set[java.lang.Throwable](
            OMFError.omfError(s"addModuleAnnotations(dbox: ${md.iri}) missing element mapping for $e")
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
  (prev: Map[api.Element, common.Element],
   e2sc: Map[api.EntityScalarDataProperty, OWLAPIEntityScalarDataProperty],
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
      ont_vt <- scv.valueType match {
        case Some(vt: api.DataRange) =>
          prev.get(vt) match {
            case Some(vtr: types.terms.DataRange) =>
              Some(vtr).right
            case _ =>
              Set[java.lang.Throwable](OMFError.omfError(
                s"convertEntitySingletonInstanceScalarDataPropertyValue value type: ${scv.valueType} conversion"
              )).left
          }
        case None =>
          None.right
      }
      ont_s = conceptualInstances(scv.singletonInstance)
      ont_scv <- ops.addSingletonInstanceScalarDataPropertyValue(md, ont_s, ont_scp, scv.scalarPropertyValue, ont_vt)
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
        ont_vt <- sdv.valueType match {
          case Some(vt: api.DataRange) =>
            e2e.get(vt) match {
              case Some(vtr: types.terms.DataRange) =>
                Some(vtr).right
              case _ =>
                Set[java.lang.Throwable](OMFError.omfError(
                  s"convertSingletonInstanceScalarDataPropertyValues value type: ${sdv.valueType} conversion"
                )).left
            }
          case None =>
            None.right
        }
        ont_sdv <- ops.makeScalarDataPropertyValue(md, ont_context, ont_st2sc, sdv.scalarPropertyValue, ont_vt)
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
                    sr.minInclusive,
                    sr.maxInclusive,
                    sr.minExclusive,
                    sr.maxExclusive
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
                    sr.minInclusive,
                    sr.maxInclusive,
                    sr.minExclusive,
                    sr.maxExclusive
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
          case (Some(s: OWLAPIEntity), Some(t: OWLAPIEntity)) =>
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
          case (s, t) =>
            Set[java.lang.Throwable](OMFError.omfError(
              s"Unresolved unreifiedRelationship: $ur0" +
                s.fold{s"\n=> unresolved source: ${ur0.source}"}{_ => ""} +
                s.fold{s"\n=> unresolved target: ${ur0.target}"}{_ => ""}
            )).left
        }
      }
    }
  }

}
