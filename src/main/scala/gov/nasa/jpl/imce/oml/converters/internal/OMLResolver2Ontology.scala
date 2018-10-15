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

import java.lang.{IllegalArgumentException, System}
import java.util.UUID

import gov.nasa.jpl.imce.oml.converters.ConversionCommand
import gov.nasa.jpl.imce.oml.resolver.Filterable._
import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.resolver.Extent2Tables.toUUIDString
import gov.nasa.jpl.imce.oml.{resolver, tables}
import gov.nasa.jpl.omf.scala.binding.owlapi
import gov.nasa.jpl.omf.scala.binding.owlapi.descriptions.ImmutableDescriptionBox
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.ImmutableTerminologyBox
import gov.nasa.jpl.omf.scala.binding.owlapi.types.{terminologies => owlapiterminologies}
import gov.nasa.jpl.omf.scala.binding.owlapi.{OWLAPIOMFGraphStore, OntologyMapping, descriptions => owlapidescriptions}
import gov.nasa.jpl.omf.scala.core
import gov.nasa.jpl.omf.scala.core.OMFError
import org.semanticweb.owlapi.model.IRI

import scala.collection.immutable.{::, Iterable, List, Map, Nil, Seq, Set, Vector}
import scala.{Boolean, Function, None, Option, Some, StringContext, Unit}
import scala.Predef.{ArrowAssoc, String}
import scala.util.control.Exception._
import scalaz._
import Scalaz._
import org.semanticweb.owlapi.formats.OWLXMLDocumentFormatFactory
//import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormatFactory

object OMLResolver2Ontology {

  type MutableTboxOrDbox = owlapiterminologies.MutableTerminologyBox \/ owlapidescriptions.MutableDescriptionBox

  type Throwables = core.OMFError.Throwables

  type ResolverResult = Throwables \/ OMLResolver2Ontology

  def convert
  (extents: Seq[api.Extent],
   outStore: OWLAPIOMFGraphStore)
  : Throwables \/ OMLResolver2Ontology
  = for {
    out_drc <- outStore.loadBuiltinDatatypeMap()
    om <- outStore.initializeOntologyMapping(out_drc)

    r2oModules <- extents.foldLeft {
      OMLResolver2Ontology(om, outStore).right[OMFError.Throwables]
    } { case (acc, apiExtent) =>
      for {
        prev <- acc
        next <- OMLResolver2Ontology.convertExtent(prev)(apiExtent)
      } yield next
    }

    r2o <- convertAllExtents(r2oModules.right)

    tboxConversions <-
    r2o
      .modules
      .foldLeft[OMFError.Throwables \/ (Seq[ImmutableTerminologyBox], OMLResolver2Ontology)] {
      (Seq.empty[ImmutableTerminologyBox], r2o).right
    } {
      case (acc, m0: resolver.api.TerminologyBox) =>
        for {
          prev <- acc
          (convs, r2oPrev) = prev
          m1 <- r2oPrev.getTbox(m0)
          _ = System.out.println(s"... Converting terminology ${m1.sig.kind}: ${m1.iri}")
          next <- r2oPrev.ops.asImmutableTerminologyBox(m1, r2oPrev.om)(outStore).map { case (i1, omWithConv) =>
            (convs :+ i1) -> r2oPrev.copy(om = omWithConv)
          }
        } yield next
      case (acc, _) =>
        acc
    }

    (tboxConvs, r2oTboxConv) = tboxConversions

    _ <- tboxConvs.foldLeft[OMFError.Throwables \/ Unit](().right[OMFError.Throwables]) {
      case (acc, itbox) =>
        for {
          _ <- acc
          _ = System.out.println(s"... Saving terminology ${itbox.iri}")
          _ <- r2o.ops.saveTerminology(itbox)(outStore)
        } yield ()
    }

    dboxConversions <-
    r2o
      .modules
      .foldLeft[OMFError.Throwables \/ (Seq[ImmutableDescriptionBox], OMLResolver2Ontology)] {
      (Seq.empty[ImmutableDescriptionBox], r2oTboxConv).right
    } {
      case (acc, m0: resolver.api.DescriptionBox) =>
        for {
          prev <- acc
          (convs, r2oPrev) = prev
          m1 <- r2oPrev.getDbox(m0)
          _ = System.out.println(s"... Converting description ${m1.sig.kind}: ${m1.iri}")
          next <- r2oPrev.ops.asImmutableDescription(m1, r2oPrev.om)(outStore).map { case (i1, omWithConv) =>
            (convs :+ i1) -> r2oPrev.copy(om = omWithConv)
          }
        } yield next
      case (acc, _) =>
        acc
    }

    (dboxConvs, _) = dboxConversions

    _ <- dboxConvs.foldLeft[OMFError.Throwables \/ Unit](().right[OMFError.Throwables]) {
      case (acc, idbox) =>
        for {
          _ <- acc
          _ = System.out.println(s"... Saving description ${idbox.iri}")
          _ <- r2o.ops.saveDescriptionBox(idbox)(outStore)
        } yield ()
    }
  } yield r2o

  def convertToCombinedOntology
  (extents: Seq[api.Extent],
   outStore: OWLAPIOMFGraphStore,
   combinedIRI: String)
  : Throwables \/ OMLResolver2Ontology
  = for {
    out_drc <- outStore.loadBuiltinDatatypeMap()
    om <- outStore.initializeOntologyMapping(out_drc)

    r2oModules <- extents.foldLeft {
      OMLResolver2Ontology(om, outStore).right[OMFError.Throwables]
    } { case (acc, apiExtent) =>
      for {
        prev <- acc
        next <- OMLResolver2Ontology.convertExtent(prev)(apiExtent)
      } yield next
    }

    r2o <- convertAllExtents(r2oModules.right)

    tboxConversions <-
      r2o
        .modules
        .foldLeft[OMFError.Throwables \/ (Seq[ImmutableTerminologyBox], OMLResolver2Ontology)] {
        (Seq.empty[ImmutableTerminologyBox], r2o).right
      } {
        case (acc, m0: resolver.api.TerminologyBox) =>
          for {
            prev <- acc
            (convs, r2oPrev) = prev
            m1 <- r2oPrev.getTbox(m0)
            _ = System.out.println(s"... Converting terminology ${m1.sig.kind}: ${m1.iri}")
            next <- r2oPrev.ops.asImmutableTerminologyBox(m1, r2oPrev.om)(outStore).map { case (i1, omWithConv) =>
              (convs :+ i1) -> r2oPrev.copy(om = omWithConv)
            }
          } yield next
        case (acc, _) =>
          acc
      }

    (tboxConvs, r2oTboxConv) = tboxConversions

    combinedLogicalIRI = IRI.create(combinedIRI)
    combinedOnt = outStore.ontManager.createOntology(combinedLogicalIRI)

    _ <- tboxConvs.foldLeft[OMFError.Throwables \/ Unit](().right[OMFError.Throwables]) {
      case (acc, itbox) =>
        for {
          _ <- acc
          _ = if (outStore.isBuiltInIRI(itbox.iri))
            System.out.println(s"... Skip axioms from built-in ontology ${itbox.iri}")
          else {
            System.out.println(s"...     Adding axioms from terminology ${itbox.iri}")
            combinedOnt.addAxioms(itbox.ont.axioms())
          }
        } yield ()
    }

    dboxConversions <-
      r2o
        .modules
        .foldLeft[OMFError.Throwables \/ (Seq[ImmutableDescriptionBox], OMLResolver2Ontology)] {
        (Seq.empty[ImmutableDescriptionBox], r2oTboxConv).right
      } {
        case (acc, m0: resolver.api.DescriptionBox) =>
          for {
            prev <- acc
            (convs, r2oPrev) = prev
            m1 <- r2oPrev.getDbox(m0)
            _ = System.out.println(s"... Converting description ${m1.sig.kind}: ${m1.iri}")
            next <- r2oPrev.ops.asImmutableDescription(m1, r2oPrev.om)(outStore).map { case (i1, omWithConv) =>
              (convs :+ i1) -> r2oPrev.copy(om = omWithConv)
            }
          } yield next
        case (acc, _) =>
          acc
      }

    (dboxConvs, _) = dboxConversions

    _ <- dboxConvs.foldLeft[OMFError.Throwables \/ Unit](().right[OMFError.Throwables]) {
      case (acc, idbox) =>
        for {
          _ <- acc
          _ = System.out.println(s"...     Adding axioms from description ${idbox.iri}")
          _ = combinedOnt.addAxioms(idbox.ont.axioms())
        } yield ()
    }

    combinedOutputIRI = outStore.catalogIRIMapper.resolveIRI(combinedLogicalIRI, outStore.catalogIRIMapper.saveResolutionStrategy)

    //format = new FunctionalSyntaxDocumentFormatFactory().createFormat()
    format = new OWLXMLDocumentFormatFactory().createFormat()

    _ <- nonFatalCatch[OMFError.Throwables \/ Unit]
        .withApply {
          cause: java.lang.Throwable =>
            Set(cause).left
        }
        .apply {
          outStore.ontManager.saveOntology(combinedOnt, format, combinedOutputIRI).right
        }

    _ = System.out.println(s"Saved combined ontology in Functional Syntax for at as: $combinedOutputIRI")
  } yield r2o

  private def convertExtent
  (prev: OMLResolver2Ontology)
  (implicit extent: api.Extent)
  : Throwables \/ OMLResolver2Ontology
  = for {
    tuple <-
    (extent.terminologyGraphs.toList,
      extent.bundles.toList,
      extent.descriptionBoxes.toList) match {
      case (g :: Nil, Nil, Nil) =>
        Function.tupled(prev.convertTerminologyGraph _)(g)
      case (Nil, b :: Nil, Nil) =>
        Function.tupled(prev.convertBundle _)(b)
      case (Nil, Nil, d :: Nil) =>
        Function.tupled(prev.convertDescriptionBox _)(d)
      case (gs, bs, ds) =>
        Set[java.lang.Throwable](core.OMFError.omfError(
          s"OMLResolver2Text.convert: extent must have a single TerminologyGraph, Bundle or DescriptionBox: " +
            s" got ${gs.size} TerminologyGraphs, ${bs.size} Bundles, ${ds.size} DescriptionBoxes")).left
    }

    (next, m0, m1, i) = tuple

    iri = m0.iri

    updated = next.copy(extents = next.extents + extent)

    result <- if (updated.om.drc.isBuiltInModule(i)(updated.ops)) {
      System.out.println(s"==> OMLResolver2Ontology w/ builtin: $iri")
      updated.withBuiltIn(m0, m1)(extent)
    } else {
      //System.out.println(s"==> OMLResolver2Ontology converting: $iri")
      //convert(updated.right[Throwables], extent, iri, m0, m1, i)
      updated.right
    }
  } yield result

  // , iri: tables.taggedTypes.IRI, m0: api.Module, m1: MutableTboxOrDbox, i: IRI
  private def convertAllExtents(c00: Throwables \/ OMLResolver2Ontology)
  : Throwables \/ OMLResolver2Ontology
  = for {
    // AnnotationProperties
    c10 <- c00
    c11 <- convertAnnotationProperties(c10)
    c1N = c11

    // TerminologyExtensions
    c20 = c1N
    c21 <- convertTerminologyExtensions(c20)
    c2N = c21

    // Atomic Entities & DataTypes
    c30 = c2N
    c31 <- c30.queue_elements.foldLeft(c30.right[Throwables])(convertAspectOrConcept)
    c32 <- convertStructures(c31)
    c33 <- convertScalars(c32)
    c3N = c33

    // Data Ranges

    c40 = c3N
    c41 <- convertRestrictedDataRanges(c40)
    c42 <- convertScalarOneOfLiteralAxioms(c41)
    c4N = c42

    // Relational constructs

    c50 = c4N
    c51 <- eliminationConverter1(\/-(c50))
    c5N = c51

    // DataRelationships

    c60 = c5N
    c61 <- convertEntityScalarDataProperties(c60)
    c62 <- convertEntityStructuredDataProperties(c61)
    c63 <- convertScalarDataProperties(c62)
    c64 <- convertStructuredDataProperties(c63)
    c6N = c64

    // Restrictions

    c70 = c6N
    c71 <- convertEntityRestrictionAxioms(c70)
    c72 <- convertEntityDataPropertyRestrictionAxioms(c71)
    c7N = c72

    // Specializations

    c80 = c7N
    c81 <- convertSpecializationAxioms(c80)
    c82 <- convertSubPropertyOfAxioms(c81)
    c8N = c82

    // Disjunctions

    c90 = c8N
    c91 <- convertRootConceptTaxonomyAxioms(c90)
    c9N = c91

    cA0 = c9N
    cA1 <- convertChainRules(cA0)
    cAN = cA1

    // ConceptualEntityInstances & UnreifiedRelationshipInstanceTuples
    cB0 = cAN
    cB1 <- convertConceptInstances(cB0)
    cB2 <- convertReifiedRelationshipInstances(cB1)
    cB3 <- convertReifiedRelationshipInstanceDomains(cB2)
    cB4 <- convertReifiedRelationshipInstanceRanges(cB3)
    cB5 <- convertUnreifiedReifiedRelationshipInstanceTuples(cB4)
    cBN = cB5

    // Data Property Values
    cC0 = cBN
    cC1 <- convertSingletonInstanceScalarDataPropertyValues(cC0)
    cC2 <- convertSingletonInstanceStructuredDataPropertyValues(cC1)
    cC3 <- convertStructuredPropertyTuples(cC2)
    cC4 <- convertScalarDataPropertyValues(cC3)
    cCN = cC4

    // Annotations
    cD0 = cCN
    cD1 <- convertAnnotations(cD0)
    cDN = cD1

    // Finished!
    result = cDN
  } yield result

  private def convertTerminologyKind(k: tables.TerminologyKind): core.TerminologyKind = k match {
    case tables.OpenWorldDefinitions =>
      core.TerminologyKind.isOpenWorld
    case tables.ClosedWorldDesignations =>
      core.TerminologyKind.isClosedWorld
  }

  private def convertDescriptionKind(k: tables.DescriptionKind): core.DescriptionKind = k match {
    case tables.Final =>
      core.DescriptionKind.isFinal
    case tables.Partial =>
      core.DescriptionKind.isPartial
  }

  private def convertAnnotationProperties(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables]) {
    case (acc, ext) =>
      ext.annotationProperties.foldLeft(acc)(convertAnnotationProperty)
  }

  private def convertAnnotationProperty
  : (ResolverResult, (api.Module, Set[api.AnnotationProperty])) => ResolverResult
  = {
    case (acci, (m0, aps0)) =>
      for {
        prev <- acci
        next <- m0 match {
          case t0: api.TerminologyBox =>
            aps0.foldLeft(acci) { case (accj, ap0) =>
              for {
                r2o <- accj
                t1 <- r2o.getTbox(t0)
                ap1 <- r2o.ops.addTerminologyAnnotationProperty(
                  t1,
                  tables.AnnotationProperty(
                    ap0.uuid,
                    t1.uuid,
                    tables.taggedTypes.iri(ap0.iri),
                    tables.taggedTypes.abbrevIRI(ap0.abbrevIRI)))(r2o.omfStore)
                updated = r2o.copy(aps = r2o.aps + (ap0 -> ap1))
              } yield updated
            }
          case d0: api.DescriptionBox =>
            aps0.foldLeft(acci) { case (accj, ap0) =>
              for {
                r2o <- accj
                d1 <- r2o.getDbox(d0)
                ap1 <- r2o.ops.addDescriptionAnnotationProperty(
                  d1,
                  tables.AnnotationProperty(
                    ap0.uuid,
                    d1.uuid,
                    tables.taggedTypes.iri(ap0.iri),
                    tables.taggedTypes.abbrevIRI(ap0.abbrevIRI)))(r2o.omfStore)
                updated = r2o.copy(aps = r2o.aps + (ap0 -> ap1))
              } yield updated
            }
        }
      } yield next
  }

  private def convertAnnotations(r2o: OMLResolver2Ontology)
  : ResolverResult
  =  r2o.extents.foldLeft(r2o.right[Throwables]) {
    case (acc, ext) =>
      ext.annotations.foldLeft(acc)(convertAnnotations(ext))
  }

  private def convertAnnotations(ext: api.Extent)
  : (ResolverResult, (api.LogicalElement, Set[api.AnnotationPropertyValue])) => ResolverResult
  = {
    case (acc, (e0, apvs)) =>
      for {
        r2o <- acc
        _ <- e0 match {
          case x0: api.Module =>
            r2o.getModule(x0).flatMap { x1 =>
              convertElementAnnotations(r2o, x1, x1, apvs)
            }
          case x0: api.ModuleElement =>
            r2o.lookupModuleElement(x0)(ext).flatMap { x1 =>
              x0.moduleContext()(ext) match {
                case Some(m0) =>
                  r2o.getModule(m0).flatMap { m1 =>
                    convertElementAnnotations(r2o, m1, x1, apvs)
                  }
                case _ =>
                  Set[java.lang.Throwable](new IllegalArgumentException(
                    s"OMLResolver2Ontology.convertAnnotations(subject=$x0) not found!")).left
              }
            }
          case x0: api.ModuleEdge =>
            // @TODO
            ().right[Throwables]
          case x0: api.LogicalElement =>
            r2o.lookupOtherElement(x0)(ext).flatMap { x1 =>
              x0.moduleContext()(ext) match {
                case Some(m0) =>
                  r2o.getModule(m0).flatMap { m1 =>
                    convertElementAnnotations(r2o, m1, x1, apvs)
                  }
                case _ =>
                  Set[java.lang.Throwable](new IllegalArgumentException(
                    s"OMLResolver2Ontology.convertAnnotations(subject=$x0) not found!")).left
              }
            }
        }
      } yield r2o
  }

  private def convertElementAnnotations
  (r2o: OMLResolver2Ontology,
   m: owlapi.common.MutableModule,
   e: owlapi.common.LogicalElement,
   apvs: Set[api.AnnotationPropertyValue])
  : Throwables \/ Unit
  = apvs.foldLeft(().right[Throwables]) { case (acc, apv) =>
    for {
      _ <- acc
      ap <- r2o.aps.get(apv.property) match {
        case Some(p) =>
          p.right[Throwables]
        case _ =>
          Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
            s"OWLResolver2Ontology.convertModuleAnnotations(${apv.property}) not found."
          )).left
      }
      _ <- m.addAnnotation(e, ap, apv.value)(r2o.omfStore)
    } yield ()
  }

  // Atomic Entities

  private def convertAspectOrConcept
  (prev: ResolverResult, entry_pair:(api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement)))
  : ResolverResult
  = entry_pair._2 match {
    case (e0, a0: api.Aspect) =>
      for {
        r2o <- prev
        t1 <- r2o.lookupMap(a0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2o.getTbox)
        a1 <- r2o.ops.addAspect(t1, tables.taggedTypes.localName(a0.name))(r2o.omfStore)
      } yield r2o.copy(
        queue_elements = r2o.queue_elements - entry_pair._1,
        aspects = r2o.aspects + (a0 -> a1))
    case (e0, c0: api.Concept) =>
      for {
        r2o <- prev
        t1 <- r2o.lookupMap(c0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2o.getTbox)
        c1 <- r2o.ops.addConcept(t1, tables.taggedTypes.localName(c0.name))(r2o.omfStore)
      } yield r2o.copy(
        queue_elements = r2o.queue_elements - entry_pair._1,
        concepts = r2o.concepts + (c0 -> c1))
    case _ =>
      prev
  }

  // ModuleEdges

  private def convertTerminologyExtensions(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.queue_edges.foldLeft(r2o.right[Throwables])(convertTerminologyExtension)

  private def convertTerminologyExtension
  (acc: ResolverResult, entry_pair: (api.taggedTypes.ModuleEdgeUUID, (api.Extent, api.ModuleEdge)))
  = entry_pair._2 match {
    case (e0, ax0: api.TerminologyExtensionAxiom) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(ax0.tbox)
        e1 <- r2o.getTboxByIRI(ax0.extendedTerminology)
        ax1 <- r2o.ops.addTerminologyExtension(extendingG = t1, extendedG = e1)(r2o.omfStore)
      } yield r2o.copy(
        queue_edges = r2o.queue_edges - entry_pair._1,
        edges = r2o.edges + (ax0 -> ax1))
    case _ =>
      acc
  }

  case class ResolvableCardinalityAspectRestriction
  (uuid: api.taggedTypes.ModuleElementUUID,
   extent: api.Extent,
   cr: api.CardinalityRestrictedAspect,
   tbox: owlapiterminologies.MutableTerminologyBox,
   rel: owlapi.types.terms.RestrictableRelationship,
   range: Option[owlapi.types.terms.Entity])

  private def resolvableCardinalityAspectRestrictions
  (r2o: OMLResolver2Ontology)
  : Iterable[ResolvableCardinalityAspectRestriction]
  = r2o
    .queue_elements
    .collect { case (uuid, (extent: api.Extent, x: api.CardinalityRestrictedAspect)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.terminologyBoxOfTerminologyBoxStatement.get(x).flatMap(r2o.lookupTbox),
        r2o.restrictableRelationshipLookup(x.restrictedRelationship)(extent),
        x.restrictedRange,
        x.restrictedRange.map(r2o.entityLookup(_)(extent)) ) match {
        case (Some(tbox), \/-(rel), Some(_), Some(\/-(range))) =>
          Some(ResolvableCardinalityAspectRestriction(uuid, extent, x, tbox, rel, Some(range)))
        case (Some(tbox), \/-(rel), None, _) =>
          Some(ResolvableCardinalityAspectRestriction(uuid, extent, x, tbox, rel, None))
        case _ =>
          None
      }
    }

  private def updateCardinalityAspectRestrictions
  (current: ResolverResult,
   x: ResolvableCardinalityAspectRestriction)
  : ResolverResult
  = for {
    r2o <- current
    y <- r2o.ops.addCardinalityRestrictedAspect(x.tbox, x.cr.name, x.cr.restrictionKind, x.rel, x.range, x.cr.restrictedCardinality)(r2o.omfStore)
    next = r2o.copy(
      queue_elements = r2o.queue_elements - x.uuid,
      aspects = r2o.aspects + (x.cr -> y))
  } yield next

  case class ResolvableCardinalityConceptRestriction
  (uuid: api.taggedTypes.ModuleElementUUID,
   extent: api.Extent,
   cr: api.CardinalityRestrictedConcept,
   tbox: owlapiterminologies.MutableTerminologyBox,
   rel: owlapi.types.terms.RestrictableRelationship,
   range: Option[owlapi.types.terms.Entity])

  private def resolvableCardinalityConceptRestrictions
  (r2o: OMLResolver2Ontology)
  : Iterable[ResolvableCardinalityConceptRestriction]
  = r2o
    .queue_elements
    .collect { case (uuid, (extent: api.Extent, x: api.CardinalityRestrictedConcept)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.terminologyBoxOfTerminologyBoxStatement.get(x).flatMap(r2o.lookupTbox),
        r2o.restrictableRelationshipLookup(x.restrictedRelationship)(extent),
        x.restrictedRange,
        x.restrictedRange.map(r2o.entityLookup(_)(extent)) ) match {
        case (Some(tbox), \/-(rel), Some(_), Some(\/-(range))) =>
          Some(ResolvableCardinalityConceptRestriction(uuid, extent, x, tbox, rel, Some(range)))
        case (Some(tbox), \/-(rel), None, _) =>
          Some(ResolvableCardinalityConceptRestriction(uuid, extent, x, tbox, rel, None))
        case _ =>
          None
      }
    }

  private def updateCardinalityConceptRestrictions
  (current: ResolverResult,
   x: ResolvableCardinalityConceptRestriction)
  : ResolverResult
  = for {
    r2o <- current
    y <- r2o.ops.addCardinalityRestrictedConcept(x.tbox, x.cr.name, x.cr.restrictionKind, x.rel, x.range, x.cr.restrictedCardinality)(r2o.omfStore)
    next = r2o.copy(
      queue_elements = r2o.queue_elements - x.uuid,
      concepts = r2o.concepts + (x.cr -> y))
  } yield next

  case class ResolvableCardinalityReifiedRelationshipRestriction
  (uuid: api.taggedTypes.ModuleElementUUID,
   extent: api.Extent,
   cr: api.CardinalityRestrictedReifiedRelationship,
   tbox: owlapiterminologies.MutableTerminologyBox,
   rel: owlapi.types.terms.RestrictableRelationship,
   range: Option[owlapi.types.terms.Entity])

  private def resolvableCardinalityReifiedRelationshipRestrictions
  (r2o: OMLResolver2Ontology)
  : Iterable[ResolvableCardinalityReifiedRelationshipRestriction]
  = r2o
    .queue_elements
    .collect { case (uuid, (extent: api.Extent, x: api.CardinalityRestrictedReifiedRelationship)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.terminologyBoxOfTerminologyBoxStatement.get(x).flatMap(r2o.lookupTbox),
        r2o.restrictableRelationshipLookup(x.restrictedRelationship)(extent),
        x.restrictedRange,
        x.restrictedRange.map(r2o.entityLookup(_)(extent)) ) match {
        case (Some(tbox), \/-(rel), Some(_), Some(\/-(range))) =>
          Some(ResolvableCardinalityReifiedRelationshipRestriction(uuid, extent, x, tbox, rel, Some(range)))
        case (Some(tbox), \/-(rel), None, _) =>
          Some(ResolvableCardinalityReifiedRelationshipRestriction(uuid, extent, x, tbox, rel, None))
        case _ =>
          None
      }
    }

  private def updateCardinalityReifiedRelationshipRestrictions
  (current: ResolverResult,
   x: ResolvableCardinalityReifiedRelationshipRestriction)
  : ResolverResult
  = for {
    r2o <- current
    y <- r2o.ops.addCardinalityRestrictedReifiedRelationship(x.tbox, x.cr.name, x.cr.restrictionKind, x.rel, x.range, x.cr.restrictedCardinality)(r2o.omfStore)
    next = r2o.copy(
      queue_elements = r2o.queue_elements - x.uuid,
      cardinalityRestrictedReifiedRelationships = r2o.cardinalityRestrictedReifiedRelationships + (x.cr -> y))
  } yield next

  case class ResolvableReifiedRelationshipRestriction
  (uuid: api.taggedTypes.ModuleElementUUID,
   extent: api.Extent,
   rrr: api.ReifiedRelationshipRestriction,
   tbox: owlapiterminologies.MutableTerminologyBox,
   source: owlapi.types.terms.Entity,
   target: owlapi.types.terms.Entity)

  private def resolvableReifiedRelationshipRestrictions
  (r2o: OMLResolver2Ontology)
  : Iterable[ResolvableReifiedRelationshipRestriction]
  = r2o
    .queue_elements
    .collect { case (uuid, (extent: api.Extent, x: api.ReifiedRelationshipRestriction)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.terminologyBoxOfTerminologyBoxStatement.get(x).flatMap(r2o.lookupTbox),
        r2o.lookupEntity(x.source),
        r2o.lookupEntity(x.target) ) match {
        case (Some(tbox), Some(source), Some(target)) =>
          Some(ResolvableReifiedRelationshipRestriction(uuid, extent, x, tbox, source, target))
        case _ =>
          None
      }
    }

  private def updateReifiedRelationshipRestrictions
  (current: ResolverResult,
   x: ResolvableReifiedRelationshipRestriction)
  : ResolverResult
  = for {
    r2o <- current
    y <- r2o.ops.addReifiedRelationshipRestriction(x.tbox, x.rrr.name, x.source, x.target)(r2o.omfStore)
    next = r2o.copy(
      queue_elements = r2o.queue_elements - x.uuid,
      reifiedRelationshipRestrictions = r2o.reifiedRelationshipRestrictions + (x.rrr -> y))
  } yield next
  
  case class ResolvableConceptDesignationTerminologyAxiom
  (uuid: api.taggedTypes.ModuleEdgeUUID,
   extent: api.Extent,
   ax: api.ConceptDesignationTerminologyAxiom,
   tbox: owlapiterminologies.MutableTerminologyBox,
   c: owlapi.types.terms.ConceptKind,
   desTbox: owlapiterminologies.MutableTerminologyBox)

  private def resolvableConceptDesignationTerminologyAxioms
  (r2o: OMLResolver2Ontology)
  : Iterable[ResolvableConceptDesignationTerminologyAxiom]
  = r2o
    .queue_edges
    .collect { case (uuid, (extent: api.Extent, x: api.ConceptDesignationTerminologyAxiom)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.terminologyBoxOfTerminologyBoxAxiom.get(x).flatMap(r2o.lookupTbox),
        r2o.lookupConcept(x.designatedConcept)(extent),
        r2o.getTboxByIRI(x.designatedTerminology)) match {
        case (Some(tbox), \/-(c), \/-(desTbox)) =>
          Some(ResolvableConceptDesignationTerminologyAxiom(uuid, extent, x, tbox, c, desTbox))
        case _ =>
          None
      }
    }

  private def updateConceptDesignationTerminologyAxioms
  (current: ResolverResult,
   x: ResolvableConceptDesignationTerminologyAxiom)
  : ResolverResult
  = for {
    r2o <- current
    y <- r2o.ops.addEntityConceptDesignationTerminologyAxiom(x.tbox, x.c, x.desTbox)(r2o.omfStore)
    next = r2o.copy(
      queue_edges = r2o.queue_edges - x.uuid,
      edges = r2o.edges + (x.ax -> y))
  } yield next

  case class ResolvableTerminologyNestingAxiom
  (uuid: api.taggedTypes.ModuleEdgeUUID,
   extent: api.Extent,
   ax: api.TerminologyNestingAxiom,
   tg: owlapiterminologies.MutableTerminologyGraph,
   nestingC: owlapi.types.terms.ConceptKind,
   nestingTbox: owlapiterminologies.MutableTerminologyBox)

  private def resolvableTerminologyNestingAxioms
  (r2o: OMLResolver2Ontology)
  : Iterable[ResolvableTerminologyNestingAxiom]
  = r2o
    .queue_edges
    .collect { case (uuid, (extent: api.Extent, x: api.TerminologyNestingAxiom)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.terminologyBoxOfTerminologyBoxAxiom.get(x).flatMap(r2o.lookupGbox),
        r2o.lookupConcept(x.nestingContext)(extent),
        r2o.getTboxByIRI(x.nestingTerminology)) match {
        case (Some(tg), \/-(c), \/-(nestingTbox)) =>
          Some(ResolvableTerminologyNestingAxiom(uuid, extent, x, tg, c, nestingTbox))
        case _ =>
          None
      }
    }

  private def updateTerminologyNestingAxioms
  (current: ResolverResult,
   x: ResolvableTerminologyNestingAxiom)
  : ResolverResult
  = for {
    r2o <- current
    y <- r2o.ops.addNestedTerminology(x.nestingTbox, x.nestingC, x.tg)(r2o.omfStore)
    next = r2o.copy(
      queue_edges = r2o.queue_edges - x.uuid,
      edges = r2o.edges + (x.ax -> y))
  } yield next

  case class ResolvableBundledTerminologyAxiom
  (uuid: api.taggedTypes.ModuleEdgeUUID,
   extent: api.Extent,
   ax: api.BundledTerminologyAxiom,
   bundle: owlapiterminologies.MutableBundle,
   bundledTbox: owlapiterminologies.MutableTerminologyBox)

  private def resolvableBundledTerminologyAxioms
  (r2o: OMLResolver2Ontology)
  : Iterable[ResolvableBundledTerminologyAxiom]
  = r2o
    .queue_edges
    .collect { case (uuid, (extent: api.Extent, x: api.BundledTerminologyAxiom)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.bundleOfTerminologyBundleAxiom.get(x).flatMap(r2o.bs.get),
       r2o.getTboxByIRI(x.bundledTerminology)) match {
        case (Some(bundle), \/-(bundledTbox)) =>
          Some(ResolvableBundledTerminologyAxiom(uuid, extent, x, bundle, bundledTbox))
        case _ =>
          None
      }
    }

  private def updateBundledTerminologyAxioms
  (current: ResolverResult,
   x: ResolvableBundledTerminologyAxiom)
  : ResolverResult
  = for {
    r2o <- current
    y <- r2o.ops.addBundledTerminologyAxiom(x.bundle, x.bundledTbox)(r2o.omfStore)
    next = r2o.copy(
      queue_edges = r2o.queue_edges - x.uuid,
      edges = r2o.edges + (x.ax -> y))
  } yield next

  case class ResolvableDescriptionBoxExtendsClosedWorldDefinitions
  (uuid: api.taggedTypes.ModuleEdgeUUID,
   extent: api.Extent,
   ax: api.DescriptionBoxExtendsClosedWorldDefinitions,
   dbox: owlapi.descriptions.MutableDescriptionBox,
   tbox: owlapiterminologies.MutableTerminologyBox)

  private def resolvableDescriptionBoxExtendsClosedWorldDefinitions
  (r2o: OMLResolver2Ontology)
  : Iterable[ResolvableDescriptionBoxExtendsClosedWorldDefinitions]
  = r2o
    .queue_edges
    .collect { case (uuid, (extent: api.Extent, x: api.DescriptionBoxExtendsClosedWorldDefinitions)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.descriptionBoxOfDescriptionBoxExtendsClosedWorldDefinitions.get(x).flatMap(r2o.ds.get),
        r2o.getTboxByIRI(x.closedWorldDefinitions)) match {
        case (Some(dbox), \/-(tbox)) =>
          Some(ResolvableDescriptionBoxExtendsClosedWorldDefinitions(uuid, extent, x, dbox, tbox))
        case _ =>
          None
      }
    }

  private def updateDescriptionBoxExtendsClosedWorldDefinitions
  (current: ResolverResult,
   x: ResolvableDescriptionBoxExtendsClosedWorldDefinitions)
  : ResolverResult
  = for {
    r2o <- current
    y <- r2o.ops.addDescriptionBoxExtendsClosedWorldDefinitions(x.dbox, x.tbox)(r2o.omfStore)
    next = r2o.copy(
      queue_edges = r2o.queue_edges - x.uuid,
      edges = r2o.edges + (x.ax -> y))
  } yield next

  case class ResolvableDescriptionBoxRefinement
  (uuid: api.taggedTypes.ModuleEdgeUUID,
   extent: api.Extent,
   ax: api.DescriptionBoxRefinement,
   dbox: owlapi.descriptions.MutableDescriptionBox,
   refined: owlapi.descriptions.MutableDescriptionBox)

  private def resolvableDescriptionBoxRefinements
  (r2o: OMLResolver2Ontology)
  : Iterable[ResolvableDescriptionBoxRefinement]
  = r2o
    .queue_edges
    .collect { case (uuid, (extent: api.Extent, x: api.DescriptionBoxRefinement)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.descriptionBoxOfDescriptionBoxRefinement.get(x).flatMap(r2o.ds.get),
        r2o.getDboxByIRI(x.refinedDescriptionBox)) match {
        case (Some(dbox), \/-(refined)) =>
          Some(ResolvableDescriptionBoxRefinement(uuid, extent, x, dbox, refined))
        case _ =>
          None
      }
    }

  private def updateDescriptionBoxRefinements
  (current: ResolverResult,
   x: ResolvableDescriptionBoxRefinement)
  : ResolverResult
  = for {
    r2o <- current
    y <- r2o.ops.addDescriptionBoxRefinement(x.dbox, x.refined)(r2o.omfStore)
    next = r2o.copy(
      queue_edges = r2o.queue_edges - x.uuid,
      edges = r2o.edges + (x.ax -> y))
  } yield next

  case class ResolvableReifiedRelationship
  (uuid: api.taggedTypes.ModuleElementUUID,
   extent: api.Extent,
   rr: api.ReifiedRelationship,
   tbox: owlapiterminologies.MutableTerminologyBox,
   source: owlapi.types.terms.Entity,
   target: owlapi.types.terms.Entity,
   fwd: api.ForwardProperty,
   inv: Option[api.InverseProperty])

  private def resolvableReifiedRelationships
  (r2o: OMLResolver2Ontology)
  : Iterable[ResolvableReifiedRelationship]
  = r2o
    .queue_elements
    .collect { case (uuid, (extent: api.Extent, x: api.ReifiedRelationship)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.terminologyBoxOfTerminologyBoxStatement.get(x).flatMap(r2o.lookupTbox),
        r2o.lookupEntity(x.source),
        r2o.lookupEntity(x.target),
        x.allNestedElements()(extent).collectFirst { case f: api.ForwardProperty => f },
        x.allNestedElements()(extent).collectFirst { case i: api.InverseProperty => i } ) match {
        case (Some(tbox), Some(source), Some(target), Some(fwd), inv) =>
          Some(ResolvableReifiedRelationship(uuid, extent, x, tbox, source, target, fwd, inv))
        case _ =>
          None
      }
    }

  private def updateReifiedRelationships
  (current: ResolverResult,
   x: ResolvableReifiedRelationship)
  : ResolverResult
  = for {
    r2o <- current
    characteristics = Iterable.empty[core.RelationshipCharacteristics.RelationshipCharacteristics] ++
      (if (x.rr.isAsymmetric)
        Iterable(core.RelationshipCharacteristics.isAsymmetric)
      else Iterable()) ++
      (if (x.rr.isEssential)
        Iterable(core.RelationshipCharacteristics.isEssential)
      else Iterable()) ++
      (if (x.rr.isFunctional)
        Iterable(core.RelationshipCharacteristics.isFunctional)
      else Iterable()) ++
      (if (x.rr.isInverseEssential)
        Iterable(core.RelationshipCharacteristics.isInverseEssential)
      else Iterable()) ++
      (if (x.rr.isInverseFunctional)
        Iterable(core.RelationshipCharacteristics.isInverseFunctional)
      else Iterable()) ++
      (if (x.rr.isIrreflexive)
        Iterable(core.RelationshipCharacteristics.isIrreflexive)
      else Iterable()) ++
      (if (x.rr.isReflexive)
        Iterable(core.RelationshipCharacteristics.isReflexive)
      else Iterable()) ++
      (if (x.rr.isSymmetric)
        Iterable(core.RelationshipCharacteristics.isSymmetric)
      else Iterable()) ++
      (if (x.rr.isTransitive)
        Iterable(core.RelationshipCharacteristics.isTransitive)
      else Iterable())
    y <- r2o.ops.addReifiedRelationship(x.tbox, x.source, x.target, characteristics, x.rr.name, x.fwd.name, x.inv.map(_.name))(r2o.omfStore)
    invPair <- (x.inv, y.inverseProperty) match {
      case (Some(i0), Some(i1)) =>
        Some(i0 -> i1).right
      case (None, None) =>
        None.right
      case _ =>
        -\/(Set[java.lang.Throwable](new IllegalArgumentException(
          s"updateReifiedRelationships: inconsistent inverse properties for ${x.rr.abbrevIRI()(x.extent)}"
        )))
    }
    next = r2o.copy(
      queue_elements = r2o.queue_elements - x.uuid,
      reifiedRelationships = r2o.reifiedRelationships + (x.rr -> y),
      forwardProperties = r2o.forwardProperties + (x.fwd -> y.forwardProperty),
      inverseProperties = r2o.inverseProperties ++ invPair)
  } yield next

  case class ResolvableUnreifiedRelationship
  (uuid: api.taggedTypes.ModuleElementUUID,
   extent: api.Extent,
   ur: api.UnreifiedRelationship,
   tbox: owlapiterminologies.MutableTerminologyBox,
   source: owlapi.types.terms.Entity,
   target: owlapi.types.terms.Entity)

  private def resolvableUnreifiedRelationships
  (r2o: OMLResolver2Ontology)
  : Iterable[ResolvableUnreifiedRelationship]
  = r2o
    .queue_elements
    .collect { case (uuid, (extent: api.Extent, x: api.UnreifiedRelationship)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.terminologyBoxOfTerminologyBoxStatement.get(x).flatMap(r2o.lookupTbox),
        r2o.lookupEntity(x.source),
        r2o.lookupEntity(x.target) ) match {
        case (Some(tbox), Some(source), Some(target)) =>
          Some(ResolvableUnreifiedRelationship(uuid, extent, x, tbox, source, target))
        case _ =>
          None
      }
    }

  private def updateUnreifiedRelationships
  (current: ResolverResult,
   x: ResolvableUnreifiedRelationship)
  : ResolverResult
  = for {
    r2o <- current
    characteristics = Iterable.empty[core.RelationshipCharacteristics.RelationshipCharacteristics] ++
      (if (x.ur.isAsymmetric)
        Iterable(core.RelationshipCharacteristics.isAsymmetric)
      else Iterable()) ++
      (if (x.ur.isEssential)
        Iterable(core.RelationshipCharacteristics.isEssential)
      else Iterable()) ++
      (if (x.ur.isFunctional)
        Iterable(core.RelationshipCharacteristics.isFunctional)
      else Iterable()) ++
      (if (x.ur.isInverseEssential)
        Iterable(core.RelationshipCharacteristics.isInverseEssential)
      else Iterable()) ++
      (if (x.ur.isInverseFunctional)
        Iterable(core.RelationshipCharacteristics.isInverseFunctional)
      else Iterable()) ++
      (if (x.ur.isIrreflexive)
        Iterable(core.RelationshipCharacteristics.isIrreflexive)
      else Iterable()) ++
      (if (x.ur.isReflexive)
        Iterable(core.RelationshipCharacteristics.isReflexive)
      else Iterable()) ++
      (if (x.ur.isTransitive)
        Iterable(core.RelationshipCharacteristics.isTransitive)
      else Iterable())
    y <- r2o.ops.addUnreifiedRelationship(x.tbox, x.source, x.target, characteristics, x.ur.name)(r2o.omfStore)
    next = r2o.copy(
      queue_elements = r2o.queue_elements - x.uuid,
      unreifiedRelationships = r2o.unreifiedRelationships + (x.ur -> y))
  } yield next

  @scala.annotation.tailrec
  final def eliminationConverter1
  (c0: ResolverResult)
  : ResolverResult
  = c0 match {
    case \/-(r2o) =>
      if (r2o.isResolved)
        \/-(r2o)
      else {
        val r1 = resolvableCardinalityAspectRestrictions(r2o)
        val r2 = resolvableCardinalityConceptRestrictions(r2o)
        val r3 = resolvableCardinalityReifiedRelationshipRestrictions(r2o)
        val r4 = resolvableConceptDesignationTerminologyAxioms(r2o)
        val r5 = resolvableTerminologyNestingAxioms(r2o)
        val r6 = resolvableBundledTerminologyAxioms(r2o)
        val r7 = resolvableDescriptionBoxExtendsClosedWorldDefinitions(r2o)
        val r8 = resolvableDescriptionBoxRefinements(r2o)
        val r9 = resolvableReifiedRelationships(r2o)
        val rA = resolvableUnreifiedRelationships(r2o)
        val rB = resolvableReifiedRelationshipRestrictions(r2o)

        val more
        = r1.nonEmpty ||
          r2.nonEmpty ||
          r3.nonEmpty ||
          r4.nonEmpty ||
          r5.nonEmpty ||
          r6.nonEmpty ||
          r7.nonEmpty ||
          r8.nonEmpty ||
          r9.nonEmpty ||
          rA.nonEmpty ||
          rB.nonEmpty
        
        if (more) {
          val c1 = r1.foldLeft(c0)(updateCardinalityAspectRestrictions)
          val c2 = r2.foldLeft(c1)(updateCardinalityConceptRestrictions)
          val c3 = r3.foldLeft(c2)(updateCardinalityReifiedRelationshipRestrictions)
          val c4 = r4.foldLeft(c3)(updateConceptDesignationTerminologyAxioms)
          val c5 = r5.foldLeft(c4)(updateTerminologyNestingAxioms)
          val c6 = r6.foldLeft(c5)(updateBundledTerminologyAxioms)
          val c7 = r7.foldLeft(c6)(updateDescriptionBoxExtendsClosedWorldDefinitions)
          val c8 = r8.foldLeft(c7)(updateDescriptionBoxRefinements)
          val c9 = r9.foldLeft(c8)(updateReifiedRelationships)
          val cA = rA.foldLeft(c9)(updateUnreifiedRelationships)
          val cB = rB.foldLeft(cA)(updateReifiedRelationshipRestrictions)
          eliminationConverter1(cB)
        } else
          c0
      }
    case -\/(errors) =>
      -\/(errors)
  }
  
  // DataTypes

  private def convertStructures(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables])(convertStructures)

  private def convertStructures(acc: ResolverResult, e: api.Extent)
  : ResolverResult
  = e.terminologyBoxOfTerminologyBoxStatement.foldLeft(acc) {
    case (prev, (s0: api.Structure, t0: api.TerminologyBox)) =>
      for {
        r2o <- prev
        t1 <- r2o.getTbox(t0)
        s1 <- r2o.ops.addStructuredDataType(t1, tables.taggedTypes.localName(s0.name))(r2o.omfStore)
      } yield r2o.copy(
        queue_elements = r2o.queue_elements - s0.uuid,
        structures = r2o.structures + (s0 -> s1))
    case (prev, _) =>
      prev
  }

  private def convertScalars(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables])(convertScalars)

  private def convertScalars(acc: ResolverResult, e: api.Extent)
  : ResolverResult
  = e.terminologyBoxOfTerminologyBoxStatement.foldLeft(acc) {
    case (prev, (s0: api.Scalar, t0: api.TerminologyBox)) =>
      for {
        r2o <- prev
        t1 <- r2o.getTbox(t0)
        s1 <- r2o.ops.addScalarDataType(t1, tables.taggedTypes.localName(s0.name))(r2o.omfStore)
      } yield r2o.copy(
        queue_elements = r2o.queue_elements - s0.uuid,
        dataRanges = r2o.dataRanges + (s0 -> s1))
    case (prev, _) =>
      prev
  }

  private def convertRestrictedDataRanges
  (r2o: OMLResolver2Ontology)
  : ResolverResult
  = {
    val edrt =
      r2o
        .extents
        .flatMap { e =>
          e
            .terminologyBoxOfTerminologyBoxStatement
            .collect { case (dr: api.RestrictedDataRange, t: api.TerminologyBox) => (dr.uuid, e, dr, t) }
        }

    convertRestrictedDataRanges(r2o.right, edrt, List.empty)(r2o.omfStore)
  }

  @scala.annotation.tailrec
  private def convertRestrictedDataRanges
  (acc: ResolverResult,
   edrt: Iterable[(api.taggedTypes.ModuleElementUUID, api.Extent, api.RestrictedDataRange, api.TerminologyBox)],
   queue: List[(api.taggedTypes.ModuleElementUUID, api.Extent, api.RestrictedDataRange, api.TerminologyBox)],
   progress: Boolean = false)
  (implicit omfStore: owlapi.OWLAPIOMF#Store)
  : ResolverResult
  = if (edrt.isEmpty) {
    if (queue.isEmpty)
      acc
    else if (progress)
      convertRestrictedDataRanges(acc, queue, List.empty)
    else
      Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
        ConversionCommand.explainProblems(
          s"convertRestrictedDataRanges: no progress with ${queue.size} data ranges in the queue",
          queue.map(_._3.name)))).left
  }
  else acc match {
    case \/-(r2o) =>
      val (uuid, e0, dr0, t0) = edrt.head
      (r2o.lookupTbox(t0), r2o.dataRanges.get(dr0.restrictedRange)) match {
        case (Some(t1), Some(rr1)) =>
          val dr1 = dr0 match {
            case rdr0: api.BinaryScalarRestriction =>
              r2o.ops
                .addBinaryScalarRestriction(
                  t1, tables.taggedTypes.localName(rdr0.name), rr1,
                  rdr0.length, rdr0.minLength, rdr0.maxLength)
                .map { rdr1 => r2o.copy(
                  queue_elements = r2o.queue_elements - uuid,
                  dataRanges = r2o.dataRanges + (rdr0 -> rdr1))
                }
            case rdr0: api.IRIScalarRestriction =>
              r2o.ops
                .addIRIScalarRestriction(
                  t1, tables.taggedTypes.localName(rdr0.name), rr1,
                  rdr0.length, rdr0.minLength, rdr0.maxLength, rdr0.pattern)
                .map { rdr1 => r2o.copy(
                  queue_elements = r2o.queue_elements - uuid,
                  dataRanges = r2o.dataRanges + (rdr0 -> rdr1))
                }
            case rdr0: api.NumericScalarRestriction =>
              r2o.ops
                .addNumericScalarRestriction(
                  t1, tables.taggedTypes.localName(rdr0.name), rr1,
                  rdr0.minInclusive, rdr0.maxInclusive,
                  rdr0.minExclusive, rdr0.maxExclusive)
                .map { rdr1 => r2o.copy(
                  queue_elements = r2o.queue_elements - uuid,
                  dataRanges = r2o.dataRanges + (rdr0 -> rdr1))
                }
            case rdr0: api.PlainLiteralScalarRestriction =>
              r2o.ops
                .addPlainLiteralScalarRestriction(
                  t1, tables.taggedTypes.localName(rdr0.name), rr1,
                  rdr0.length, rdr0.minLength, rdr0.maxLength, rdr0.pattern,
                  rdr0.langRange.map(r => tables.taggedTypes.languageTagDataType(r)))
                .map { rdr1 => r2o.copy(
                  queue_elements = r2o.queue_elements - uuid,
                  dataRanges = r2o.dataRanges + (rdr0 -> rdr1))
                }
            case rdr0: api.ScalarOneOfRestriction =>
              r2o.ops
                .addScalarOneOfRestriction(
                  t1, tables.taggedTypes.localName(rdr0.name), rr1)
                .map { rdr1 => r2o.copy(
                  queue_elements = r2o.queue_elements - uuid,
                  dataRanges = r2o.dataRanges + (rdr0 -> rdr1))
                }
            case rdr0: api.StringScalarRestriction =>
              r2o.ops
                .addStringScalarRestriction(
                  t1, tables.taggedTypes.localName(rdr0.name), rr1,
                  rdr0.length, rdr0.minLength, rdr0.maxLength, rdr0.pattern)
                .map { rdr1 => r2o.copy(
                  queue_elements = r2o.queue_elements - uuid,
                  dataRanges = r2o.dataRanges + (rdr0 -> rdr1))
                }
            case rdr0: api.SynonymScalarRestriction =>
              r2o.ops
                .addSynonymScalarRestriction(
                  t1, tables.taggedTypes.localName(rdr0.name), rr1)
                .map { rdr1 => r2o.copy(
                  queue_elements = r2o.queue_elements - uuid,
                  dataRanges = r2o.dataRanges + (rdr0 -> rdr1))
                }
            case rdr0: api.TimeScalarRestriction =>
              r2o.ops
                .addTimeScalarRestriction(
                  t1, tables.taggedTypes.localName(rdr0.name), rr1,
                  rdr0.minInclusive, rdr0.maxInclusive,
                  rdr0.minExclusive, rdr0.maxExclusive)
                .map { rdr1 => r2o.copy(
                  queue_elements = r2o.queue_elements - uuid,
                  dataRanges = r2o.dataRanges + (rdr0 -> rdr1))
                }
          }
          dr1 match {
            case \/-(next) =>
              convertRestrictedDataRanges(\/-(next), edrt.tail, queue, progress = true)
            case -\/(errors) =>
              -\/(errors)
          }
        case (Some(_), None) =>
          val rest = edrt.tail
          if (rest.isEmpty)
            convertRestrictedDataRanges(acc, edrt.head :: queue, List.empty, progress)
          else
            convertRestrictedDataRanges(acc, rest, edrt.head :: queue)
        case (None, _) =>
          Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
            s"convertRestrictedDataRanges: Failed to resolve " +
              s"tbox: $t0")).left
      }
    case _ =>
      acc
  }

  private def convertScalarOneOfLiteralAxioms(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables])(convertScalarOneOfLiteralAxioms)

  private def convertScalarOneOfLiteralAxioms(acc: ResolverResult, e: api.Extent)
  : ResolverResult
  = e.terminologyBoxOfTerminologyBoxStatement.foldLeft(acc) {
    case (prev, (s0: api.ScalarOneOfLiteralAxiom, t0: api.TerminologyBox)) =>
      for {
        r2o <- prev
        t1 <- r2o.getTbox(t0)
        r1 <- r2o.lookupScalarOneOfRestriction(s0.axiom)(e)
        vt1 <- s0.valueType match {
          case Some(vt0) =>
            r2o.lookupDataRange(vt0)(e).map(Option.apply)
          case None =>
            None.right
        }
        s1 <- r2o.ops.addScalarOneOfLiteralAxiom(t1, r1, s0.value, vt1)(r2o.omfStore)
      } yield r2o.copy(
        queue_elements = r2o.queue_elements - s0.uuid,
        termAxioms = r2o.termAxioms + (s0 -> s1))
    case (prev, _) =>
      prev
  }

  // DataRelationships

  private def convertEntityScalarDataProperties(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables])(convertEntityScalarDataProperties)

  private def convertEntityScalarDataProperties(acc: ResolverResult, e: api.Extent)
  : ResolverResult
  = e.terminologyBoxOfTerminologyBoxStatement.foldLeft(acc) {
    case (prev, (dp0: api.EntityScalarDataProperty, t0: api.TerminologyBox)) =>
      for {
        r2o <- prev
        t1 <- r2o.getTbox(t0)
        e1 <- r2o.entityLookup(dp0.domain)(e)
        r1 <- r2o.lookupDataRange(dp0.range)(e)
        dp1 <- r2o.ops.addEntityScalarDataProperty(
          t1, e1, r1,
          tables.taggedTypes.localName(dp0.name),
          dp0.isIdentityCriteria)(r2o.omfStore)
      } yield r2o.copy(
        queue_elements = r2o.queue_elements - dp0.uuid,
        entityScalarDataProperties = r2o.entityScalarDataProperties + (dp0 -> dp1))
    case (prev, _) =>
      prev
  }

  private def convertEntityStructuredDataProperties(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables])(convertEntityStructuredDataProperties)

  private def convertEntityStructuredDataProperties(acc: ResolverResult, e: api.Extent)
  : ResolverResult
  = e.terminologyBoxOfTerminologyBoxStatement.foldLeft(acc) {
    case (prev, (dp0: api.EntityStructuredDataProperty, t0: api.TerminologyBox)) =>
      for {
        r2o <- prev
        t1 <- r2o.getTbox(t0)
        e1 <- r2o.entityLookup(dp0.domain)(e)
        r1 <- r2o.lookupStructure(dp0.range)(e)
        dp1 <- r2o.ops.addEntityStructuredDataProperty(
          t1, e1, r1,
          tables.taggedTypes.localName(dp0.name),
          dp0.isIdentityCriteria)(r2o.omfStore)
      } yield r2o.copy(
        queue_elements = r2o.queue_elements - dp0.uuid,
        entityStructuredDataProperties = r2o.entityStructuredDataProperties + (dp0 -> dp1))
    case (prev, _) =>
      prev
  }

  private def convertScalarDataProperties(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables])(convertScalarDataProperties)

  private def convertScalarDataProperties(acc: ResolverResult, e: api.Extent)
  : ResolverResult
  = e.terminologyBoxOfTerminologyBoxStatement.foldLeft(acc) {
    case (prev, (dp0: api.ScalarDataProperty, t0: api.TerminologyBox)) =>
      for {
        r2o <- prev
        t1 <- r2o.getTbox(t0)
        e1 <- r2o.lookupStructure(dp0.domain)(e)
        r1 <- r2o.lookupDataRange(dp0.range)(e)
        dp1 <- r2o.ops.addScalarDataProperty(
          t1, e1, r1,
          tables.taggedTypes.localName(dp0.name))(r2o.omfStore)
      } yield r2o.copy(
        queue_elements = r2o.queue_elements - dp0.uuid,
        scalarDataProperties = r2o.scalarDataProperties + (dp0 -> dp1))
    case (prev, _) =>
      prev
  }

  private def convertStructuredDataProperties(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables])(convertStructuredDataProperties)

  private def convertStructuredDataProperties(acc: ResolverResult, e: api.Extent)
  : ResolverResult
  = e.terminologyBoxOfTerminologyBoxStatement.foldLeft(acc) {
    case (prev, (dp0: api.StructuredDataProperty, t0: api.TerminologyBox)) =>
      for {
        r2o <- prev
        t1 <- r2o.getTbox(t0)
        e1 <- r2o.lookupStructure(dp0.domain)(e)
        r1 <- r2o.lookupStructure(dp0.range)(e)
        dp1 <- r2o.ops.addStructuredDataProperty(
          t1, e1, r1,
          tables.taggedTypes.localName(dp0.name))(r2o.omfStore)
      } yield r2o.copy(
        queue_elements = r2o.queue_elements - dp0.uuid,
        structuredDataProperties = r2o.structuredDataProperties + (dp0 -> dp1))
    case (prev, _) =>
      prev
  }

  // Entity Restrictions

  private def convertEntityRestrictionAxioms(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables]) {
    case (acc, ext) =>
      ext.terminologyBoxOfTerminologyBoxStatement.foldLeft(acc)(convertEntityRestrictionAxiom(ext))
  }

  private def convertEntityRestrictionAxiom(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (er0: api.EntityRestrictionAxiom, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        d1 <- r2o.entityLookup(er0.restrictedDomain)
        r1 <- r2o.entityLookup(er0.restrictedRange)
        rel1 <- r2o.restrictableRelationshipLookup(er0.restrictedRelationship)
        er1 <- er0 match {
          case _: api.EntityExistentialRestrictionAxiom =>
            r2o.ops.addEntityExistentialRestrictionAxiom(t1, d1, rel1, r1)(r2o.omfStore)
          case _: api.EntityUniversalRestrictionAxiom =>
            r2o.ops.addEntityUniversalRestrictionAxiom(t1, d1, rel1, r1)(r2o.omfStore)
        }
      } yield r2o.copy(termAxioms = r2o.termAxioms + (er0 -> er1))
    case (acc, _) =>
      acc
  }

  private def convertEntityDataPropertyRestrictionAxioms(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables]) {
    case (acc, ext) =>
      ext.terminologyBoxOfTerminologyBoxStatement.foldLeft(acc)(convertEntityDataPropertyRestrictionAxiom(ext))
  }

  private def convertEntityDataPropertyRestrictionAxiom(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (er0: api.EntityScalarDataPropertyRestrictionAxiom, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        d1 <- r2o.entityLookup(er0.restrictedEntity)
        sc1 <- r2o.lookupEntityScalarDataProperty(er0.scalarProperty)
        er1 <- er0 match {
          case ep0: api.EntityScalarDataPropertyExistentialRestrictionAxiom =>
            r2o.lookupDataRange(ep0.scalarRestriction).flatMap { r1 =>
              r2o.ops.addEntityScalarDataPropertyExistentialRestrictionAxiom(t1, d1, sc1, r1)(r2o.omfStore)
            }
          case ep0: api.EntityScalarDataPropertyUniversalRestrictionAxiom =>
            r2o.lookupDataRange(ep0.scalarRestriction).flatMap { r1 =>
              r2o.ops.addEntityScalarDataPropertyUniversalRestrictionAxiom(t1, d1, sc1, r1)(r2o.omfStore)
            }
          case ep0: api.EntityScalarDataPropertyParticularRestrictionAxiom =>
            for {
              vt1 <- ep0.valueType match {
                case Some(vt0) =>
                  r2o.lookupDataRange(vt0).map(Option.apply)
                case None =>
                  None.right
              }
              ep1 <- r2o.ops.addEntityScalarDataPropertyParticularRestrictionAxiom(t1, d1, sc1, ep0.literalValue, vt1)(r2o.omfStore)
            } yield ep1
        }
      } yield r2o.copy(termAxioms = r2o.termAxioms + (er0 -> er1))
    case (acc, (er0: api.EntityStructuredDataPropertyParticularRestrictionAxiom, t0)) =>
      for {
        r2o1 <- acc
        t1 <- r2o1.getTbox(t0)
        d1 <- r2o1.entityLookup(er0.restrictedEntity)
        sc1 <- r2o1.lookupEntityStructuredDataProperty(er0.structuredDataProperty)
        er1 <- r2o1.ops.addEntityStructuredDataPropertyParticularRestrictionAxiom(t1, d1, sc1)(r2o1.omfStore)
        erTuple = er0 -> er1
        r2o2 <- convertRestrictionStructuredDataPropertyContext(r2o1.copy(termAxioms = r2o1.termAxioms + erTuple), t1, Seq(erTuple))
      } yield r2o2
    case (acc, _) =>
      acc
  }

  @scala.annotation.tailrec
  protected final def convertRestrictionStructuredDataPropertyContext
  (r2o: OMLResolver2Ontology,
   t1: owlapi.types.terminologies.MutableTerminologyBox,
   cs: Seq[(api.RestrictionStructuredDataPropertyContext, owlapi.types.RestrictionStructuredDataPropertyContext)])
  (implicit ext: api.Extent)
  : Throwables \/ OMLResolver2Ontology
  = if (cs.isEmpty)
    r2o.right[Throwables]
  else {
    val (c0, c1) = cs.head
    val values = ext
      .restrictionStructuredDataPropertyContextOfRestrictionScalarDataPropertyValue
      .foldLeft(r2o.right[Throwables]) { case (acc, (vi, ci)) =>
        if (c0 != ci)
          acc
        else
          for {
            r2o1 <- acc
            ont_dp <- r2o1.lookupDataRelationshipToScalar(vi.scalarDataProperty)
            ont_vt <- vi.valueType match {
              case Some(dt) =>
                r2o1.lookupDataRange(dt).map(Option.apply)
              case None =>
                Option.empty.right[Throwables]
            }
            vj <- r2o1.ops.addRestrictionScalarDataPropertyValue(t1, c1, ont_dp, vi.scalarPropertyValue, ont_vt)(r2o1.omfStore)
          } yield r2o1.copy(restrictionScalarDataPropertyValues = r2o1.restrictionScalarDataPropertyValues + (vi -> vj))
      }

    val tuples = ext
      .restrictionStructuredDataPropertyContextOfRestrictionStructuredDataPropertyTuple
      .foldLeft(values.map(_ -> cs.tail)) { case (acc, (ti, ci)) =>
        if (c0 != ci)
          acc
        else
          for {
            r2o1_cs1 <- acc
            (r2o1, cs1) = r2o1_cs1
            ont_dp <- r2o1.lookupDataRelationshipToStructure(ti.structuredDataProperty)
            ont_ti <- r2o1.ops.addRestrictionStructuredDataPropertyTuple(t1, c1, ont_dp)(r2o1.omfStore)
            r2o2 = r2o1.copy(restrictionStructuredDataPropertyTuples = r2o1.restrictionStructuredDataPropertyTuples + (ti -> ont_ti))
            cs2 = cs1 :+ (ti -> ont_ti)
          } yield r2o2 -> cs2
      }


    tuples match {
      case \/-((r2o1, next)) =>
        convertRestrictionStructuredDataPropertyContext(r2o1, t1, next)
      case -\/(errors) =>
        -\/(errors)
    }
  }

  // Specializations

  private def convertSpecializationAxioms(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables]) {
    case (acc, ext) =>
      ext.terminologyBoxOfTerminologyBoxStatement.foldLeft(acc)(convertSpecializationAxiom(ext))
  }

  private def convertSpecializationAxiom(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (ax0: api.SpecializationAxiom, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        p1 <- r2o.entityLookup(ax0.parent())
        c1 <- r2o.entityLookup(ax0.child())
        ax1 <- (ax0, p1, c1) match {
          case (_: api.AspectSpecializationAxiom, sup: owlapi.types.terms.AspectKind, sub) =>
            r2o.ops.addAspectSpecializationAxiom(t1, sub, sup)(r2o.omfStore)
          case (_: api.ConceptSpecializationAxiom, sup: owlapi.types.terms.ConceptKind, sub: owlapi.types.terms.ConceptKind) =>
            r2o.ops.addConceptSpecializationAxiom(t1, sub, sup)(r2o.omfStore)
          case (_: api.ReifiedRelationshipSpecializationAxiom, sup: owlapi.types.terms.ConceptualRelationship, sub: owlapi.types.terms.ConceptualRelationship) =>
            r2o.ops.addReifiedRelationshipSpecializationAxiom(t1, sub, sup)(r2o.omfStore)
          case (_, sup, sub) =>
            Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
              s"convertSpecializationAxiom: Failed to resolve\n" +
                s"tbox: $t0\n" +
                s"sub: $sub\n" +
                s"sup: $sup\n" +
                s" for defining SpecializationAxiom: $ax0")).left
        }
      } yield r2o.copy(termAxioms = r2o.termAxioms + (ax0 -> ax1))
    case (acc, _) =>
      acc
  }

  // Sub{Data|Object}PropertyOfAxioms

  private def convertSubPropertyOfAxioms(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables]) {
    case (acc, ext) =>
      ext.terminologyBoxOfTerminologyBoxStatement.foldLeft(acc)(convertSubPropertyOfAxiom(ext))
  }

  private def convertSubPropertyOfAxiom(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (ax0: api.SubDataPropertyOfAxiom, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        sub1 <- r2o.lookupEntityScalarDataProperty(ax0.subProperty)
        sup1 <- r2o.lookupEntityScalarDataProperty(ax0.superProperty)
        ax1 <- r2o.ops.addSubDataPropertyOfAxiom(t1, sub1, sup1)(r2o.omfStore)
      } yield r2o.copy(termAxioms = r2o.termAxioms + (ax0 -> ax1))

    case (acc, (ax0: api.SubObjectPropertyOfAxiom, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        sub1 <- r2o.lookupUnreifiedRelationship(ax0.subProperty)
        sup1 <- r2o.lookupUnreifiedRelationship(ax0.superProperty)
        ax1 <- r2o.ops.addSubObjectPropertyOfAxiom(t1, sub1, sup1)(r2o.omfStore)
      } yield r2o.copy(termAxioms = r2o.termAxioms + (ax0 -> ax1))

    case (acc, _) =>
      acc
  }

  // Disjunctions

  private def convertRootConceptTaxonomyAxioms(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables]) {
    case (acc, ext) =>
      ext.terminologyBoxOfTerminologyBoxStatement.foldLeft(acc)(convertRootConceptTaxonomyAxiom(ext))
  }

  private def convertRootConceptTaxonomyAxiom(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (ax0: api.RootConceptTaxonomyAxiom, b0: api.Bundle)) =>
      for {
        r2o <- acc
        b1 <- r2o.getBundle(b0)
        r1 <- r2o.lookupConcept(ax0.root)
        ax1 <- r2o.ops.addRootConceptTaxonomyAxiom(b1, r1)(r2o.omfStore)
        disjuncts = disjunctsForConceptTreeDisjunction(r2o, ax0, ax1)
        next <- convertConceptTreeDisjunctions(r2o, b1, ax0, disjuncts)
      } yield next
    case (acc, _) =>
      acc
  }

  private def disjunctsForConceptTreeDisjunction
  (r2o: OMLResolver2Ontology, ctd0: api.ConceptTreeDisjunction, ctd1: owlapi.types.bundleStatements.ConceptTreeDisjunction)
  (implicit ext: api.Extent)
  : Seq[(api.DisjointUnionOfConceptsAxiom, owlapi.types.bundleStatements.ConceptTreeDisjunction)]
  = ext.disjunctions.getOrElse(ctd0, Set.empty).to[Seq].map(_ -> ctd1)

  @scala.annotation.tailrec
  private def convertConceptTreeDisjunctions
  (r2o: OMLResolver2Ontology,
   b1: owlapi.types.terminologies.MutableBundle,
   ax0: api.ConceptTreeDisjunction,
   disjuncts: Seq[(api.DisjointUnionOfConceptsAxiom, owlapi.types.bundleStatements.ConceptTreeDisjunction)])
  (implicit ext: api.Extent)
  : ResolverResult
  = if (disjuncts.isEmpty)
    r2o.right
  else {
    val (dis0, ctd1) = disjuncts.head
    dis0 match {
      case ax0: api.AnonymousConceptUnionAxiom =>
        r2o.ops.addAnonymousConceptTaxonomyAxiom(b1, ctd1, tables.taggedTypes.localName(ax0.name))(r2o.omfStore) match {
          case \/-(ax1) =>
            val upd = r2o.copy(disjointUnionOfConceptAxioms = r2o.disjointUnionOfConceptAxioms + (ax0 -> ax1))
            val children = disjunctsForConceptTreeDisjunction(upd, ax0, ax1)
            convertConceptTreeDisjunctions(upd, b1, ax0, children ++ disjuncts.tail)
          case -\/(errors) =>
            -\/(errors)
        }
      case ax0: api.SpecificDisjointConceptAxiom =>
        for {
          l1 <- r2o.lookupConcept(ax0.disjointLeaf)
          ax1 <- r2o.ops.addSpecificDisjointConceptAxiom(b1, ctd1, l1)(r2o.omfStore)
        } yield r2o.copy(disjointUnionOfConceptAxioms = r2o.disjointUnionOfConceptAxioms + (ax0 -> ax1))
    }
  }

  // ChainRules

  private def convertChainRules(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables]) {
    case (acc, ext) =>
      ext.terminologyBoxOfTerminologyBoxStatement.foldLeft(acc)(convertChainRule(ext))
  }

  private def convertChainRule(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (ax0: api.ChainRule, t0: api.TerminologyGraph)) =>
      for {
        r2o <- acc
        t1 <- r2o.getGbox(t0)
        h1 <- r2o.restrictableRelationshipLookup(ax0.head)
        ax1 <- r2o.ops.addChainRule(t1, h1, tables.taggedTypes.localName(ax0.name))(r2o.omfStore)
        _ <- ext.lookupFirstSegment(ax0) match {
          case Some(s0) =>
            convertRuleBodySegment(r2o.copy(chainRules = r2o.chainRules + (ax0 -> ax1)), ext, s0, t1, Some(ax1), None)
          case None =>
            Set[java.lang.Throwable](new IllegalArgumentException(
              s"OMLResolver2Ontology.convertChainRule: Missing first segment for rule: $ax0")).left
        }
        _ <- r2o.ops.makeChainRule(t1, ax1)(r2o.omfStore)
      } yield r2o
    case (acc, _) =>
      acc
  }

  @scala.annotation.tailrec
  private def convertRuleBodySegment
  (r2o: OMLResolver2Ontology,
   ext: api.Extent,
   s0: api.RuleBodySegment,
   t1: owlapi.types.terminologies.MutableTerminologyGraph,
   chainRule: Option[owlapi.types.terms.ChainRule],
   previousSegment: Option[owlapi.types.terms.RuleBodySegment])
  : ResolverResult
  = ext.predicate.get(s0) match {
    case Some(p0) =>
      r2o.ops.addRuleBodySegment(t1, chainRule, previousSegment)(r2o.omfStore) match {
        case \/-(s1) =>
          val conv
          : Throwables \/ (OMLResolver2Ontology, owlapi.types.terms.SegmentPredicate)
          = (p0.predicate,
           p0.reifiedRelationshipSource,
           p0.reifiedRelationshipInverseSource,
           p0.reifiedRelationshipTarget,
           p0.reifiedRelationshipInverseTarget,
           p0.unreifiedRelationshipInverse) match {

            case (Some(a0: api.AspectKind), _, _, _, _, _) =>
              r2o.lookupAspect(a0)(ext).flatMap { a1 =>
                r2o.ops.addSegmentPredicate(t1, s1, predicate=Some(a1))(r2o.omfStore).map { p1 =>
                  (r2o.copy(segmentPredicates = r2o.segmentPredicates + (p0 -> p1)), p1)
                }
              }

            case (Some(c0: api.ConceptKind), _, _, _, _, _) =>
              r2o.lookupConcept(c0)(ext).flatMap { c1 =>
                r2o.ops.addSegmentPredicate(t1, s1, predicate=Some(c1))(r2o.omfStore).map { p1 =>
                  (r2o.copy(segmentPredicates = r2o.segmentPredicates + (p0 -> p1)), p1)
                }
              }

            case (Some(rr0: api.ReifiedRelationshipRestriction), _, _, _, _, _) =>
              r2o.lookupReifiedRelationshipRestriction(rr0)(ext).flatMap { rr1 =>
                r2o.ops.addSegmentPredicate(t1, s1, predicate=Some(rr1))(r2o.omfStore).map { p1 =>
                  (r2o.copy(segmentPredicates = r2o.segmentPredicates + (p0 -> p1)), p1)
                }
              }

            case (Some(rr0: api.ReifiedRelationship), _, _, _, _, _) =>
              r2o.lookupReifiedRelationship(rr0)(ext).flatMap { rr1 =>
                r2o.ops.addSegmentPredicate(t1, s1, predicate=Some(rr1))(r2o.omfStore).map { p1 =>
                  (r2o.copy(segmentPredicates = r2o.segmentPredicates + (p0 -> p1)), p1)
                }
              }

            case (Some(fwd0: api.ForwardProperty), _, _, _, _, _) =>
              r2o.lookupReifiedRelationship(fwd0.reifiedRelationship)(ext).flatMap { rr1 =>
                r2o.ops.addSegmentPredicate(t1, s1, predicate=Some(rr1.forwardProperty))(r2o.omfStore).map { p1 =>
                  (r2o.copy(segmentPredicates = r2o.segmentPredicates + (p0 -> p1)), p1)
                }
              }

            case (Some(inv0: api.InverseProperty), _, _, _, _, _) =>
              r2o.lookupReifiedRelationship(inv0.reifiedRelationship)(ext).flatMap { rr1 =>
                rr1.inverseProperty match {
                  case Some(inv) =>
                    r2o.ops.addSegmentPredicate(t1, s1, predicate = Some(inv))(r2o.omfStore).map { p1 =>
                      (r2o.copy(segmentPredicates = r2o.segmentPredicates + (p0 -> p1)), p1)
                    }
                  case _ =>
                    -\/(Set[java.lang.Throwable](new IllegalArgumentException("")
                    ))
                }
              }

            case (Some(ur0: api.UnreifiedRelationship), _, _, _, _, _) =>
              r2o.lookupUnreifiedRelationship(ur0)(ext).flatMap { ur1 =>
                r2o.ops.addSegmentPredicate(t1, s1, predicate=Some(ur1))(r2o.omfStore).map { p1 =>
                  (r2o.copy(segmentPredicates = r2o.segmentPredicates + (p0 -> p1)), p1)
                }
              }

            case (None, Some(rr0), _, _, _, _) =>
              r2o.lookupReifiedRelationship(rr0)(ext).flatMap { rr1 =>
                r2o.ops.addSegmentPredicate(t1, s1, reifiedRelationshipSource=Some(rr1))(r2o.omfStore).map { p1 =>
                  (r2o.copy(segmentPredicates = r2o.segmentPredicates + (p0 -> p1)), p1)
                }
              }

            case (None, _, Some(rr0), _, _, _) =>
              r2o.lookupReifiedRelationship(rr0)(ext).flatMap { rr1 =>
                r2o.ops.addSegmentPredicate(t1, s1, reifiedRelationshipInverseSource=Some(rr1))(r2o.omfStore).map { p1 =>
                  (r2o.copy(segmentPredicates = r2o.segmentPredicates + (p0 -> p1)), p1)
                }
              }

            case (None, _, _,Some(rr0), _, _) =>
              r2o.lookupReifiedRelationship(rr0)(ext).flatMap { rr1 =>
                r2o.ops.addSegmentPredicate(t1, s1, reifiedRelationshipTarget=Some(rr1))(r2o.omfStore).map { p1 =>
                  (r2o.copy(segmentPredicates = r2o.segmentPredicates + (p0 -> p1)), p1)
                }
              }

            case (None, _, _, _, Some(rr0), _) =>
              r2o.lookupReifiedRelationship(rr0)(ext).flatMap { rr1 =>
                r2o.ops.addSegmentPredicate(t1, s1, reifiedRelationshipInverseTarget=Some(rr1))(r2o.omfStore).map { p1 =>
                  (r2o.copy(segmentPredicates = r2o.segmentPredicates + (p0 -> p1)), p1)
                }
              }

            case (None, _, _, _, _, Some(ur0)) =>
              r2o.lookupUnreifiedRelationship(ur0)(ext).flatMap { ur1 =>
                r2o.ops.addSegmentPredicate(t1, s1, unreifiedRelationshipInverse=Some(ur1))(r2o.omfStore).map { p1 =>
                  (r2o.copy(segmentPredicates = r2o.segmentPredicates + (p0 -> p1)), p1)
                }
              }

            case (pred, source, isource, target, itarget, uinv) =>
              Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
                s"OWLResolver2Ontology.convertRuleBodySegment(ruleBodySegment=$s0) unresolved:\n" +
                  pred.fold[String]("- no predicate"){ x =>
                    s"- predicate: ${x.term().abbrevIRI()(ext)}"
                  } +
                  source.fold[String]("- no reifiedRelationshipSource"){ x =>
                    s"- reifiedRelationshipSource: ${x.abbrevIRI()(ext)}"
                  } +
                  isource.fold[String]("- no reifiedRelationshipInverseSource"){ x =>
                    s"- reifiedRelationshipInverseSource: ${x.abbrevIRI()(ext)}"
                  } +
                  target.fold[String]("- no reifiedRelationshipTarget"){ x =>
                    s"- reifiedRelationshipTarget: ${x.abbrevIRI()(ext)}"
                  } +
                  itarget.fold[String]("- no reifiedRelationshipInverseTarget"){ x =>
                    s"- reifiedRelationshipInverseTarget: ${x.abbrevIRI()(ext)}"
                  } +
                  uinv.fold[String]("- no unreifiedRelationshipInverse"){ x =>
                    s"- unreifiedRelationshipInverse: ${x.abbrevIRI()(ext)}"
                  }
              )).left
          }
          conv match {
            case \/-((updated, p1)) =>
              val next = updated.copy(ruleBodySegments = updated.ruleBodySegments + (s0 -> s1))
              ext.lookupNextSegment(s0) match {
                case Some(n0) =>
                  convertRuleBodySegment(next, ext, n0, t1, None, Some(s1))
                case None =>
                  next.right
              }
            case -\/(errors) =>
              -\/(errors)
          }
        case -\/(errors) =>
          -\/(errors)
      }
    case None =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.convertRuleBodySegment: Missing predicate for segment: $s0")).left
  }

  // ConceptualEntityInstances

  private def convertConceptInstances(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables]) {
    case (acc, ext) =>
      ext.descriptionBoxOfConceptInstance.foldLeft(acc)(convertConceptInstances(ext))
  }

  private def convertConceptInstances(implicit ext: api.Extent)
  : (ResolverResult, (api.ConceptInstance, api.DescriptionBox)) => ResolverResult
  = {
    case (acc, (ax0: api.ConceptInstance, d0: api.DescriptionBox)) =>
      for {
        r2o <- acc
        d1 <- r2o.getDbox(d0)
        c1 <- r2o.lookupConcept(ax0.singletonConceptClassifier)
        ax1 <- r2o.ops.addConceptInstance(d1, c1, tables.taggedTypes.localName(ax0.name))(r2o.omfStore)
      } yield r2o.copy(conceptualEntitySingletonInstances = r2o.conceptualEntitySingletonInstances + (ax0 -> ax1))
  }

  private def convertReifiedRelationshipInstances(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables]) {
    case (acc, ext) =>
      ext.descriptionBoxOfReifiedRelationshipInstance.foldLeft(acc)(convertReifiedRelationshipInstances(ext))
  }

  private def convertReifiedRelationshipInstances(implicit ext: api.Extent)
  : (ResolverResult, (api.ReifiedRelationshipInstance, api.DescriptionBox)) => ResolverResult
  = {
    case (acc, (ax0: api.ReifiedRelationshipInstance, d0: api.DescriptionBox)) =>
      for {
        r2o <- acc
        d1 <- r2o.getDbox(d0)
        rr1 <- r2o.lookupConceptualRelationship(ax0.singletonConceptualRelationshipClassifier)
        ax1 <- r2o.ops.addReifiedRelationshipInstance(d1, rr1, tables.taggedTypes.localName(ax0.name))(r2o.omfStore)
      } yield r2o.copy(conceptualEntitySingletonInstances = r2o.conceptualEntitySingletonInstances + (ax0 -> ax1))
  }

  private def convertReifiedRelationshipInstanceDomains(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables]) {
    case (acc, ext) =>
      ext.descriptionBoxOfReifiedRelationshipInstanceDomain.foldLeft(acc)(convertReifiedRelationshipInstanceDomains(ext))
  }

  private def convertReifiedRelationshipInstanceDomains(implicit ext: api.Extent)
  : (ResolverResult, (api.ReifiedRelationshipInstanceDomain, api.DescriptionBox)) => ResolverResult
  = {
    case (acc, (ax0: api.ReifiedRelationshipInstanceDomain, d0: api.DescriptionBox)) =>
      for {
        r2o <- acc
        d1 <- r2o.getDbox(d0)
        s1 <- r2o.lookupConceptualEntityInstance(ax0.domain)
        rr1 <- r2o.lookupReifiedRelationshipInstance(ax0.reifiedRelationshipInstance)
        ax1 <- r2o.ops.addReifiedRelationshipInstanceDomain(d1, rr1, s1)(r2o.omfStore)
      } yield r2o.copy(reifiedRelationshipInstanceDomains = r2o.reifiedRelationshipInstanceDomains + (ax0 -> ax1))
  }

  private def convertReifiedRelationshipInstanceRanges(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables]) {
    case (acc, ext) =>
      ext.descriptionBoxOfReifiedRelationshipInstanceRange.foldLeft(acc)(convertReifiedRelationshipInstanceRanges(ext))
  }

  private def convertReifiedRelationshipInstanceRanges(implicit ext: api.Extent)
  : (ResolverResult, (api.ReifiedRelationshipInstanceRange, api.DescriptionBox)) => ResolverResult
  = {
    case (acc, (ax0: api.ReifiedRelationshipInstanceRange, d0: api.DescriptionBox)) =>
      for {
        r2o <- acc
        d1 <- r2o.getDbox(d0)
        s1 <- r2o.lookupConceptualEntityInstance(ax0.range)
        rr1 <- r2o.lookupReifiedRelationshipInstance(ax0.reifiedRelationshipInstance)
        ax1 <- r2o.ops.addReifiedRelationshipInstanceRange(d1, rr1, s1)(r2o.omfStore)
      } yield r2o.copy(reifiedRelationshipInstanceRanges = r2o.reifiedRelationshipInstanceRanges + (ax0 -> ax1))
  }

  private def convertUnreifiedReifiedRelationshipInstanceTuples(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables]) {
    case (acc, ext) =>
      ext.descriptionBoxOfUnreifiedRelationshipInstanceTuple.foldLeft(acc)(convertUnreifiedReifiedRelationshipInstanceTuples(ext))
  }

  private def convertUnreifiedReifiedRelationshipInstanceTuples(implicit ext: api.Extent)
  : (ResolverResult, (api.UnreifiedRelationshipInstanceTuple, api.DescriptionBox)) => ResolverResult
  = {
    case (acc, (ax0: api.UnreifiedRelationshipInstanceTuple, d0: api.DescriptionBox)) =>
      for {
        r2o <- acc
        d1 <- r2o.getDbox(d0)
        s1 <- r2o.lookupConceptualEntityInstance(ax0.domain)
        t1 <- r2o.lookupConceptualEntityInstance(ax0.range)
        ur1 <- r2o.lookupUnreifiedRelationship(ax0.unreifiedRelationship)
        ax1 <- r2o.ops.addUnreifiedRelationshipInstanceTuple(d1, ur1, s1, t1)(r2o.omfStore)
      } yield r2o.copy(unreifiedRelationshipInstanceTuples = r2o.unreifiedRelationshipInstanceTuples + (ax0 -> ax1))
  }

  // Data Property Values of ConceptualEntityInstances

  private def convertSingletonInstanceScalarDataPropertyValues(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables]) {
    case (acc, ext) =>
      ext.descriptionBoxOfSingletonInstanceScalarDataPropertyValue.foldLeft(acc)(convertSingletonInstanceScalarDataPropertyValues(ext))
  }

  private def convertSingletonInstanceScalarDataPropertyValues(implicit ext: api.Extent)
  : (ResolverResult, (api.SingletonInstanceScalarDataPropertyValue, api.DescriptionBox)) => ResolverResult
  = {
    case (acc, (ax0: api.SingletonInstanceScalarDataPropertyValue, d0: api.DescriptionBox)) =>
      for {
        r2o <- acc
        d1 <- r2o.getDbox(d0)
        s1 <- r2o.lookupConceptualEntityInstance(ax0.singletonInstance)
        dp1 <- r2o.lookupEntityScalarDataProperty(ax0.scalarDataProperty)
        vt1 <- ax0.valueType match {
          case Some(vt0) =>
            r2o.lookupDataRange(vt0).map(Option.apply)
          case None =>
            None.right
        }
        ax1 <- r2o.ops.addSingletonInstanceScalarDataPropertyValue(d1, s1, dp1, ax0.scalarPropertyValue, vt1)(r2o.omfStore)
      } yield r2o.copy(singletonScalarDataPropertyValues = r2o.singletonScalarDataPropertyValues + (ax0 -> ax1))
  }

  private def convertSingletonInstanceStructuredDataPropertyValues(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables]) {
    case (acc, ext) =>
      ext.descriptionBoxOfSingletonInstanceStructuredDataPropertyValue.foldLeft(acc)(convertSingletonInstanceStructuredDataPropertyValues(ext))
  }

  private def convertSingletonInstanceStructuredDataPropertyValues(implicit ext: api.Extent)
  : (ResolverResult, (api.SingletonInstanceStructuredDataPropertyValue, api.DescriptionBox)) => ResolverResult
  = {
    case (acc, (ax0: api.SingletonInstanceStructuredDataPropertyValue, d0: api.DescriptionBox)) =>
      for {
        r2o <- acc
        d1 <- r2o.getDbox(d0)
        s1 <- r2o.lookupConceptualEntityInstance(ax0.singletonInstance)
        dp1 <- r2o.lookupEntityStructuredDataProperty(ax0.structuredDataProperty)
        ax1 <- r2o.ops.addSingletonInstanceStructuredDataPropertyValue(d1, s1, dp1)(r2o.omfStore)
      } yield r2o.copy(singletonStructuredDataPropertyValues = r2o.singletonStructuredDataPropertyValues + (ax0 -> ax1))
  }

  private def convertStructuredPropertyTuples(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables]) {
    case (acc, ext) =>
      ext.structuredPropertyTuples.foldLeft(acc)(convertStructuredPropertyTuples(ext))
  }

  private def convertStructuredPropertyTuples(implicit ext: api.Extent)
  : (ResolverResult, (api.SingletonInstanceStructuredDataPropertyContext, Set[api.StructuredDataPropertyTuple])) => ResolverResult
  = {
    case (acc, (c0, ts0)) =>
      for {
        r2o1 <- acc
        c1 <- r2o1.lookupSingletonInstanceStructuredDataPropertyContext(c0)
        r2oN <- ts0.foldLeft(acc)(convertStructuredDataPropertyValue(c0, c1)(ext))
      } yield r2oN
  }

  private def convertStructuredDataPropertyValue
  (c0: api.SingletonInstanceStructuredDataPropertyContext,
   c1: owlapi.descriptions.SingletonInstanceStructuredDataPropertyContext)
  (implicit ext: api.Extent)
  : (ResolverResult, api.StructuredDataPropertyTuple) => ResolverResult
  = {
    case (acc, t0) =>
      // TODO
      acc
  }

  private def convertScalarDataPropertyValues(r2o: OMLResolver2Ontology)
  : ResolverResult
  = r2o.extents.foldLeft(r2o.right[Throwables]) {
    case (acc, ext) =>
      ext.scalarDataPropertyValues.foldLeft(acc)(convertScalarDataPropertyValues(ext))
  }

  private def convertScalarDataPropertyValues(implicit ext: api.Extent)
  : (ResolverResult, (api.SingletonInstanceStructuredDataPropertyContext, Set[api.ScalarDataPropertyValue])) => ResolverResult
  = {
    case (acc, (c0, vs0)) =>
      for {
        r2o1 <- acc
        c1 <- r2o1.lookupSingletonInstanceStructuredDataPropertyContext(c0)
        r2oN <- vs0.foldLeft(acc)(convertScalarDataPropertyValue(c0, c1)(ext))
      } yield r2oN
  }

  private def convertScalarDataPropertyValue
  (c0: api.SingletonInstanceStructuredDataPropertyContext,
   c1: owlapi.descriptions.SingletonInstanceStructuredDataPropertyContext)
  (implicit ext: api.Extent)
  : (ResolverResult, api.ScalarDataPropertyValue) => ResolverResult
  = {
    case (acc, v0) =>
      // TODO
      acc
  }
}

case class OMLResolver2Ontology
(om: OntologyMapping,
 omfStore: owlapi.OWLAPIOMFGraphStore,
 extents: Set[api.Extent] = Set.empty,

 queue_edges: Map[api.taggedTypes.ModuleEdgeUUID, (api.Extent,api.ModuleEdge)] = Map.empty,
 queue_elements: Map[api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement)] = Map.empty,

 // Modules
 modules
 : Set[api.Module]
 = Set.empty,

 gs
 : Map[api.TerminologyGraph, owlapiterminologies.MutableTerminologyGraph]
 = Map.empty,

 bs
 : Map[api.Bundle, owlapiterminologies.MutableBundle]
 = Map.empty,

 ds
 : Map[api.DescriptionBox, owlapidescriptions.MutableDescriptionBox]
 = Map.empty,

 // AnnotationProperties
 aps
 : Map[api.AnnotationProperty, tables.AnnotationProperty]
 = Map.empty,

 // ModuleEdges
 edges
 : Map[api.ModuleEdge, owlapi.common.ModuleEdge]
 = Map.empty,

 aspects
 : Map[api.AspectKind, owlapi.types.terms.AspectKind]
 = Map.empty,

 concepts
 : Map[api.ConceptKind, owlapi.types.terms.ConceptKind]
 = Map.empty,

 reifiedRelationshipRestrictions
 : Map[api.ReifiedRelationshipRestriction, owlapi.types.terms.ReifiedRelationshipRestriction]
 = Map.empty,

 cardinalityRestrictedReifiedRelationships
 : Map[api.CardinalityRestrictedReifiedRelationship, owlapi.types.terms.CardinalityRestrictedReifiedRelationship]
 = Map.empty,

 reifiedRelationships
 : Map[api.ReifiedRelationship, owlapi.types.terms.ReifiedRelationship]
 = Map.empty,

 forwardProperties
 : Map[api.ForwardProperty, owlapi.types.terms.ForwardProperty]
 = Map.empty,

 inverseProperties
 : Map[api.InverseProperty, owlapi.types.terms.InverseProperty]
 = Map.empty,

 unreifiedRelationships
 : Map[api.UnreifiedRelationship, owlapi.types.terms.UnreifiedRelationship]
 = Map.empty,

 dataRanges
 : Map[api.DataRange, owlapi.types.terms.DataRange]
 = Map.empty,

 structures
 : Map[api.Structure, owlapi.types.terms.Structure]
 = Map.empty,

 entityScalarDataProperties
 : Map[api.EntityScalarDataProperty, owlapi.types.terms.EntityScalarDataProperty]
 = Map.empty,

 entityStructuredDataProperties
 : Map[api.EntityStructuredDataProperty, owlapi.types.terms.EntityStructuredDataProperty]
 = Map.empty,

 scalarDataProperties
 : Map[api.ScalarDataProperty, owlapi.types.terms.ScalarDataProperty]
 = Map.empty,

 structuredDataProperties
 : Map[api.StructuredDataProperty, owlapi.types.terms.StructuredDataProperty]
 = Map.empty,

 termAxioms
 : Map[api.TermAxiom, owlapi.types.termAxioms.TermAxiom]
 = Map.empty,

 conceptTreeDisjunctions
 : Map[api.ConceptTreeDisjunction, owlapi.types.bundleStatements.ConceptTreeDisjunction]
 = Map.empty,

 disjointUnionOfConceptAxioms
 : Map[api.DisjointUnionOfConceptsAxiom, owlapi.types.bundleStatements.DisjointUnionOfConceptsAxiom]
 = Map.empty,

 chainRules
 : Map[api.ChainRule, owlapi.types.terms.ChainRule]
 = Map.empty,

 ruleBodySegments
 : Map[api.RuleBodySegment, owlapi.types.terms.RuleBodySegment]
 = Map.empty,

 segmentPredicates
 : Map[api.SegmentPredicate, owlapi.types.terms.SegmentPredicate]
 = Map.empty,

 conceptualEntitySingletonInstances
 : Map[api.ConceptualEntitySingletonInstance, owlapi.descriptions.ConceptualEntitySingletonInstance]
 = Map.empty,

 reifiedRelationshipInstanceDomains
 : Map[api.ReifiedRelationshipInstanceDomain, owlapi.descriptions.ReifiedRelationshipInstanceDomain]
 = Map.empty,

 reifiedRelationshipInstanceRanges
 : Map[api.ReifiedRelationshipInstanceRange, owlapi.descriptions.ReifiedRelationshipInstanceRange]
 = Map.empty,

 unreifiedRelationshipInstanceTuples
 : Map[api.UnreifiedRelationshipInstanceTuple, owlapi.descriptions.UnreifiedRelationshipInstanceTuple]
 = Map.empty,

 singletonScalarDataPropertyValues
 : Map[api.SingletonInstanceScalarDataPropertyValue, owlapi.descriptions.SingletonInstanceScalarDataPropertyValue]
 = Map.empty,

 singletonStructuredDataPropertyValues
 : Map[api.SingletonInstanceStructuredDataPropertyValue, owlapi.descriptions.SingletonInstanceStructuredDataPropertyValue]
 = Map.empty,

 structuredPropertyTuples
 : Map[api.StructuredDataPropertyTuple, owlapi.descriptions.StructuredDataPropertyTuple]
 = Map.empty,

 scalarDataPropertyValues
 : Map[api.ScalarDataPropertyValue, owlapi.descriptions.ScalarDataPropertyValue]
 = Map.empty,

 singletonInstanceStructuredDataPropertyContexts
 : Map[api.SingletonInstanceStructuredDataPropertyContext, owlapi.descriptions.SingletonInstanceStructuredDataPropertyContext]
 = Map.empty,

 restrictionStructuredDataPropertyTuples
 : Map[api.RestrictionStructuredDataPropertyTuple, owlapi.types.RestrictionStructuredDataPropertyTuple]
 = Map.empty,

 restrictionScalarDataPropertyValues
 : Map[api.RestrictionScalarDataPropertyValue, owlapi.types.RestrictionScalarDataPropertyValue]
 = Map.empty


) {
  def isResolved: Boolean
  = queue_edges.isEmpty &&
    queue_elements.isEmpty

  def withBuiltIn
  (m0: api.Module, m1: OMLResolver2Ontology.MutableTboxOrDbox)
  (implicit ext: api.Extent)
  : core.OMFError.Throwables \/ OMLResolver2Ontology
  = (m0, m1) match {
    case (g0: api.TerminologyGraph, -\/(g1: owlapiterminologies.TerminologyGraph)) =>

      val r2oWithAPS
      : core.OMFError.Throwables \/ OMLResolver2Ontology
      = ext
        .annotationProperties
        .foldLeft[core.OMFError.Throwables \/ OMLResolver2Ontology](this.right) {
        case (acc1, (t0: api.TerminologyBox, aps0)) =>
          for {
            r2o <- acc1
            t1 <- r2o.getTbox(t0)
            next <- aps0.foldLeft(acc1) { case (acc2, ap0) =>
              for {
                prev <- acc2
                apUUID = ap0.uuid
                aps1 = t1.annotationProperties()
                f1 = aps1.find(_.uuid == apUUID.toString)
                updated <- f1 match {
                    case Some(ap1) =>
                      prev.copy(aps = prev.aps + (ap0 -> ap1)).right
                    case _ =>
                      Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
                        s"withBuiltIn: Failed to lookup annotation property: $ap0 from ${g1.iri}")).left
                  }
              } yield updated
            }
          } yield next
        case (acc, _) =>
          acc
      }

      g0
        .moduleElements()
        .selectByKindOf { case d0: api.DataRange => d0 }
        .foldLeft[core.OMFError.Throwables \/ OMLResolver2Ontology](r2oWithAPS) {
        case (\/-(r2o), d0) =>
          g1.lookupTerm(d0.iri().map(IRI.create), recursively=false)(this.omfStore) match {
            case Some(d1: owlapi.types.terms.DataRange) =>
              r2o.copy(dataRanges = r2o.dataRanges + (d0 -> d1)).right
            case _ =>
              Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
                s"withBuiltIn: Failed to lookup data range: ${d0.iri()} from ${g1.iri}")).left
          }
        case (acc, _) =>
          acc
      }
    case _ =>
      Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
        s"withBuiltIn: Invalid modules: m0=$m0, m1=$m1")).left
  }

  def getModule(m0: api.Module):  core.OMFError.Throwables \/ owlapi.common.MutableModule
  = m0 match {
    case t0: api.TerminologyBox =>
      getTbox(t0)
    case d0: api.DescriptionBox =>
      getDbox(d0)
  }

  def getTbox(m0: api.TerminologyBox): core.OMFError.Throwables \/ owlapiterminologies.MutableTerminologyBox
  = (m0 match {
    case mt: api.TerminologyGraph =>
      gs.get(mt)
    case mb: api.Bundle =>
      bs.get(mb)
  }) match {
    case Some(m1) =>
      m1.right
    case None =>
      Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
        s"OWLResolver2Ontology.getTbox(${m0.kind} ${m0.iri}) not found."
      )).left
  }

  def getGbox(m0: api.TerminologyBox): core.OMFError.Throwables \/ owlapiterminologies.MutableTerminologyGraph
  = (m0 match {
    case mt: api.TerminologyGraph =>
      gs.get(mt)
    case _ =>
      None
  }) match {
    case Some(m1) =>
      m1.right
    case None =>
      Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
        s"OWLResolver2Ontology.getGbox(${m0.kind} ${m0.iri}) not found."
      )).left
  }

  private val iri2mutableTbox
  : Map[tables.taggedTypes.IRI, owlapiterminologies.MutableTerminologyBox]
  = gs.map { case (g, mg) => g.iri -> mg } ++ bs.map { case (b, mb) => b.iri -> mb }

  def getTboxByIRI(iri: tables.taggedTypes.IRI): core.OMFError.Throwables \/ owlapiterminologies.MutableTerminologyBox
  = iri2mutableTbox.get(iri) match {
    case Some(mtbox) =>
      mtbox.right[core.OMFError.Throwables]
    case None =>
      Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
        s"OMLResolver2Ontology.getTbox(iri=$iri) Not found!"
      )).left
  }

  def lookupTbox(t0: api.TerminologyBox): Option[owlapiterminologies.MutableTerminologyBox]
  = t0 match {
    case mt: api.TerminologyGraph =>
      gs.get(mt)
    case mb: api.Bundle =>
      bs.get(mb)
  }

  def lookupGbox(t0: api.TerminologyBox): Option[owlapiterminologies.MutableTerminologyGraph]
  = t0 match {
    case mt: api.TerminologyGraph =>
      gs.get(mt)
    case _ =>
      None
  }

  def getBundle(b0: api.Bundle): core.OMFError.Throwables \/ owlapiterminologies.MutableBundle
  = bs.get(b0) match {
    case Some(b1) =>
      b1.right
    case None =>
      Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
        s"OWLResolver2Ontology.getBundle(${b0.kind} ${b0.iri}) not found."
      )).left
  }

  def getDbox(m0: api.DescriptionBox): core.OMFError.Throwables \/ owlapidescriptions.MutableDescriptionBox
  = this.ds.get(m0) match {
    case Some(m1) =>
      m1.right
    case None =>
      Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
        s"OWLResolver2Ontology.getDbox(${m0.kind} ${m0.iri}) not found."
      )).left
  }

  def getDboxByIRI(iri: tables.taggedTypes.IRI): core.OMFError.Throwables \/ owlapidescriptions.MutableDescriptionBox
  = ds.values
    .find { dbox =>
      dbox.iri.toString == iri
    } match {
    case Some(mdbox) =>
      mdbox.right[core.OMFError.Throwables]
    case None =>
      Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
        s"OMLResolver2Ontology.lookupDbox(iri=$iri) Not found!"
      )).left
  }

  def lookupAspect(a0: api.AspectKind)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.AspectKind
  = aspects.get(a0) match {
    case Some(a1) =>
      a1.right
    case None =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookupAspect(a=${a0.iri()}) failed")).left
  }

  def lookupConcept(c0: api.ConceptKind)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.ConceptKind
  = concepts.get(c0) match {
    case Some(c1) =>
      c1.right
    case None =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookupConcept(c=${c0.iri()}) failed")).left
  }

  def lookupConceptualRelationship(cr0: api.ConceptualRelationship)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.ConceptualRelationship
  = cr0 match {
    case crr0: api.CardinalityRestrictedReifiedRelationship =>
      cardinalityRestrictedReifiedRelationships.get(crr0) match {
        case Some(crr1) =>
          crr1.right
        case None =>
          Set[java.lang.Throwable](new IllegalArgumentException(
            s"OMLResolver2Ontology.lookupConceptualRelationship(crr=${crr0.iri()}) failed")).left
      }
    case rr0: api.ReifiedRelationship =>
      reifiedRelationships.get(rr0) match {
      case Some(rr1) =>
        rr1.right
      case None =>
        Set[java.lang.Throwable](new IllegalArgumentException(
          s"OMLResolver2Ontology.lookupConceptualRelationship(rr=${rr0.iri()}) failed")).left
    }
    case rs0: api.ReifiedRelationshipRestriction =>
      reifiedRelationshipRestrictions.get(rs0) match {
        case Some(rs1) =>
          rs1.right
        case None =>
          Set[java.lang.Throwable](new IllegalArgumentException(
            s"OMLResolver2Ontology.lookupConceptualRelationship(rr=${rs0.iri()}) failed")).left
      }
  }

  def lookupReifiedRelationshipRestriction(rr0: api.ReifiedRelationshipRestriction)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.ReifiedRelationshipRestriction
  = reifiedRelationshipRestrictions.get(rr0) match {
    case Some(rr1) =>
      rr1.right
    case None =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookupReifiedRelationshipRestriction(rr=${rr0.iri()}) failed")).left
  }

  def lookupReifiedRelationship(rr0: api.ReifiedRelationship)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.ReifiedRelationship
  = reifiedRelationships.get(rr0) match {
    case Some(rr1) =>
      rr1.right
    case None =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookupReifiedRelationship(rr=${rr0.iri()}) failed")).left
  }

  def lookupUnreifiedRelationship(ur0: api.UnreifiedRelationship)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.UnreifiedRelationship
  = unreifiedRelationships.get(ur0) match {
    case Some(ur1) =>
      ur1.right
    case None =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookupUnreifiedRelationship(ur=${ur0.iri()}) failed")).left
  }

  def lookupDataRange(d0: api.DataRange)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.DataRange
  = dataRanges.get(d0) match {
    case Some(d1) =>
      d1.right
    case None =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookupDataRange(d=${d0.iri()}) failed")).left
  }

  def lookupStructure(s0: api.Structure)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.Structure
  = structures.get(s0) match {
    case Some(s1) =>
      s1.right
    case None =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookupStructure(s=${s0.iri()}) failed")).left
  }

  def lookupScalarOneOfRestriction(d0: api.ScalarOneOfRestriction)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.ScalarOneOfRestriction
  = dataRanges.get(d0) match {
    case Some(d1: owlapi.types.terms.ScalarOneOfRestriction) =>
      d1.right
    case _ =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookupScalarOneOfRestriction(d=${d0.iri()}) failed")).left
  }

  def lookupEntity(e0: api.Entity): Option[owlapi.types.terms.Entity]
  = e0 match {
    case a0: api.AspectKind =>
      aspects.get(a0)
    case c0: api.ConceptKind =>
      concepts.get(c0)
    case crr0: api.CardinalityRestrictedReifiedRelationship =>
      cardinalityRestrictedReifiedRelationships.get(crr0)
    case rs0: api.ReifiedRelationshipRestriction =>
      reifiedRelationshipRestrictions.get(rs0)
    case rr0: api.ReifiedRelationship =>
      reifiedRelationships.get(rr0)
  }

  def entityLookup(e0: api.Entity)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.Entity
  = lookupEntity(e0) match {
    case Some(e1) =>
      e1.right
    case None =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.entityLookup(e=${e0.iri()}) failed")).left
  }

  def restrictableRelationshipLookup(e0: api.RestrictableRelationship)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.RestrictableRelationship
  = e0 match {
    case u0: api.UnreifiedRelationship =>
      unreifiedRelationships.get(u0) match {
        case Some(u1) =>
          u1.right
        case _ =>
          Set[java.lang.Throwable](new IllegalArgumentException(
            s"OMLResolver2Ontology.restrictableRelationshipLookup(unreifiedRelationship=${u0.iri()}) failed")).left
      }
    case f0: api.ForwardProperty =>
      forwardProperties.get(f0) match {
        case Some(f1) =>
          f1.right
        case _ =>
          Set[java.lang.Throwable](new IllegalArgumentException(
            s"OMLResolver2Ontology.restrictableRelationshipLookup(forwardProperty=${f0.iri()}) failed")).left
      }
    case f0: api.InverseProperty =>
      inverseProperties.get(f0) match {
        case Some(f1) =>
          f1.right
        case _ =>
          Set[java.lang.Throwable](new IllegalArgumentException(
            s"OMLResolver2Ontology.restrictableRelationshipLookup(inverseProperty=${f0.iri()}) failed")).left
      }
  }

  def entityRelationshipLookup(e0: api.EntityRelationship)(implicit ext: api.Extent)
  : core.OMFError.Throwables \/ owlapi.types.terms.EntityRelationship
  = e0 match {
    case crr0: api.CardinalityRestrictedReifiedRelationship =>
      lookup(crr0, cardinalityRestrictedReifiedRelationships)
    case rr0: api.ReifiedRelationship =>
      lookup(rr0, reifiedRelationships)
    case ur0: api.UnreifiedRelationship =>
      lookup(ur0, unreifiedRelationships)
  }

  def lookupEntityScalarDataProperty(d0: api.EntityScalarDataProperty)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.EntityScalarDataProperty
  = entityScalarDataProperties.get(d0) match {
    case Some(d1: owlapi.types.terms.EntityScalarDataProperty) =>
      d1.right
    case _ =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookupEntityScalarDataProperty(d=${d0.iri()}) failed")).left
  }

  def lookupEntityStructuredDataProperty(d0: api.DataRelationshipToStructure)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.EntityStructuredDataProperty
  = d0 match {
    case es0: api.EntityStructuredDataProperty =>
      entityStructuredDataProperties.get(es0) match {
        case Some(es1: owlapi.types.terms.EntityStructuredDataProperty) =>
          es1.right
        case _ =>
          Set[java.lang.Throwable](new IllegalArgumentException(
            s"OMLResolver2Ontology.lookupEntityStructuredDataProperty(d=${d0.iri()}) failed")).left
      }
    case _ =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookupEntityStructuredDataProperty(d=${d0.iri()}) property is not an EntityStructuredDataProperty")).left
  }

  def lookupDataRelationshipToStructure(d0: api.DataRelationshipToStructure)(implicit ext: api.Extent)
  : core.OMFError.Throwables \/ owlapi.types.terms.DataRelationshipToStructure
  = d0 match {
    case es0: api.EntityStructuredDataProperty =>
      entityStructuredDataProperties.get(es0) match {
        case Some(es1: owlapi.types.terms.EntityStructuredDataProperty) =>
          es1.right
        case _ =>
          Set[java.lang.Throwable](new IllegalArgumentException(
            s"OMLResolver2Ontology.lookupDataRelationshipToStructure(d=${d0.iri()}) failed")).left
      }
    case es0: api.StructuredDataProperty =>
      structuredDataProperties.get(es0) match {
        case Some(es1: owlapi.types.terms.StructuredDataProperty) =>
          es1.right
        case _ =>
          Set[java.lang.Throwable](new IllegalArgumentException(
            s"OMLResolver2Ontology.lookupDataRelationshipToStructure(d=${d0.iri()}) failed")).left
      }
  }

  def lookupDataRelationshipToScalar(d0: api.DataRelationshipToScalar)(implicit ext: api.Extent)
  : core.OMFError.Throwables \/ owlapi.types.terms.DataRelationshipToScalar
  = d0 match {
    case es0: api.EntityScalarDataProperty =>
      entityScalarDataProperties.get(es0) match {
        case Some(es1: owlapi.types.terms.EntityScalarDataProperty) =>
          es1.right
        case _ =>
          Set[java.lang.Throwable](new IllegalArgumentException(
            s"OMLResolver2Ontology.lookupDataRelationshipToScalar(d=${d0.iri()}) failed")).left
      }
    case es0: api.ScalarDataProperty =>
      scalarDataProperties.get(es0) match {
        case Some(es1: owlapi.types.terms.ScalarDataProperty) =>
          es1.right
        case _ =>
          Set[java.lang.Throwable](new IllegalArgumentException(
            s"OMLResolver2Ontology.lookupDataRelationshipToScalar(d=${d0.iri()}) failed")).left
      }
  }

  def lookupScalarDataProperty(d0: api.ScalarDataProperty)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.ScalarDataProperty
  = scalarDataProperties.get(d0) match {
    case Some(d1: owlapi.types.terms.ScalarDataProperty) =>
      d1.right
    case _ =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookupScalarDataProperty(d=${d0.iri()}) failed")).left
  }

  def lookupStructuredDataProperty(d0: api.StructuredDataProperty)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.StructuredDataProperty
  = structuredDataProperties.get(d0) match {
    case Some(d1: owlapi.types.terms.StructuredDataProperty) =>
      d1.right
    case _ =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookupStructuredDataProperty(d=${d0.iri()}) failed")).left
  }

  def lookupConceptualEntityInstance(s0: api.ConceptualEntitySingletonInstance)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.descriptions.ConceptualEntitySingletonInstance
  = conceptualEntitySingletonInstances.get(s0) match {
    case Some(s1) =>
      s1.right
    case None =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookupConceptualEntityInstance(s=${s0.iri()}) failed")).left
  }

  def lookupReifiedRelationshipInstance(rr0: api.ReifiedRelationshipInstance)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.descriptions.ReifiedRelationshipInstance
  = conceptualEntitySingletonInstances.get(rr0) match {
    case Some(rr1: owlapi.descriptions.ReifiedRelationshipInstance) =>
      rr1.right
    case _ =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookupReifiedRelationshipInstance(rr=${rr0.iri()}) failed")).left
  }

  def lookupSingletonInstanceStructuredDataPropertyContext
  (t0: api.SingletonInstanceStructuredDataPropertyContext)
  (implicit ext: api.Extent)
  : core.OMFError.Throwables \/ owlapi.descriptions.SingletonInstanceStructuredDataPropertyContext
  = singletonInstanceStructuredDataPropertyContexts.get(t0) match {
    case Some(t1: owlapi.descriptions.SingletonInstanceStructuredDataPropertyContext) =>
      t1.right
    case _ =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookupSingletonInstanceStructuredDataPropertyContext(rr=${t0.uuid}) failed")).left
  }

  def lookupMap[U <: api.LogicalElement, V <: api.LogicalElement]
  (u: U, uv: Map[U, V])
  (implicit ext: api.Extent)
  : core.OMFError.Throwables \/ V
  = uv.get(u) match {
    case Some(u) =>
      u.right[core.OMFError.Throwables]
    case _ =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookupMap failed for: $u")).left
  }

  def lookup[U <: api.LogicalElement, V <: owlapi.common.LogicalElement]
  (u: U, uv: Map[U, V])
  (implicit ext: api.Extent)
  : core.OMFError.Throwables \/ V
  = uv.get(u) match {
    case Some(u) =>
      u.right[core.OMFError.Throwables]
    case _ =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookup failed for: $u")).left
  }

  def lookupModuleElement(me: api.ModuleElement)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.common.ModuleElement
  = me match {
    case x: api.AspectKind =>
      lookupAspect(x)
    case x: api.ConceptKind =>
      lookupConcept(x)
    case x: api.ReifiedRelationshipRestriction =>
      lookupReifiedRelationshipRestriction(x)
    case x: api.ReifiedRelationship =>
      lookupReifiedRelationship(x)
    case x: api.UnreifiedRelationship =>
      lookupUnreifiedRelationship(x)
    case x: api.DataRange =>
      lookupDataRange(x)
    case x: api.Structure =>
      lookupStructure(x)
    case x: api.TermAxiom =>
      lookup(x, termAxioms)
    case x: api.DataRelationshipToStructure =>
      lookupDataRelationshipToStructure(x)
    case x: api.DataRelationshipToScalar =>
      lookupDataRelationshipToScalar(x)
    case x: api.ConceptualEntitySingletonInstance =>
      lookupConceptualEntityInstance(x)
    case x: api.ReifiedRelationshipInstanceDomain =>
      lookup(x, reifiedRelationshipInstanceDomains)
    case x: api.ReifiedRelationshipInstanceRange =>
      lookup(x, reifiedRelationshipInstanceRanges)
    case x: api.UnreifiedRelationshipInstanceTuple =>
      lookup(x, unreifiedRelationshipInstanceTuples)
    case x: api.SingletonInstanceScalarDataPropertyValue =>
      lookup(x, singletonScalarDataPropertyValues)
    case x: api.SingletonInstanceStructuredDataPropertyValue =>
      lookup(x, singletonStructuredDataPropertyValues)
    case x: api.RestrictionStructuredDataPropertyTuple =>
      lookup(x, restrictionStructuredDataPropertyTuples)
    case x: api.ChainRule =>
      lookup(x, chainRules)
  }

  def lookupOtherElement(e: api.LogicalElement)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.common.LogicalElement
  = e match {
    case x: api.ConceptTreeDisjunction =>
      lookup(x, conceptTreeDisjunctions)
    case x: api.DisjointUnionOfConceptsAxiom =>
      lookup(x, disjointUnionOfConceptAxioms)
    case x: api.RuleBodySegment =>
      lookup(x, ruleBodySegments)
    case x: api.SegmentPredicate =>
      lookup(x, segmentPredicates)
    case x: api.RestrictionScalarDataPropertyValue =>
      lookup(x, restrictionScalarDataPropertyValues)
    case x: api.StructuredDataPropertyTuple =>
      lookup(x, structuredPropertyTuples)
    case x: api.ScalarDataPropertyValue =>
      lookup(x, scalarDataPropertyValues)
  }

  val ops: owlapi.OWLAPIOMFOps = omfStore.ops

  private def convertTerminologyGraph
  (uuid: UUID, g0: api.TerminologyGraph)
  (implicit extent: api.Extent)
  : core.OMFError.Throwables \/ (OMLResolver2Ontology, api.Module, OMLResolver2Ontology.MutableTboxOrDbox, IRI)
  = {
    val i = IRI.create(g0.iri)
    om.lookupMutableTerminologyGraph(i)(this.ops) match {
      case Some(g1) =>
        \/-((
          this.copy(modules = this.modules + g0, gs = this.gs + (g0 -> g1)),
          g0,
          g1.left,
          i))
      case None =>
        val k = OMLResolver2Ontology.convertTerminologyKind(g0.kind)
        for {
          g1_om <- this.om.drc.lookupBuiltInModule(i)(this.ops) match {
            case Some(mbox: owlapiterminologies.MutableTerminologyGraph) =>
              (mbox -> om).right[core.OMFError.Throwables]
            case _ =>
              this.ops.makeTerminologyGraph(i, k)(this.omfStore).flatMap { m =>
                om.addMutableModule(m)(this.ops).map { omm =>
                  m -> omm
                }
              }
          }
          (g1, omg) = g1_om
          _ <- if (g1.uuid == uuid && g0.uuid == uuid)
            ().right[core.OMFError.Throwables]
          else
            Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
              s"convertTerminologyBox(g0=${g0.iri}) UUID mismatch\n g0.uuid=${g0.uuid}\n g1.uuid=${g1.uuid}"
            )).left
          edges = g0.moduleEdges().to[Vector].map { e0 => e0.uuid -> (extent -> e0) }.toMap
          elements = g0.moduleElements().to[Vector].map { e0 => e0.uuid -> (extent -> e0) }.toMap
          that = copy(
            om = omg,
            queue_edges = this.queue_edges ++ edges,
            queue_elements = this.queue_elements ++ elements,
            modules = this.modules + g0,
            gs = this.gs + (g0 -> g1))
        } yield (that, g0, g1.left, i)
    }
  }

  private def convertBundle
  (uuid: UUID, b0: api.Bundle)
  (implicit extent: api.Extent)
  : core.OMFError.Throwables \/ (OMLResolver2Ontology, api.Module, OMLResolver2Ontology.MutableTboxOrDbox, IRI)
  = {
    val i = IRI.create(b0.iri)
    om.lookupMutableBundle(i)(this.ops) match {
      case Some(b1) =>
        \/-((
          this.copy(modules = this.modules + b0, bs = this.bs + (b0 -> b1)),
          b0,
          b1.left,
          i))
      case None =>
        val k = OMLResolver2Ontology.convertTerminologyKind(b0.kind)
        for {
          b1 <- this.ops.makeBundle(i, k)(this.omfStore)
          _ <- if (b1.uuid == uuid && b0.uuid == uuid)
            ().right[core.OMFError.Throwables]
          else
            Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
              s"convertBundle(b0=${b0.iri}) UUID mismatch\n b0.uuid=${b0.uuid}\n b1.uuid=${b1.uuid}"
            )).left
          omb <- om.addMutableModule(b1)(this.ops)
          edges = b0.moduleEdges().to[Vector].map { e0 => e0.uuid -> (extent -> e0) }.toMap
          elements = b0.moduleElements().to[Vector].map { e0 => e0.uuid -> (extent -> e0) }.toMap
          that = copy(
            om = omb,
            queue_edges = this.queue_edges ++ edges,
            queue_elements = this.queue_elements ++ elements,
            modules = this.modules + b0,
            bs = this.bs + (b0 -> b1))
        } yield (that, b0, b1.left, i)
    }
  }

  private def convertDescriptionBox
  (uuid: UUID, d0: api.DescriptionBox)
  (implicit extent: api.Extent)
  : core.OMFError.Throwables \/ (OMLResolver2Ontology, api.Module, OMLResolver2Ontology.MutableTboxOrDbox, IRI)
  = {
    val i = IRI.create(d0.iri)
    om.lookupMutableDescriptionBox(i)(this.ops) match {
      case Some(d1) =>
        \/-((
          this.copy(modules = this.modules + d0, ds = this.ds + (d0 -> d1)),
          d0,
          d1.right,
          i))
      case None =>
        val k = OMLResolver2Ontology.convertDescriptionKind(d0.kind)
        for {
          d1 <- this.ops.makeDescriptionBox(i, k)(this.omfStore)
          _ <- if (d1.uuid == uuid && d0.uuid == uuid)
            ().right[core.OMFError.Throwables]
          else
            Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
              s"convertDescriptionBox(d0=${d0.iri}) UUID mismatch\n d0.uuid=${d0.uuid}\n d1.uuid=${d1.uuid}"
            )).left
          omd <- om.addMutableModule(d1)(this.ops)
          edges = d0.moduleEdges().to[Vector].map { e0 => e0.uuid -> (extent -> e0) }.toMap
          elements = d0.moduleElements().to[Vector].map { e0 => e0.uuid -> (extent -> e0) }.toMap
          that = copy(
            om = omd,
            queue_edges = this.queue_edges ++ edges,
            queue_elements = this.queue_elements ++ elements,
            modules = this.modules + d0,
            ds = this.ds + (d0 -> d1))
        } yield (that, d0, d1.right, i)
    }
  }

}
