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

import java.util.UUID

import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable.filterable
import gov.nasa.jpl.imce.oml.converters.utils.EMFProblems
import gov.nasa.jpl.imce.oml.model._
import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.tables.{IRI, ClosedWorldDesignations, OpenWorldDefinitions, TerminologyKind => TTerminologyKind}
import gov.nasa.jpl.imce.oml.tables.{Final, Partial, DescriptionKind => TDescriptionKind}
import org.eclipse.xtext.resource.XtextResourceSet

import scala.collection.immutable._
import scala.{None, Option, Some, StringContext}
import scala.Predef.{ArrowAssoc,String}
import scalaz.Scalaz._
import scalaz._

case class OMLResolver2Text
(mappings: Map[IRI, (api.Extent, common.Extent)] = Map.empty,

 // Modules
 gs: Map[api.TerminologyGraph, graphs.TerminologyGraph] = Map.empty,
 bs: Map[api.Bundle, bundles.Bundle] = Map.empty,
 ds: Map[api.DescriptionBox, descriptions.DescriptionBox] = Map.empty,

 aps: Map[api.AnnotationProperty, common.AnnotationProperty] = Map.empty,

 aspects: Map[api.Aspect, terminologies.Aspect] = Map.empty,
 concepts: Map[api.Concept, terminologies.Concept] = Map.empty,
 reifiedRelationships: Map[api.ReifiedRelationship, terminologies.ReifiedRelationship] = Map.empty,
 unreifiedRelationships: Map[api.UnreifiedRelationship, terminologies.UnreifiedRelationship] = Map.empty,

 dataRanges: Map[api.DataRange, terminologies.DataRange] = Map.empty,
 structures: Map[api.Structure, terminologies.Structure] = Map.empty,
 scalarOneOfLiterals: Map[api.ScalarOneOfLiteralAxiom, terminologies.ScalarOneOfLiteralAxiom] = Map.empty,

 entityScalarDataProperties: Map[api.EntityScalarDataProperty, terminologies.EntityScalarDataProperty] = Map.empty,
 entityStructuredDataProperties: Map[api.EntityStructuredDataProperty, terminologies.EntityStructuredDataProperty] = Map.empty,
 scalarDataProperties: Map[api.ScalarDataProperty, terminologies.ScalarDataProperty] = Map.empty,
 structuredDataProperties: Map[api.StructuredDataProperty, terminologies.StructuredDataProperty] = Map.empty,

 moduleEdges: Map[api.ModuleEdge, common.ModuleEdge] = Map.empty,
 termAxioms: Map[api.TermAxiom, terminologies.TermAxiom] = Map.empty,
 conceptTreeDisjunctions: Map[api.ConceptTreeDisjunction, bundles.ConceptTreeDisjunction] = Map.empty,
 disjointUnionOfConceptAxioms: Map[api.DisjointUnionOfConceptsAxiom, bundles.DisjointUnionOfConceptsAxiom] = Map.empty,
 conceptualEntitySingletonInstances: Map[api.ConceptualEntitySingletonInstance, descriptions.ConceptualEntitySingletonInstance] = Map.empty) {

  def moduleLookup(m: api.Module)
  : Option[common.Module]
  = m match {
    case g: api.TerminologyGraph => gs.get(g)
    case b: api.Bundle => bs.get(b)
    case d: api.DescriptionBox => ds.get(d)
  }

  def tboxLookup(iri: IRI)
  : Option[terminologies.TerminologyBox]
  = gs.values.find { g => g.iri() == iri } orElse bs.values.find { b => b.iri() == iri }

  def bundleLookup(iri: IRI)
  : Option[bundles.Bundle]
  = bs.values.find { b => b.iri() == iri }

  def dboxLookup(iri: IRI)
  : Option[descriptions.DescriptionBox]
  = ds.values.find { d => d.iri() == iri }

  def tboxLookup(m: api.TerminologyBox)
  : Option[terminologies.TerminologyBox]
  = m match {
    case g: api.TerminologyGraph => gs.get(g)
    case b: api.Bundle => bs.get(b)
  }

  def entityLookup(e: api.Entity)
  : Option[terminologies.Entity] = e match {
    case a: api.Aspect => aspects.get(a)
    case c: api.Concept => concepts.get(c)
    case rr: api.ReifiedRelationship => reifiedRelationships.get(rr)
    case _ => None
  }

  def entityRelationshipLookup(rl: api.EntityRelationship)
  : Option[terminologies.EntityRelationship] = rl match {
    case rr: api.ReifiedRelationship => reifiedRelationships.get(rr)
    case ur: api.UnreifiedRelationship => unreifiedRelationships.get(ur)
    case _ => None
  }

  def dataRelationshipToStructureLookup(dp: api.DataRelationshipToStructure)
  : Option[terminologies.DataRelationshipToStructure] = dp match {
    case edp: api.EntityStructuredDataProperty =>
      entityStructuredDataProperties.get(edp)
    case sdp: api.StructuredDataProperty =>
      structuredDataProperties.get(sdp)
  }

  def dataRelationshipToScalarLookup(dp: api.DataRelationshipToScalar)
  : Option[terminologies.DataRelationshipToScalar] = dp match {
    case edp: api.EntityScalarDataProperty =>
      entityScalarDataProperties.get(edp)
    case sdp: api.ScalarDataProperty =>
      scalarDataProperties.get(sdp)
  }
}

object OMLResolver2Text {

  case class ConversionState
  (iri: IRI,
   extent: api.Extent,
   omlExtent: common.Extent,
   conversions: OMLResolver2Text)

  type ConversionResult = EMFProblems \/ ConversionState

  def convert
  (extent: api.Extent,
   rs: XtextResourceSet,
   conversions: OMLResolver2Text)
  : EMFProblems \/ OMLResolver2Text
  = for {
    iri <-
    ( extent.terminologyGraphs.toList,
      extent.bundles.toList,
      extent.descriptionBoxes.toList ) match {
      case (g :: Nil, Nil, Nil) =>
        g._2.iri.right
      case (Nil, b :: Nil, Nil) =>
        b._2.iri.right
      case (Nil, Nil, d :: Nil) =>
        d._2.iri.right
      case (gs, bs, ds) =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"OMLResolver2Text.convert: extent must have a single TerminologyGraph, Bundle or DescriptionBox: "+
          s" got ${gs.size} TerminologyGraphs, ${bs.size} Bundles, ${ds.size} DescriptionBoxes")).left
    }

    _ = java.lang.System.out.println(s"==> OMLResolver2Text converting: $iri")

    c00 <- ConversionState(
      iri,
      extent,
      common.CommonFactory.eINSTANCE.createExtent(),
      conversions).right[EMFProblems]


    // Modules
    c010 = c00.copy(conversions = c00.conversions.copy(mappings = c00.conversions.mappings + (iri -> (c00.extent -> c00.omlExtent))))
    c011 <- c00.extent.terminologyGraphs.foldLeft(c010.right[EMFProblems])(convertTerminologyGraph)
    c012 <- c011.extent.bundles.foldLeft(c011.right[EMFProblems])(convertBundle)
    c013 <- c012.extent.descriptionBoxes.foldLeft(c012.right[EMFProblems])(convertDescription)
    c01 = c013

    // AnnotationProperties
    c02 <- c01.extent.annotationProperties.foldLeft(c01.right[EMFProblems])(convertAnnotationProperty)

    // TerminologyExtensions
    c03 <- c02.extent.terminologyBoxOfTerminologyBoxAxiom.foldLeft(c02.right[EMFProblems])(convertTerminologyExtension)

    // Atomic Entities
    c10 <- c03.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c03.right[EMFProblems])(convertAspectOrConcept)

    // Other ModuleEdges
    c20 <- c10.extent.terminologyBoxOfTerminologyBoxAxiom.foldLeft(c10.right[EMFProblems])(convertConceptDesignationTerminologyAxiom)
    c21 <- c20.extent.terminologyBoxOfTerminologyBoxAxiom.foldLeft(c20.right[EMFProblems])(convertTerminologyNestingAxiom)
    c22 <- c21.extent.bundleOfTerminologyBundleAxiom.foldLeft(c21.right[EMFProblems])(convertBundledTerminologyAxiom)
    c23 <- c22.extent.descriptionBoxOfDescriptionBoxExtendsClosedWorldDefinitions.foldLeft(c22.right[EMFProblems])(convertDescriptionBoxExtendsClosedWorldDefinition)
    c24 <- c23.extent.descriptionBoxOfDescriptionBoxRefinement.foldLeft(c23.right[EMFProblems])(convertDescriptionBoxRefinement)

    // Relationships

    c30 <- c24.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c24.right[EMFProblems])(convertReifiedRelationship)
    c31 <- c30.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c30.right[EMFProblems])(convertUnreifiedRelationship)

    // DataTypes

    c32 <- c31.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c31.right[EMFProblems])(convertStructure)
    c33 <- c32.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c32.right[EMFProblems])(convertScalar)
    c34 <- convertRestrictedDataRanges(
      c33.right[EMFProblems],
      c33
        .extent
        .terminologyBoxOfTerminologyBoxStatement
        .selectByKindOf { case (dr: api.RestrictedDataRange, t: api.TerminologyBox) => dr -> t },
      List.empty)
    c35 <- c34.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c34.right[EMFProblems])(convertScalarOneOfLiteralAxiom)

    // DataRelationships

    c40 <- c35.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c35.right[EMFProblems])(convertEntityScalarDataProperty)
    c41 <- c40.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c40.right[EMFProblems])(convertEntityStructuredDataProperty)
    c42 <- c41.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c41.right[EMFProblems])(convertScalarDataProperty)
    c43 <- c42.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c42.right[EMFProblems])(convertStructuredDataProperty)

    // Restrictions

    c50 <- c43.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c43.right[EMFProblems])(convertEntityRestrictionAxiom)
    c51 <- c50.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c50.right[EMFProblems])(convertEntityScalarDataPropertyRestrictionAxiom)

    // Specializations

    c52 <- c51.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c51.right[EMFProblems])(convertSpecializationAxiom)

    // Disjunctions

    c60 <- c52.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c52.right[EMFProblems])(convertRootConceptTaxonomyAxiom)

    // ConceptualEntityInstances
    // @TODO

    // Data Property Values
    // @TODO

    // Annotations

    // Finished!
    cDone = c60

    result = cDone.conversions

    _ = java.lang.System.out.println(s"==> OMLResolver2Text  converted: $iri")
  } yield result

  def normalizeName(n: String): String = n.replaceAll("[.]","_")

  // Module

  protected def convertTerminologyKind(k: TTerminologyKind): terminologies.TerminologyKind = k match {
    case OpenWorldDefinitions => terminologies.TerminologyKind.OPEN_WORLD_DEFINITIONS
    case ClosedWorldDesignations => terminologies.TerminologyKind.CLOSED_WORLD_DESIGNATIONS
  }

  protected def convertDescriptionKind(k: TDescriptionKind): descriptions.DescriptionKind = k match {
    case Final => descriptions.DescriptionKind.FINAL
    case Partial => descriptions.DescriptionKind.PARTIAL
  }

  protected val convertTerminologyGraph
  : (ConversionResult, (UUID, api.TerminologyGraph)) => ConversionResult
  = { case (acc, (_, g0)) =>
    for {
      r2t <- acc
      g1 = graphs.GraphsFactory.eINSTANCE.createTerminologyGraph()
      _ = g1.setExtent(r2t.omlExtent)
      _ = g1.setKind(convertTerminologyKind(g0.kind))
      _ = g1.setIri(g0.iri)
    } yield r2t.copy(conversions = r2t.conversions.copy(gs = r2t.conversions.gs + (g0 -> g1)))
  }

  protected val convertBundle
  : (ConversionResult, (UUID, api.Bundle)) => ConversionResult
  = { case (acc, (_, b0)) =>
    for {
      r2t <- acc
      b1 = bundles.BundlesFactory.eINSTANCE.createBundle()
      _ = b1.setExtent(r2t.omlExtent)
      _ = b1.setKind(convertTerminologyKind(b0.kind))
      _ = b1.setIri(b0.iri)
    } yield r2t.copy(conversions = r2t.conversions.copy(bs = r2t.conversions.bs + (b0 -> b1)))
  }

  protected val convertDescription
  : (ConversionResult, (UUID, api.DescriptionBox)) => ConversionResult
  = { case (acc, (_, d0)) =>
    for {
      r2t <- acc
      d1 = descriptions.DescriptionsFactory.eINSTANCE.createDescriptionBox()
      _ = d1.setExtent(r2t.omlExtent)
      _ = d1.setKind(convertDescriptionKind(d0.kind))
      _ = d1.setIri(d0.iri)
    } yield r2t.copy(conversions = r2t.conversions.copy(ds = r2t.conversions.ds + (d0 -> d1)))
  }

  // AnnotationProperty

  protected val convertAnnotationProperty
  : (ConversionResult, (UUID, api.AnnotationProperty)) => ConversionResult
  = { case (acc, (_, ap0)) =>
    for {
      r2t <- acc
      next = r2t.conversions.aps.get(ap0) match {
        case None =>
          val ap1 = common.CommonFactory.eINSTANCE.createAnnotationProperty()
          ap1.setExtent(r2t.omlExtent)
          ap1.setAbbrevIRI(ap0.abbrevIRI)
          ap1.setIri(ap0.iri)
//          java.lang.System.out.println(s"convertAnnotationProperty[${r2t.iri}](abbrevIRI=${ap0.abbrevIRI}, iri=${ap0.iri}")
          r2t.copy(conversions = r2t.conversions.copy(aps = r2t.conversions.aps + (ap0 -> ap1)))
        case Some(_) =>
          r2t
      }
    } yield next
  }

  // TerminologyExtensions

  protected val convertTerminologyExtension
  : (ConversionResult, (api.TerminologyBoxAxiom, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (ax0: api.TerminologyExtensionAxiom, t0)) =>
      val ctw: ConversionResult = for {
        r2t <- acc
        ax1 = terminologies.TerminologiesFactory.eINSTANCE.createTerminologyExtensionAxiom()
        upd <- (r2t.conversions.tboxLookup(t0), r2t.conversions.tboxLookup(ax0.extendedTerminology)) match {
          case (Some(t1), Some(e1)) =>
            ax1.setTbox(t1)
            ax1.setExtendedTerminology(e1)
            r2t.copy(conversions = r2t.conversions.copy(moduleEdges = r2t.conversions.moduleEdges + (ax0 -> ax1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertTerminologyExtensions: Failed to resolve " +
                s"extending tbox: $t0" +
                s"extended tbox: ${ax0.extendedTerminology}")).left
        }
      } yield upd
      ctw
    case (acc, _) =>
      acc
  }

  // Atomic Entities

  protected val convertAspectOrConcept
  : (ConversionResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (a0: api.Aspect, t0)) =>
      for {
        r2t <- acc
        a1 = terminologies.TerminologiesFactory.eINSTANCE.createAspect()
        upd <- r2t.conversions.tboxLookup(t0) match {
          case Some(t1) =>
            a1.setTbox(t1)
            a1.setName(normalizeName(a0.name))
            r2t.copy(conversions = r2t.conversions.copy(aspects = r2t.conversions.aspects + (a0 -> a1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertAspect: Failed to resolve " +
                s"tbox: $t0" +
                s" for defining aspect: $a0")).left
        }
      } yield upd
    case (acc, (c0: api.Concept, t0)) =>
      for {
        r2t <- acc
        c1 = terminologies.TerminologiesFactory.eINSTANCE.createConcept()
        upd <- r2t.conversions.tboxLookup(t0) match {
          case Some(t1) =>
            c1.setTbox(t1)
            c1.setName(normalizeName(c0.name))
            r2t.copy(conversions = r2t.conversions.copy(concepts = r2t.conversions.concepts + (c0 -> c1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertConcept: Failed to resolve " +
                s"tbox: $t0" +
                s" for defining concept: $c0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  // Other ModuleEdges

  protected val convertConceptDesignationTerminologyAxiom
  : (ConversionResult, (api.TerminologyBoxAxiom, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (ax0: api.ConceptDesignationTerminologyAxiom, t0)) =>
      for {
        r2t <- acc
        ax1 = graphs.GraphsFactory.eINSTANCE.createConceptDesignationTerminologyAxiom()
        upd <- (r2t.conversions.tboxLookup(t0),
          r2t.conversions.tboxLookup(ax0.designatedTerminology),
          r2t.conversions.concepts.get(ax0.designatedConcept)) match {
          case (Some(t1), Some(dt1), Some(dc1)) =>
            ax1.setTbox(t1)
            ax1.setDesignatedTerminology(dt1)
            ax1.setDesignatedConcept(dc1)
            r2t.copy(conversions = r2t.conversions.copy(moduleEdges = r2t.conversions.moduleEdges + (ax0 -> ax1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertConceptDesignationTerminologyAxiom: Failed to resolve " +
                s"tbox: $t0" +
                s" for ConceptDesignationTerminologyAxiom: $ax0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  protected val convertTerminologyNestingAxiom
  : (ConversionResult, (api.TerminologyBoxAxiom, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (ax0: api.TerminologyNestingAxiom, t0)) =>
      for {
        r2t <- acc
        ax1 = graphs.GraphsFactory.eINSTANCE.createTerminologyNestingAxiom()
        upd <- (r2t.conversions.tboxLookup(t0),
          r2t.conversions.tboxLookup(ax0.nestingTerminology),
          r2t.conversions.concepts.get(ax0.nestingContext)) match {
          case (Some(t1), Some(nt1), Some(nc1)) =>
            ax1.setTbox(t1)
            ax1.setNestingTerminology(nt1)
            ax1.setNestingContext(nc1)
            r2t.copy(conversions = r2t.conversions.copy(moduleEdges = r2t.conversions.moduleEdges + (ax0 -> ax1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertTerminologyNestingAxiom: Failed to resolve " +
                s"tbox: $t0" +
                s" for TerminologyNestingAxiom: $ax0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  protected val convertBundledTerminologyAxiom
  : (ConversionResult, (api.TerminologyBundleAxiom, api.Bundle)) => ConversionResult
  = {
    case (acc, (ax0: api.BundledTerminologyAxiom, b0)) =>
      for {
        r2t <- acc
        ax1 = bundles.BundlesFactory.eINSTANCE.createBundledTerminologyAxiom()
        upd <- (r2t.conversions.bs.get(b0), r2t.conversions.tboxLookup(ax0.bundledTerminology)) match {
          case (Some(b1), Some(bt1)) =>
            ax1.setBundle(b1)
            ax1.setBundledTerminology(bt1)
            r2t.copy(conversions = r2t.conversions.copy(moduleEdges = r2t.conversions.moduleEdges + (ax0 -> ax1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertBundledTerminologyAxiom: Failed to resolve " +
                s"bundle: $b0" +
                s" for BundledTerminologyAxiom: $ax0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  protected val convertDescriptionBoxExtendsClosedWorldDefinition
  : (ConversionResult, (api.DescriptionBoxExtendsClosedWorldDefinitions, api.DescriptionBox)) => ConversionResult
  = {
    case (acc, (ax0, d0)) =>
      for {
        r2t <- acc
        ax1 = descriptions.DescriptionsFactory.eINSTANCE.createDescriptionBoxExtendsClosedWorldDefinitions()
        upd <- (r2t.conversions.ds.get(d0), r2t.conversions.tboxLookup(ax0.closedWorldDefinitions)) match {
          case (Some(d1), Some(cwt1)) =>
            ax1.setDescriptionBox(d1)
            ax1.setClosedWorldDefinitions(cwt1)
            r2t.copy(conversions = r2t.conversions.copy(moduleEdges = r2t.conversions.moduleEdges + (ax0 -> ax1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertDescriptionBoxExtendsClosedWorldDefinition: Failed to resolve " +
                s"descriptionBox: $d0" +
                s" for DescriptionBoxExtendsClosedWorldDefinition: $ax0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  protected val convertDescriptionBoxRefinement
  : (ConversionResult, (api.DescriptionBoxRefinement, api.DescriptionBox)) => ConversionResult
  = {
    case (acc, (ax0, d0)) =>
      for {
        r2t <- acc
        ax1 = descriptions.DescriptionsFactory.eINSTANCE.createDescriptionBoxRefinement()
        upd <- (r2t.conversions.ds.get(d0), r2t.conversions.dboxLookup(ax0.refinedDescriptionBox)) match {
          case (Some(d1), Some(rd1)) =>
            ax1.setRefiningDescriptionBox(d1)
            ax1.setRefinedDescriptionBox(rd1)
            r2t.copy(conversions = r2t.conversions.copy(moduleEdges = r2t.conversions.moduleEdges + (ax0 -> ax1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertDescriptionBoxRefinement: Failed to resolve " +
                s"descriptionBox: $d0" +
                s" for DescriptionBoxRefinement: $ax0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  // Relationships

  protected val convertReifiedRelationship
  : (ConversionResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (rr0: api.ReifiedRelationship, t0)) =>
      for {
        r2t <- acc
        rr1 = terminologies.TerminologiesFactory.eINSTANCE.createReifiedRelationship()
        upd <- (r2t.conversions.tboxLookup(t0),
        r2t.conversions.entityLookup(rr0.source),
        r2t.conversions.entityLookup(rr0.target)) match {
          case (Some(t1), Some(rs1), Some(rt1)) =>
            rr1.setTbox(t1)
            rr1.setName(normalizeName(rr0.name))
            rr1.setIsAsymmetric(rr0.isAsymmetric)
            rr1.setIsEssential(rr0.isEssential)
            rr1.setIsFunctional(rr0.isFunctional)
            rr1.setIsInverseEssential(rr0.isInverseEssential)
            rr1.setIsInverseFunctional(rr0.isInverseFunctional)
            rr1.setIsIrreflexive(rr0.isIrreflexive)
            rr1.setIsReflexive(rr0.isSymmetric)
            rr1.setIsTransitive(rr0.isTransitive)
            rr1.setUnreifiedPropertyName(rr0.unreifiedPropertyName)
            rr0.unreifiedInversePropertyName.foreach(rr1.setUnreifiedInversePropertyName)
            rr1.setSource(rs1)
            rr1.setTarget(rt1)
            r2t.copy(conversions = r2t.conversions.copy(reifiedRelationships = r2t.conversions.reifiedRelationships + (rr0 -> rr1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertReifiedRelationship: Failed to resolve " +
                s"tbox: $t0" +
                s" for defining ReifiedRelationship: $rr0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  protected val convertUnreifiedRelationship
  : (ConversionResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (ur0: api.UnreifiedRelationship, t0)) =>
      for {
        r2t <- acc
        ur1 = terminologies.TerminologiesFactory.eINSTANCE.createUnreifiedRelationship()
        upd <- (r2t.conversions.tboxLookup(t0),
          r2t.conversions.entityLookup(ur0.source),
          r2t.conversions.entityLookup(ur0.target)) match {
          case (Some(t1), Some(rs1), Some(rt1)) =>
            ur1.setTbox(t1)
            ur1.setName(normalizeName(ur0.name))
            ur1.setIsAsymmetric(ur0.isAsymmetric)
            ur1.setIsEssential(ur0.isEssential)
            ur1.setIsFunctional(ur0.isFunctional)
            ur1.setIsInverseEssential(ur0.isInverseEssential)
            ur1.setIsInverseFunctional(ur0.isInverseFunctional)
            ur1.setIsIrreflexive(ur0.isIrreflexive)
            ur1.setIsReflexive(ur0.isSymmetric)
            ur1.setIsTransitive(ur0.isTransitive)
            ur1.setSource(rs1)
            ur1.setTarget(rt1)
            r2t.copy(conversions = r2t.conversions.copy(unreifiedRelationships = r2t.conversions.unreifiedRelationships + (ur0 -> ur1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertUnreifiedRelationship: Failed to resolve " +
                s"tbox: $t0" +
                s" for defining UnreifiedRelationship: $ur0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  // DataTypes

  protected val convertStructure
  : (ConversionResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (s0: api.Structure, t0)) =>
      for {
        r2t <- acc
        s1 = terminologies.TerminologiesFactory.eINSTANCE.createStructure()
        upd <- r2t.conversions.tboxLookup(t0) match {
          case Some(t1) =>
            s1.setTbox(t1)
            s1.setName(normalizeName(s0.name))
            r2t.copy(conversions = r2t.conversions.copy(structures = r2t.conversions.structures + (s0 -> s1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertStructure: Failed to resolve " +
                s"tbox: $t0" +
                s" for defining Structure: $s0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  protected val convertScalar
  : (ConversionResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (s0: api.Scalar, t0)) =>
      for {
        r2t <- acc
        s1 = terminologies.TerminologiesFactory.eINSTANCE.createScalar()
        upd <- r2t.conversions.tboxLookup(t0) match {
          case Some(t1) =>
            s1.setTbox(t1)
            s1.setName(normalizeName(s0.name))
            r2t.copy(conversions = r2t.conversions.copy(dataRanges = r2t.conversions.dataRanges + (s0 -> s1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertScalar: Failed to resolve " +
                s"tbox: $t0" +
                s" for defining Scalar: $s0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  @scala.annotation.tailrec
  protected def convertRestrictedDataRanges
  (acc: ConversionResult,
   drs: Iterable[(api.RestrictedDataRange, api.TerminologyBox)],
   queue: List[(api.RestrictedDataRange, api.TerminologyBox)])
  : ConversionResult
  = if (drs.isEmpty) {
    if (queue.isEmpty)
      acc
    else
      convertRestrictedDataRanges(acc, queue, List.empty)
  }
  else
    acc match {
      case \/-(r2t) =>
        val (dr0, t0) = drs.head
        (r2t.conversions.tboxLookup(t0), r2t.conversions.dataRanges.get(dr0.restrictedRange)) match {
          case (Some(t1), Some(rr1)) =>
            val dr1 = dr0 match {
              case rdr0: api.BinaryScalarRestriction =>
                val rdr1 = terminologies.TerminologiesFactory.eINSTANCE.createBinaryScalarRestriction()
                rdr0.length.foreach(v => rdr1.setLength(new datatypes.PositiveIntegerValue(v)))
                rdr0.minLength.foreach(v => rdr1.setMinLength(new datatypes.PositiveIntegerValue(v)))
                rdr0.maxLength.foreach(v => rdr1.setMaxLength(new datatypes.PositiveIntegerValue(v)))
                rdr1
              case rdr0: api.IRIScalarRestriction =>
                val rdr1 = terminologies.TerminologiesFactory.eINSTANCE.createIRIScalarRestriction()
                rdr0.length.foreach(v => rdr1.setLength(new datatypes.PositiveIntegerValue(v)))
                rdr0.minLength.foreach(v => rdr1.setMinLength(new datatypes.PositiveIntegerValue(v)))
                rdr0.maxLength.foreach(v => rdr1.setMaxLength(new datatypes.PositiveIntegerValue(v)))
                rdr0.pattern.foreach(v => rdr1.setPattern(new datatypes.PatternValue(v)))
                rdr1
              case rdr0: api.NumericScalarRestriction =>
                val rdr1 = terminologies.TerminologiesFactory.eINSTANCE.createNumericScalarRestriction()
                rdr0.minInclusive.foreach(v => rdr1.setMinInclusive(tables2emf(v)))
                rdr0.maxInclusive.foreach(v => rdr1.setMaxInclusive(tables2emf(v)))
                rdr0.minExclusive.foreach(v => rdr1.setMinExclusive(tables2emf(v)))
                rdr0.maxExclusive.foreach(v => rdr1.setMaxExclusive(tables2emf(v)))
                rdr1
              case rdr0: api.PlainLiteralScalarRestriction =>
                val rdr1 = terminologies.TerminologiesFactory.eINSTANCE.createPlainLiteralScalarRestriction()
                rdr0.length.foreach(v => rdr1.setLength(new datatypes.PositiveIntegerValue(v)))
                rdr0.minLength.foreach(v => rdr1.setMinLength(new datatypes.PositiveIntegerValue(v)))
                rdr0.maxLength.foreach(v => rdr1.setMaxLength(new datatypes.PositiveIntegerValue(v)))
                rdr0.pattern.foreach(v => rdr1.setPattern(new datatypes.PatternValue(v)))
                rdr0.langRange.foreach(v => rdr1.setLangRange(new datatypes.LanguageTagValue(v)))
                rdr1
              case _: api.ScalarOneOfRestriction =>
                val rdr1 = terminologies.TerminologiesFactory.eINSTANCE.createScalarOneOfRestriction()
                rdr1
              case rdr0: api.StringScalarRestriction =>
                val rdr1 = terminologies.TerminologiesFactory.eINSTANCE.createStringScalarRestriction()
                rdr0.length.foreach(v => rdr1.setLength(new datatypes.PositiveIntegerValue(v)))
                rdr0.minLength.foreach(v => rdr1.setMinLength(new datatypes.PositiveIntegerValue(v)))
                rdr0.maxLength.foreach(v => rdr1.setMaxLength(new datatypes.PositiveIntegerValue(v)))
                rdr0.pattern.foreach(v => rdr1.setPattern(new datatypes.PatternValue(v)))
                rdr1
              case _: api.SynonymScalarRestriction =>
                val rdr1 = terminologies.TerminologiesFactory.eINSTANCE.createSynonymScalarRestriction()
                rdr1
              case rdr0: api.TimeScalarRestriction =>
                val rdr1 = terminologies.TerminologiesFactory.eINSTANCE.createTimeScalarRestriction()
                rdr0.minInclusive.foreach(v => rdr1.setMinInclusive(tables2emf(v)))
                rdr0.maxInclusive.foreach(v => rdr1.setMaxInclusive(tables2emf(v)))
                rdr0.minExclusive.foreach(v => rdr1.setMinExclusive(tables2emf(v)))
                rdr0.maxExclusive.foreach(v => rdr1.setMaxExclusive(tables2emf(v)))
                rdr1
            }
            dr1.setTbox(t1)
            dr1.setName(normalizeName(dr0.name))
            dr1.setRestrictedRange(rr1)
            convertRestrictedDataRanges(
              r2t.copy(conversions = r2t.conversions.copy(dataRanges = r2t.conversions.dataRanges + (dr0 -> dr1))).right[EMFProblems],
              drs.tail,
              queue)
          case (Some(t1), None) =>
            convertRestrictedDataRanges(acc, drs.tail, drs.head :: queue)
          case (None, _) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRestrictedDataRanges: Failed to resolve " +
                s"tbox: $t0")).left[ConversionState]
        }
      case _ =>
        acc
    }

  protected val convertScalarOneOfLiteralAxiom
  : (ConversionResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (s0: api.ScalarOneOfLiteralAxiom, t0)) =>
      for {
        r2t <- acc
        s1 = terminologies.TerminologiesFactory.eINSTANCE.createScalarOneOfLiteralAxiom()
        upd <- (r2t.conversions.tboxLookup(t0), r2t.conversions.dataRanges.get(s0.axiom)) match {
          case (Some(t1), Some(a1: terminologies.ScalarOneOfRestriction)) =>
            s1.setTbox(t1)
            s1.setAxiom(a1)
            s1.setValue(tables2emf(s0.value))
            r2t.copy(conversions = r2t.conversions.copy(scalarOneOfLiterals = r2t.conversions.scalarOneOfLiterals + (s0 -> s1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertScalarOneOfLiteralAxiom: Failed to resolve " +
                s"tbox: $t0" +
                s" for defining ScalarOneOfLiteralAxiom: $s0")).left[ConversionState]
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  protected val convertEntityScalarDataProperty
  : (ConversionResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (dp0: api.EntityScalarDataProperty, t0)) =>
      for {
        r2t <- acc
        dp1 = terminologies.TerminologiesFactory.eINSTANCE.createEntityScalarDataProperty()
        upd <- (r2t.conversions.tboxLookup(t0), r2t.conversions.entityLookup(dp0.domain), r2t.conversions.dataRanges.get(dp0.range)) match {
          case (Some(t1), Some(e1), Some(r1)) =>
            dp1.setTbox(t1)
            dp1.setName(normalizeName(dp0.name))
            dp1.setIsIdentityCriteria(dp0.isIdentityCriteria)
            dp1.setDomain(e1)
            dp1.setRange(r1)
            r2t.copy(conversions = r2t.conversions.copy(entityScalarDataProperties = r2t.conversions.entityScalarDataProperties + (dp0 -> dp1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertEntityScalarDataProperty: Failed to resolve " +
                s"tbox: $t0" +
                s" for defining EntityScalarDataProperty: $dp0")).left[ConversionState]
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  protected val convertEntityStructuredDataProperty
  : (ConversionResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (dp0: api.EntityStructuredDataProperty, t0)) =>
      for {
        r2t <- acc
        dp1 = terminologies.TerminologiesFactory.eINSTANCE.createEntityStructuredDataProperty()
        upd <- (r2t.conversions.tboxLookup(t0), r2t.conversions.entityLookup(dp0.domain), r2t.conversions.structures.get(dp0.range)) match {
          case (Some(t1), Some(e1), Some(r1)) =>
            dp1.setTbox(t1)
            dp1.setName(normalizeName(dp0.name))
            dp1.setIsIdentityCriteria(dp0.isIdentityCriteria)
            dp1.setDomain(e1)
            dp1.setRange(r1)
            r2t.copy(conversions = r2t.conversions.copy(entityStructuredDataProperties = r2t.conversions.entityStructuredDataProperties + (dp0 -> dp1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertEntityStructuredDataProperty: Failed to resolve " +
                s"tbox: $t0" +
                s" for defining EntityStructuredDataProperty: $dp0")).left[ConversionState]
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  protected val convertScalarDataProperty
  : (ConversionResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (dp0: api.ScalarDataProperty, t0)) =>
      for {
        r2t <- acc
        dp1 = terminologies.TerminologiesFactory.eINSTANCE.createScalarDataProperty()
        upd <- (r2t.conversions.tboxLookup(t0), r2t.conversions.structures.get(dp0.domain), r2t.conversions.dataRanges.get(dp0.range)) match {
          case (Some(t1), Some(e1), Some(r1)) =>
            dp1.setTbox(t1)
            dp1.setName(normalizeName(dp0.name))
            dp1.setDomain(e1)
            dp1.setRange(r1)
            r2t.copy(conversions = r2t.conversions.copy(scalarDataProperties = r2t.conversions.scalarDataProperties + (dp0 -> dp1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertScalarDataProperty: Failed to resolve " +
                s"tbox: $t0" +
                s" for defining ScalarDataProperty: $dp0")).left[ConversionState]
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  protected val convertStructuredDataProperty
  : (ConversionResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (dp0: api.StructuredDataProperty, t0)) =>
      for {
        r2t <- acc
        dp1 = terminologies.TerminologiesFactory.eINSTANCE.createStructuredDataProperty()
        upd <- (r2t.conversions.tboxLookup(t0), r2t.conversions.structures.get(dp0.domain), r2t.conversions.structures.get(dp0.range)) match {
          case (Some(t1), Some(e1), Some(r1)) =>
            dp1.setTbox(t1)
            dp1.setName(normalizeName(dp0.name))
            dp1.setDomain(e1)
            dp1.setRange(r1)
            r2t.copy(conversions = r2t.conversions.copy(structuredDataProperties = r2t.conversions.structuredDataProperties + (dp0 -> dp1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertStructuredDataProperty: Failed to resolve " +
                s"tbox: $t0" +
                s" for defining StructuredDataProperty: $dp0")).left[ConversionState]
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  protected val convertEntityRestrictionAxiom
  : (ConversionResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (er0: api.EntityRestrictionAxiom, t0)) =>
      for {
        r2t <- acc
        er1 = er0 match {
          case _: api.EntityExistentialRestrictionAxiom =>
            terminologies.TerminologiesFactory.eINSTANCE.createEntityExistentialRestrictionAxiom()
          case _: api.EntityUniversalRestrictionAxiom =>
            terminologies.TerminologiesFactory.eINSTANCE.createEntityUniversalRestrictionAxiom()
        }
        upd <- (r2t.conversions.tboxLookup(t0),
          r2t.conversions.entityLookup(er0.restrictedDomain),
          r2t.conversions.entityRelationshipLookup(er0.restrictedRelation),
          r2t.conversions.entityLookup(er0.restrictedRange)) match {
          case (Some(t1), Some(d1), Some(rel1), Some(r1)) =>
            er1.setTbox(t1)
            er1.setRestrictedDomain(d1)
            er1.setRestrictedRelation(rel1)
            er1.setRestrictedRange(r1)
            r2t.copy(conversions = r2t.conversions.copy(termAxioms = r2t.conversions.termAxioms + (er0 -> er1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertEntityRestrictionAxiom: Failed to resolve " +
                s"tbox: $t0" +
                s" for defining EntityRestrictionAxiom: $er0")).left[ConversionState]
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  protected val convertEntityScalarDataPropertyRestrictionAxiom
  : (ConversionResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (er0: api.EntityScalarDataPropertyRestrictionAxiom, t0)) =>
      for {
        r2t <- acc
        upd <- (r2t.conversions.tboxLookup(t0),
          r2t.conversions.entityLookup(er0.restrictedEntity),
          r2t.conversions.entityScalarDataProperties.get(er0.scalarProperty)) match {
          case (Some(t1), Some(e1), Some(dp1)) =>
            val er1 = er0 match {
              case _: api.EntityScalarDataPropertyExistentialRestrictionAxiom =>
                terminologies.TerminologiesFactory.eINSTANCE.createEntityScalarDataPropertyExistentialRestrictionAxiom()
              case _: api.EntityScalarDataPropertyUniversalRestrictionAxiom =>
                terminologies.TerminologiesFactory.eINSTANCE.createEntityScalarDataPropertyUniversalRestrictionAxiom()
              case ep0: api.EntityScalarDataPropertyParticularRestrictionAxiom =>
                val ep1 = terminologies.TerminologiesFactory.eINSTANCE.createEntityScalarDataPropertyParticularRestrictionAxiom()
                ep1.setLiteralValue(tables2emf(ep0.literalValue))
                ep1
            }
            er1.setTbox(t1)
            er1.setRestrictedEntity(e1)
            er1.setScalarProperty(dp1)
            r2t.copy(conversions = r2t.conversions.copy(termAxioms = r2t.conversions.termAxioms + (er0 -> er1))).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertEntityScalarDataPropertyRestrictionAxiom: Failed to resolve " +
                s"tbox: $t0" +
                s" for defining EntityScalarDataPropertyRestrictionAxiom: $er0")).left[ConversionState]
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  protected val convertSpecializationAxiom
  : (ConversionResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (ax0: api.SpecializationAxiom, t0)) =>
      for {
        r2t <- acc
        ax1 <- (ax0, r2t.conversions.tboxLookup(t0), r2t.conversions.entityLookup(ax0.parent()), r2t.conversions.entityLookup(ax0.child())) match {
          case (_: api.ConceptSpecializationAxiom,
          Some(t1),
          Some(sup: terminologies.Concept),
          Some(sub: terminologies.Concept)) =>
            val s1 = terminologies.TerminologiesFactory.eINSTANCE.createConceptSpecializationAxiom()
            s1.setTbox(t1)
            s1.setSuperConcept(sup)
            s1.setSubConcept(sub)
            s1.right[EMFProblems]
          case (_: api.AspectSpecializationAxiom,
          Some(t1),
          Some(sup: terminologies.Aspect),
          Some(sub)) =>
            val s1 = terminologies.TerminologiesFactory.eINSTANCE.createAspectSpecializationAxiom()
            s1.setTbox(t1)
            s1.setSuperAspect(sup)
            s1.setSubEntity(sub)
            s1.right[EMFProblems]
          case (_: api.ReifiedRelationshipSpecializationAxiom,
          Some(t1),
          Some(sup: terminologies.ReifiedRelationship),
          Some(sub: terminologies.ReifiedRelationship)) =>
            val s1 = terminologies.TerminologiesFactory.eINSTANCE.createReifiedRelationshipSpecializationAxiom()
            s1.setTbox(t1)
            s1.setSuperRelationship(sup)
            s1.setSubRelationship(sub)
            s1.right[EMFProblems]
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertSpecializationAxiom: Failed to resolve " +
                s"tbox: $t0" +
                s" for defining SpecializationAxiom: $ax0")).left[terminologies.SpecializationAxiom]
        }
      } yield r2t.copy(conversions = r2t.conversions.copy(termAxioms = r2t.conversions.termAxioms + (ax0 -> ax1)))
    case (acc, _) =>
      acc
  }

  protected val convertRootConceptTaxonomyAxiom
  : (ConversionResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (ax0: api.RootConceptTaxonomyAxiom, b0: api.Bundle)) =>
      for {
        r2t <- acc
        ax1 = bundles.BundlesFactory.eINSTANCE.createRootConceptTaxonomyAxiom()
        upd <- (r2t.conversions.bs.get(b0), r2t.conversions.concepts.get(ax0.root)) match {
          case (Some(b1), Some(r1)) =>
            ax1.setBundle(b1)
            ax1.setRoot(r1)
            val next = r2t.copy(conversions = r2t.conversions.copy(conceptTreeDisjunctions = r2t.conversions.conceptTreeDisjunctions + (ax0 -> ax1)))
            val disjuncts = disjunctsForConceptTreeDisjunction(next, ax0, ax1)
            convertConceptTreeDisjunctions(next, disjuncts)
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRootConceptTaxonomyAxiom: Failed to resolve " +
                s"tbox: $b0" +
                s" for defining RootConceptTaxonomyAxiom: $ax0")).left[ConversionState]
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  protected def disjunctsForConceptTreeDisjunction
  (r2t: ConversionState, ctd0: api.ConceptTreeDisjunction, ctd1: bundles.ConceptTreeDisjunction)
  : Seq[(api.DisjointUnionOfConceptsAxiom, bundles.ConceptTreeDisjunction)]
  = r2t.extent.disjunctions.getOrElse(ctd0, Set.empty).to[Seq].map(_ -> ctd1)

  @scala.annotation.tailrec
  protected def convertConceptTreeDisjunctions
  (r2t: ConversionState, disjuncts: Seq[(api.DisjointUnionOfConceptsAxiom, bundles.ConceptTreeDisjunction)])
  : ConversionResult
  = if (disjuncts.isEmpty)
    r2t.right
  else {
    val (dis0, ctd1) = disjuncts.head
    dis0 match {
      case ax0: api.AnonymousConceptUnionAxiom =>
        val ax1 = bundles.BundlesFactory.eINSTANCE.createAnonymousConceptUnionAxiom()
        ax1.setName(normalizeName(ax0.name))
        val next = r2t.copy(conversions = r2t.conversions.copy(disjointUnionOfConceptAxioms = r2t.conversions.disjointUnionOfConceptAxioms + (ax0 -> ax1)))
        val children = disjunctsForConceptTreeDisjunction(next, ax0, ax1)
        convertConceptTreeDisjunctions(next, children ++ disjuncts.tail)

      case ax0: api.SpecificDisjointConceptAxiom =>
        val ax1 = bundles.BundlesFactory.eINSTANCE.createSpecificDisjointConceptAxiom()
        r2t.conversions.concepts.get(ax0.disjointLeaf) match {
          case Some(l1) =>
            ax1.setDisjointLeaf(l1)
            ax1.setDisjointTaxonomyParent(ctd1)
            val next = r2t.copy(conversions = r2t.conversions.copy(disjointUnionOfConceptAxioms = r2t.conversions.disjointUnionOfConceptAxioms + (ax0 -> ax1)))
            convertConceptTreeDisjunctions(next, disjuncts.tail)
          case None =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertConceptTreeDisjunctions: Failed to resolve " +
                s"tbox: ${ax0.disjointLeaf}")).left[ConversionState]
        }
    }
  }

}