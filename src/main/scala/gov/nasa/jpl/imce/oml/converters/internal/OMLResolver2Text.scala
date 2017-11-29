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

import java.util.UUID

import gov.nasa.jpl.imce.oml.converters.tables2emf
import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable.filterable
import gov.nasa.jpl.imce.oml.converters.utils.EMFProblems
import gov.nasa.jpl.imce.oml.model._
import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.tables.{ClosedWorldDesignations, OpenWorldDefinitions, TerminologyKind => TTerminologyKind}
import gov.nasa.jpl.imce.oml.tables.{Final, Partial, DescriptionKind => TDescriptionKind}
import org.eclipse.xtext.resource.XtextResourceSet

import scala.collection.immutable._
import scala.{None, Option, Some, StringContext}
import scala.Predef.{ArrowAssoc,String}
import scalaz.Scalaz._
import scalaz._

case class OMLResolver2Text
(mappings: Seq[(tables.taggedTypes.IRI, (api.Extent, common.Extent))] = Seq.empty,

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
 conceptualEntitySingletonInstances: Map[api.ConceptualEntitySingletonInstance, descriptions.ConceptualEntitySingletonInstance] = Map.empty,
 reifiedRelationshipInstanceDomains: Map[api.ReifiedRelationshipInstanceDomain, descriptions.ReifiedRelationshipInstanceDomain] = Map.empty,
 reifiedRelationshipInstanceRanges: Map[api.ReifiedRelationshipInstanceRange, descriptions.ReifiedRelationshipInstanceRange] = Map.empty,
 unreifiedRelationshipInstanceTuples: Map[api.UnreifiedRelationshipInstanceTuple, descriptions.UnreifiedRelationshipInstanceTuple] = Map.empty,
 singletonInstanceStructuredDataPropertyValues: Map[api.SingletonInstanceStructuredDataPropertyValue, descriptions.SingletonInstanceStructuredDataPropertyValue] = Map.empty,
 singletonInstanceScalarDataPropertyValues: Map[api.SingletonInstanceScalarDataPropertyValue, descriptions.SingletonInstanceScalarDataPropertyValue] = Map.empty,
 structuredDataPropertyTuples: Map[api.StructuredDataPropertyTuple, descriptions.StructuredDataPropertyTuple] = Map.empty,
 scalarDataPropertyValues: Map[api.ScalarDataPropertyValue, descriptions.ScalarDataPropertyValue] = Map.empty,

 chainRules: Map[api.ChainRule, terminologies.ChainRule] = Map.empty,
 ruleBodySegments: Map[api.RuleBodySegment, terminologies.RuleBodySegment] = Map.empty,
 aspectPredicates: Map[api.AspectPredicate, terminologies.AspectPredicate] = Map.empty,
 conceptPredicates: Map[api.ConceptPredicate, terminologies.ConceptPredicate] = Map.empty,
 reifiedRelationshipPredicates: Map[api.ReifiedRelationshipPredicate, terminologies.ReifiedRelationshipPredicate] = Map.empty,
 reifiedRelationshipPropertyPredicates: Map[api.ReifiedRelationshipPropertyPredicate, terminologies.ReifiedRelationshipPropertyPredicate] = Map.empty,
 reifiedRelationshipInversePropertyPredicates: Map[api.ReifiedRelationshipInversePropertyPredicate, terminologies.ReifiedRelationshipInversePropertyPredicate] = Map.empty,
 reifiedRelationshipSourcePropertyPredicates: Map[api.ReifiedRelationshipSourcePropertyPredicate, terminologies.ReifiedRelationshipSourcePropertyPredicate] = Map.empty,
 reifiedRelationshipSourceInversePropertyPredicates: Map[api.ReifiedRelationshipSourceInversePropertyPredicate, terminologies.ReifiedRelationshipSourceInversePropertyPredicate] = Map.empty,
 reifiedRelationshipTargetPropertyPredicates: Map[api.ReifiedRelationshipTargetPropertyPredicate, terminologies.ReifiedRelationshipTargetPropertyPredicate] = Map.empty,
 reifiedRelationshipTargetInversePropertyPredicates: Map[api.ReifiedRelationshipTargetInversePropertyPredicate, terminologies.ReifiedRelationshipTargetInversePropertyPredicate] = Map.empty,
 unreifiedRelationshipPropertyPredicates: Map[api.UnreifiedRelationshipPropertyPredicate, terminologies.UnreifiedRelationshipPropertyPredicate] = Map.empty,
 unreifiedRelationshipInversePropertyPredicates: Map[api.UnreifiedRelationshipInversePropertyPredicate, terminologies.UnreifiedRelationshipInversePropertyPredicate] = Map.empty) {

  def moduleLookup(m: api.Module)
  : Option[common.Module]
  = m match {
    case g: api.TerminologyGraph => gs.get(g)
    case b: api.Bundle => bs.get(b)
    case d: api.DescriptionBox => ds.get(d)
  }

  def tboxLookup(iri: tables.taggedTypes.IRI)
  : Option[terminologies.TerminologyBox]
  = gs.values.find { g => g.iri() == iri } orElse bs.values.find { b => b.iri() == iri }

  def bundleLookup(iri: tables.taggedTypes.IRI)
  : Option[bundles.Bundle]
  = bs.values.find { b => b.iri() == iri }

  def dboxLookup(iri: tables.taggedTypes.IRI)
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

  def structuredDataPropertyContext(sdpt: api.SingletonInstanceStructuredDataPropertyContext)
  : Option[descriptions.SingletonInstanceStructuredDataPropertyContext]
  = sdpt match {
    case ac: api.SingletonInstanceStructuredDataPropertyValue =>
      singletonInstanceStructuredDataPropertyValues.get(ac)
    case ac: api.StructuredDataPropertyTuple =>
      structuredDataPropertyTuples.get(ac)
    }

  def elementLookup(e: api.Element)
  : Option[common.Element]
  = e match {
    case m: api.Module =>
      moduleLookup(m)

    case t: api.Entity =>
      entityLookup(t)
    case r: api.EntityRelationship =>
      entityRelationshipLookup(r)

    case d: api.DataRange =>
      dataRanges.get(d)
    case d: api.Structure =>
      structures.get(d)
    case l: api.ScalarOneOfLiteralAxiom =>
      scalarOneOfLiterals.get(l)

    case dp: api.DataRelationshipToStructure =>
      dataRelationshipToStructureLookup(dp)
    case dp: api.DataRelationshipToScalar =>
      dataRelationshipToScalarLookup(dp)

    case me: api.ModuleEdge =>
      moduleEdges.get(me)
    case ax: api.TermAxiom =>
      termAxioms.get(ax)
    case ax: api.ConceptTreeDisjunction =>
      conceptTreeDisjunctions.get(ax)
    case ax: api.DisjointUnionOfConceptsAxiom =>
      disjointUnionOfConceptAxioms.get(ax)
    case ax: api.ConceptualEntitySingletonInstance =>
      conceptualEntitySingletonInstances.get(ax)
    case ax: api.ReifiedRelationshipInstanceDomain =>
      reifiedRelationshipInstanceDomains.get(ax)
    case ax: api.ReifiedRelationshipInstanceRange =>
			reifiedRelationshipInstanceRanges.get(ax)
    case ax: api.UnreifiedRelationshipInstanceTuple =>
			unreifiedRelationshipInstanceTuples.get(ax)
    case ax: api.SingletonInstanceStructuredDataPropertyValue =>
			singletonInstanceStructuredDataPropertyValues.get(ax)
    case ax: api.SingletonInstanceScalarDataPropertyValue =>
			singletonInstanceScalarDataPropertyValues.get(ax)
    case ax: api.StructuredDataPropertyTuple =>
			structuredDataPropertyTuples.get(ax)
    case ax: api.ScalarDataPropertyValue =>
			scalarDataPropertyValues.get(ax)
    case ax: api.ChainRule =>
			chainRules.get(ax)
    case ax: api.RuleBodySegment =>
			ruleBodySegments.get(ax)
    case ax: api.AspectPredicate =>
			aspectPredicates.get(ax)
    case ax: api.ConceptPredicate =>
			conceptPredicates.get(ax)
    case ax: api.ReifiedRelationshipPredicate =>
			reifiedRelationshipPredicates.get(ax)
    case ax: api.ReifiedRelationshipPropertyPredicate =>
			reifiedRelationshipPropertyPredicates.get(ax)
    case ax: api.ReifiedRelationshipInversePropertyPredicate =>
			reifiedRelationshipInversePropertyPredicates.get(ax)
    case ax: api.ReifiedRelationshipSourcePropertyPredicate =>
			reifiedRelationshipSourcePropertyPredicates.get(ax)
    case ax: api.ReifiedRelationshipSourceInversePropertyPredicate =>
			reifiedRelationshipSourceInversePropertyPredicates.get(ax)
    case ax: api.ReifiedRelationshipTargetPropertyPredicate =>
			reifiedRelationshipTargetPropertyPredicates.get(ax)
    case ax: api.ReifiedRelationshipTargetInversePropertyPredicate =>
			reifiedRelationshipTargetInversePropertyPredicates.get(ax)
    case ax: api.UnreifiedRelationshipPropertyPredicate =>
			unreifiedRelationshipPropertyPredicates.get(ax)
    case ax: api.UnreifiedRelationshipInversePropertyPredicate =>
			unreifiedRelationshipInversePropertyPredicates.get(ax)
  }

}

object OMLResolver2Text {

  case class ConversionState
  (iri: tables.taggedTypes.IRI,
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
    (extent.terminologyGraphs.toList,
      extent.bundles.toList,
      extent.descriptionBoxes.toList) match {
      case (g :: Nil, Nil, Nil) =>
        g._2.iri.right
      case (Nil, b :: Nil, Nil) =>
        b._2.iri.right
      case (Nil, Nil, d :: Nil) =>
        d._2.iri.right
      case (gs, bs, ds) =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"OMLResolver2Text.convert: extent must have a single TerminologyGraph, Bundle or DescriptionBox: " +
            s" got ${gs.size} TerminologyGraphs, ${bs.size} Bundles, ${ds.size} DescriptionBoxes")).left
    }

    _ = java.lang.System.out.println(s"==> OMLResolver2Text converting: $iri")

    c00 <- ConversionState(
      iri,
      extent,
      common.CommonFactory.eINSTANCE.createExtent(),
      conversions).right[EMFProblems]


    // Modules
    c010 = c00.copy(conversions = c00.conversions.copy(mappings = c00.conversions.mappings :+ (iri -> (c00.extent -> c00.omlExtent))))
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
    c51 <- c50.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c50.right[EMFProblems])(convertDataPropertyRestrictionAxiom(extent))

    // Specializations

    c52 <- c51.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c51.right[EMFProblems])(convertSpecializationAxiom)

    // Disjunctions

    c60 <- c52.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c52.right[EMFProblems])(convertRootConceptTaxonomyAxiom)

    // ChainRule, RuleBodySegment, SegmentPredicates
    c70 <- c60.extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c60.right[EMFProblems])(convertChainRule)

    // ConceptualEntityInstances
    c80 <- c70.extent.descriptionBoxOfConceptInstance.foldLeft(c70.right[EMFProblems])(convertConceptInstance)
    c81 <- c80.extent.descriptionBoxOfReifiedRelationshipInstance.foldLeft(c80.right[EMFProblems])(convertReifiedRelationshipInstance)
    c82 <- c81.extent.descriptionBoxOfReifiedRelationshipInstanceDomain.foldLeft(c81.right[EMFProblems])(convertReifiedRelationshipInstanceDomain)
    c83 <- c82.extent.descriptionBoxOfReifiedRelationshipInstanceRange.foldLeft(c82.right[EMFProblems])(convertReifiedRelationshipInstanceRange)
    c84 <- c83.extent.descriptionBoxOfUnreifiedRelationshipInstanceTuple.foldLeft(c83.right[EMFProblems])(convertUnreifiedRelationshipInstanceTuple)

    // Data Property Values
    c90 <- c84.extent.descriptionBoxOfSingletonInstanceStructuredDataPropertyValue.foldLeft(c84.right[EMFProblems])(convertSingletonInstanceStructuredDataPropertyValue)
    c91 <- c90.extent.descriptionBoxOfSingletonInstanceScalarDataPropertyValue.foldLeft(c90.right[EMFProblems])(convertSingletonInstanceScalarDataPropertyValue)
    c92 <- c91.extent.singletonInstanceStructuredDataPropertyContextOfStructuredDataPropertyTuple.foldLeft(c91.right[EMFProblems])(convertStructuredDataPropertyTuple)
    c93 <- c92.extent.singletonInstanceStructuredDataPropertyContextOfScalarDataPropertyValue.foldLeft(c92.right[EMFProblems])(convertScalarDataPropertyValue)

    // Annotations
    c100 <- c93.extent.elementOfAnnotationPropertyValue.keys.foldLeft(c93.right[EMFProblems])(convertAnnotationPropertyValue)

    // Finished!
    cDone = c100

    result = cDone.conversions

    _ = java.lang.System.out.println(s"==> OMLResolver2Text  converted: $iri")
  } yield result

  def normalizeName(n: String): String = n

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
  = {
    case (acc, (_, g0)) =>
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
  = {
    case (acc, (_, b0)) =>
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
  = {
    case (acc, (_, d0)) =>
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
  = {
    case (acc, (_, ap0)) =>
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
            val next = r2t.copy(conversions =
              r2t.conversions.copy(scalarOneOfLiterals =
                r2t.conversions.scalarOneOfLiterals + (s0 -> s1))).right
            s0.valueType match {
              case Some(vt0) =>
                r2t.conversions.dataRanges.get(vt0) match {
                  case Some(vt1) =>
                    s1.setValueType(vt1)
                    next
                  case None =>
                    new EMFProblems(new java.lang.IllegalArgumentException(
                      s"convertScalarOneOfLiteralAxiom: Failed to resolve " +
                        s" value type: $vt0 " +
                        s" in tbox: $t0" +
                        s" for defining ScalarOneOfLiteralAxiom: $s0")).left[ConversionState]
                }
              case None =>
                next
            }

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

  protected def convertDataPropertyRestrictionAxiom
  (ext: api.Extent)
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
              s"convertDataPropertyRestrictionAxiom: Failed to resolve " +
                s"tbox: $t0" +
                s" for defining EntityScalarDataPropertyRestrictionAxiom: $er0")).left[ConversionState]
        }
      } yield upd
    case (acc, (er0: api.EntityStructuredDataPropertyParticularRestrictionAxiom, t0)) =>
      for {
        r2t1 <- acc
        upd <-
        (r2t1.conversions.tboxLookup(t0),
          r2t1.conversions.entityLookup(er0.restrictedEntity),
          r2t1.conversions.dataRelationshipToStructureLookup(er0.structuredDataProperty)) match {
          case (Some(t1), Some(e1), Some(dp1)) =>
            val er1 = terminologies.TerminologiesFactory.eINSTANCE.createEntityStructuredDataPropertyParticularRestrictionAxiom()
            er1.setTbox(t1)
            er1.setRestrictedEntity(e1)
            er1.setStructuredDataProperty(dp1)
            val r2t2 =
              r2t1.copy(conversions = r2t1.conversions.copy(termAxioms = r2t1.conversions.termAxioms + (er0 -> er1)))
            val next =
              convertRestrictionStructuredDataPropertyContext(ext, r2t2, Seq(er0 -> er1))
            next
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertDataPropertyRestrictionAxiom: Failed to resolve " +
                s"tbox: $t0" +
                s" for defining EntityStructuredDataPropertyParticularRestrictionAxiom: $er0")).left[ConversionState]
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  @scala.annotation.tailrec
  protected final def convertRestrictionStructuredDataPropertyContext
  (ext: api.Extent,
   r2t: ConversionState,
   cs: Seq[(api.RestrictionStructuredDataPropertyContext, terminologies.RestrictionStructuredDataPropertyContext)])
  : ConversionResult
  = if (cs.isEmpty)
    r2t.right
  else {
    val (c0, c1) = cs.head
    val values
    : EMFProblems \/ ConversionState
    = ext
      .restrictionStructuredDataPropertyContextOfRestrictionScalarDataPropertyValue
      .foldLeft(r2t.right[EMFProblems]) { case (acc, (vi, ci)) =>
        if (c0 != ci)
          acc
        else
          for {
            r2t1 <- acc
            dp <- r2t1.conversions.dataRelationshipToScalarLookup(vi.scalarDataProperty) match {
              case Some(dp1) =>
                dp1.right[EMFProblems]
              case None =>
                new EMFProblems(new java.lang.IllegalArgumentException(
                  s"convertRestrictionStructuredDataPropertyContext: failed to resolved scalar data property; ${vi.scalarDataProperty}"
                )).left
            }
            vt <- vi.valueType match {
              case Some(dt0) =>
                r2t1.conversions.dataRanges.get(dt0) match {
                  case Some(dt1) =>
                    Option(dt1).right[EMFProblems]
                  case None =>
                    new EMFProblems(new java.lang.IllegalArgumentException(
                      s"convertRestrictionStructuredDataPropertyContext: failed to resolved data range; $dt0"
                    )).left
                }
              case None =>
                Option.empty[terminologies.DataRange].right[EMFProblems]
            }
            vj = terminologies.TerminologiesFactory.eINSTANCE.createRestrictionScalarDataPropertyValue()
            _ = vj.setStructuredDataPropertyContext(c1)
            _ = vj.setScalarDataProperty(dp)
            _ = vj.setScalarPropertyValue(tables2emf(vi.scalarPropertyValue))
            _ = vt.foreach(vj.setValueType)
          } yield r2t1
      }

    val tuples
    : EMFProblems \/
      (ConversionState, Seq[(api.RestrictionStructuredDataPropertyContext, terminologies.RestrictionStructuredDataPropertyContext)])
    = ext
      .restrictionStructuredDataPropertyContextOfRestrictionStructuredDataPropertyTuple
      .foldLeft {
        values.map(_ -> cs.tail)
      } { case (acc, (ti, ci)) =>
        if (c0 != ci)
          acc
        else
          for {
            tuple <- acc
            (r2t1, queue) = tuple
            dp <- r2t1.conversions.dataRelationshipToStructureLookup(ti.structuredDataProperty) match {
              case Some(dp1) =>
                dp1.right[EMFProblems]
              case None =>
                new EMFProblems(new java.lang.IllegalArgumentException(
                  s"convertRestrictionStructuredDataPropertyContext: failed to resolved structured data property; ${ti.structuredDataProperty}"
                )).left
            }
            tj = terminologies.TerminologiesFactory.eINSTANCE.createRestrictionStructuredDataPropertyTuple()
            _ = tj.setStructuredDataPropertyContext(c1)
            _ = tj.setStructuredDataProperty(dp)
          } yield r2t1 -> (queue :+ (ti -> tj))
      }

    tuples match {
      case \/-((next, queue)) =>
        convertRestrictionStructuredDataPropertyContext(ext, next, queue)
      case -\/(errors) =>
        -\/(errors)
    }
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

  protected val convertChainRule
  : (ConversionResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (ax0: api.ChainRule, t0: api.TerminologyGraph)) =>
      for {
        r2t <- acc
        ax1 = terminologies.TerminologiesFactory.eINSTANCE.createChainRule()
        upd <- (
          r2t.conversions.gs.get(t0),
          r2t.conversions.unreifiedRelationships.get(ax0.head),
          r2t.extent.firstSegment.get(ax0)) match {
          case (
            Some(t1),
            Some(h1),
            Some(s0)) =>
            r2t.extent.predicate.get(s0) match {
              case Some(p0) =>
                ax1.setTbox(t1)
                ax1.setHead(h1)
                ax1.setName(ax0.name)
                val next = r2t.copy(conversions = r2t.conversions.copy(chainRules =
                  r2t.conversions.chainRules + (ax0 -> ax1)))
                convertRuleBodySegmentPredicate(next, s0, Some(ax1), None, p0)
              case _ =>
                new EMFProblems(new java.lang.IllegalArgumentException(
                  s"convertChainRule: Failed to resolve " +
                    s"tbox: $t0" +
                    s" unreified relationship: ${ax0.head}" +
                    s" first segment for $ax0")).left[ConversionState]
            }
          case (t1, h2, s0) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertChainRule: Failed to resolve $ax0\n" +
                s"tbox: $t0\n" +
                s"=> $t1\n" +
                s"unreified relationship: ${ax0.head}\n" +
                s"=> $h2\n" +
                s"first segment? $s0")).left[ConversionState]
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  protected def convertPredicate
  (r2t0: ConversionState,
   predicate: api.SegmentPredicate)
  : EMFProblems \/ (terminologies.SegmentPredicate,ConversionState)
  = predicate match {
    case p0: api.AspectPredicate =>
      val p1 = terminologies.TerminologiesFactory.eINSTANCE.createAspectPredicate()
      r2t0.conversions.aspects.get(p0.aspect) match {
        case Some(a1) =>
          p1.setAspect(a1)
          (p1, r2t0.copy(conversions = r2t0.conversions.copy(aspectPredicates =
            r2t0.conversions.aspectPredicates + (p0 -> p1)))).right
        case None =>
          new EMFProblems(new java.lang.IllegalArgumentException(
            s"convertRuleBodySegmentPredicate: Failed to resolve " +
              s"AspectPredicate: $p0")).left
      }
    case p0: api.ConceptPredicate =>
      val p1 = terminologies.TerminologiesFactory.eINSTANCE.createConceptPredicate()
      r2t0.conversions.concepts.get(p0.concept) match {
        case Some(c1) =>
          p1.setConcept(c1)
          (p1, r2t0.copy(conversions = r2t0.conversions.copy(conceptPredicates =
            r2t0.conversions.conceptPredicates + (p0 -> p1)))).right
        case None =>
          new EMFProblems(new java.lang.IllegalArgumentException(
            s"convertRuleBodySegmentPredicate: Failed to resolve " +
              s"ConceptPredicate: $p0")).left
      }
    case p0: api.ReifiedRelationshipPredicate =>
      val p1 = terminologies.TerminologiesFactory.eINSTANCE.createReifiedRelationshipPredicate()
      r2t0.conversions.reifiedRelationships.get(p0.reifiedRelationship) match {
        case Some(rr1) =>
          p1.setReifiedRelationship(rr1)
          (p1, r2t0.copy(conversions = r2t0.conversions.copy(reifiedRelationshipPredicates =
            r2t0.conversions.reifiedRelationshipPredicates + (p0 -> p1)))).right
        case None =>
          new EMFProblems(new java.lang.IllegalArgumentException(
            s"convertRuleBodySegmentPredicate: Failed to resolve " +
              s"ReifiedRelationshipPredicate: $p0")).left
      }
    case p0: api.ReifiedRelationshipPropertyPredicate =>
      val p1 = terminologies.TerminologiesFactory.eINSTANCE.createReifiedRelationshipPropertyPredicate()
      r2t0.conversions.reifiedRelationships.get(p0.reifiedRelationship) match {
        case Some(rr1) =>
          p1.setReifiedRelationship(rr1)
          (p1, r2t0.copy(conversions = r2t0.conversions.copy(reifiedRelationshipPropertyPredicates =
            r2t0.conversions.reifiedRelationshipPropertyPredicates + (p0 -> p1)))).right
        case None =>
          new EMFProblems(new java.lang.IllegalArgumentException(
            s"convertRuleBodySegmentPredicate: Failed to resolve " +
              s"ReifiedRelationshipPropertyPredicate: $p0")).left
      }
    case p0: api.ReifiedRelationshipInversePropertyPredicate =>
      val p1 = terminologies.TerminologiesFactory.eINSTANCE.createReifiedRelationshipInversePropertyPredicate()
      r2t0.conversions.reifiedRelationships.get(p0.reifiedRelationship) match {
        case Some(rr1) =>
          p1.setReifiedRelationship(rr1)
          (p1, r2t0.copy(conversions = r2t0.conversions.copy(reifiedRelationshipInversePropertyPredicates =
            r2t0.conversions.reifiedRelationshipInversePropertyPredicates + (p0 -> p1)))).right
        case None =>
          new EMFProblems(new java.lang.IllegalArgumentException(
            s"convertRuleBodySegmentPredicate: Failed to resolve " +
              s"ReifiedRelationshipInversePropertyPredicate: $p0")).left
      }
    case p0: api.ReifiedRelationshipSourcePropertyPredicate =>
      val p1 = terminologies.TerminologiesFactory.eINSTANCE.createReifiedRelationshipSourcePropertyPredicate()
      r2t0.conversions.reifiedRelationships.get(p0.reifiedRelationship) match {
        case Some(rr1) =>
          p1.setReifiedRelationship(rr1)
          (p1, r2t0.copy(conversions = r2t0.conversions.copy(reifiedRelationshipSourcePropertyPredicates =
            r2t0.conversions.reifiedRelationshipSourcePropertyPredicates + (p0 -> p1)))).right
        case None =>
          new EMFProblems(new java.lang.IllegalArgumentException(
            s"convertRuleBodySegmentPredicate: Failed to resolve " +
              s"ReifiedRelationshipSourcePropertyPredicate: $p0")).left
      }
    case p0: api.ReifiedRelationshipSourceInversePropertyPredicate =>
      val p1 = terminologies.TerminologiesFactory.eINSTANCE.createReifiedRelationshipSourceInversePropertyPredicate()
      r2t0.conversions.reifiedRelationships.get(p0.reifiedRelationship) match {
        case Some(rr1) =>
          p1.setReifiedRelationship(rr1)
          (p1, r2t0.copy(conversions = r2t0.conversions.copy(reifiedRelationshipSourceInversePropertyPredicates =
            r2t0.conversions.reifiedRelationshipSourceInversePropertyPredicates + (p0 -> p1)))).right
        case None =>
          new EMFProblems(new java.lang.IllegalArgumentException(
            s"convertRuleBodySegmentPredicate: Failed to resolve " +
              s"ReifiedRelationshipSourceInversePropertyPredicate: $p0")).left
      }
    case p0: api.ReifiedRelationshipTargetPropertyPredicate =>
      val p1 = terminologies.TerminologiesFactory.eINSTANCE.createReifiedRelationshipTargetPropertyPredicate()
      r2t0.conversions.reifiedRelationships.get(p0.reifiedRelationship) match {
        case Some(rr1) =>
          p1.setReifiedRelationship(rr1)
          (p1, r2t0.copy(conversions = r2t0.conversions.copy(reifiedRelationshipTargetPropertyPredicates =
            r2t0.conversions.reifiedRelationshipTargetPropertyPredicates + (p0 -> p1)))).right
        case None =>
          new EMFProblems(new java.lang.IllegalArgumentException(
            s"convertRuleBodySegmentPredicate: Failed to resolve " +
              s"ReifiedRelationshipTargetPropertyPredicate: $p0")).left
      }
    case p0: api.ReifiedRelationshipTargetInversePropertyPredicate =>
      val p1 = terminologies.TerminologiesFactory.eINSTANCE.createReifiedRelationshipTargetInversePropertyPredicate()
      r2t0.conversions.reifiedRelationships.get(p0.reifiedRelationship) match {
        case Some(rr1) =>
          p1.setReifiedRelationship(rr1)
          (p1, r2t0.copy(conversions = r2t0.conversions.copy(reifiedRelationshipTargetInversePropertyPredicates =
            r2t0.conversions.reifiedRelationshipTargetInversePropertyPredicates + (p0 -> p1)))).right
        case None =>
          new EMFProblems(new java.lang.IllegalArgumentException(
            s"convertRuleBodySegmentPredicate: Failed to resolve " +
              s"ReifiedRelationshipTargetInversePropertyPredicate: $p0")).left
      }
    case p0: api.UnreifiedRelationshipPropertyPredicate =>
      val p1 = terminologies.TerminologiesFactory.eINSTANCE.createUnreifiedRelationshipPropertyPredicate()
      r2t0.conversions.unreifiedRelationships.get(p0.unreifiedRelationship) match {
        case Some(ur1) =>
          p1.setUnreifiedRelationship(ur1)
          (p1, r2t0.copy(conversions = r2t0.conversions.copy(unreifiedRelationshipPropertyPredicates =
            r2t0.conversions.unreifiedRelationshipPropertyPredicates + (p0 -> p1)))).right
        case None =>
          new EMFProblems(new java.lang.IllegalArgumentException(
            s"convertRuleBodySegmentPredicate: Failed to resolve " +
              s"UnreifiedRelationshipPropertyPredicate: $p0")).left
      }
    case p0: api.UnreifiedRelationshipInversePropertyPredicate =>
      val p1 = terminologies.TerminologiesFactory.eINSTANCE.createUnreifiedRelationshipInversePropertyPredicate()
      r2t0.conversions.unreifiedRelationships.get(p0.unreifiedRelationship) match {
        case Some(ur1) =>
          p1.setUnreifiedRelationship(ur1)
          (p1, r2t0.copy(conversions = r2t0.conversions.copy(unreifiedRelationshipInversePropertyPredicates =
            r2t0.conversions.unreifiedRelationshipInversePropertyPredicates + (p0 -> p1)))).right
        case None =>
          new EMFProblems(new java.lang.IllegalArgumentException(
            s"convertRuleBodySegmentPredicate: Failed to resolve " +
              s"UnreifiedRelationshipInversePropertyPredicate: $p0")).left
      }
  }

  @scala.annotation.tailrec
  protected final def convertRuleBodySegmentPredicate
  (r2t0: ConversionState,
   s0: api.RuleBodySegment,
   rule: Option[terminologies.ChainRule],
   previousSegment: Option[terminologies.RuleBodySegment],
   predicate: api.SegmentPredicate)
  : ConversionResult
  = convertPredicate(r2t0, predicate) match {
    case \/-((p1, r2t1)) =>
      val s1 = terminologies.TerminologiesFactory.eINSTANCE.createRuleBodySegment()
      val r2t2 = r2t1.copy(conversions = r2t1.conversions.copy(ruleBodySegments =
        r2t1.conversions.ruleBodySegments + (s0 -> s1)))
      s1.setPredicate(p1)
      rule.foreach(s1.setRule)
      previousSegment.foreach(s1.setPreviousSegment)
      r2t2.extent.nextSegment.get(s0) match {
        case Some(n0) =>
          r2t2.extent.predicate.get(n0) match {
            case Some(p0) =>
              convertRuleBodySegmentPredicate(r2t2, n0, None, Some(s1), p0)
            case None =>
              new EMFProblems(new java.lang.IllegalArgumentException(
                s"convertRuleBodySegmentPredicate: Failed to resolve predicate $n0")).left
          }
        case None =>
          r2t2.right
      }
    case -\/(errors) =>
      -\/(errors)
  }

  protected val convertConceptInstance
  : (ConversionResult, (api.ConceptInstance, api.DescriptionBox)) => ConversionResult
  = {
    case (acc, (ci0, d0)) =>
      for {
        r2t <- acc
        ci1 = descriptions.DescriptionsFactory.eINSTANCE.createConceptInstance()
        upd <- (
          r2t.conversions.ds.get(d0),
          r2t.conversions.concepts.get(ci0.singletonConceptClassifier)) match {
          case (Some(d1), Some(c1)) =>
            ci1.setDescriptionBox(d1)
            ci1.setName(normalizeName(ci0.name))
            ci1.setSingletonConceptClassifier(c1)
            r2t.copy(conversions = r2t.conversions.copy(conceptualEntitySingletonInstances = r2t.conversions.conceptualEntitySingletonInstances + (ci0 -> ci1))).right
          case (d1, c1) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertConceptInstance: Failed to resolve " +
                d1.fold(s" dbox: ${d0.iri}")(_ => "") +
                c1.fold(s" concept: ${ci0.singletonConceptClassifier}")(_ => ""))
            ).left
        }
      } yield upd
  }

  protected val convertReifiedRelationshipInstance
  : (ConversionResult, (api.ReifiedRelationshipInstance, api.DescriptionBox)) => ConversionResult
  = {
    case (acc, (rri0, d0)) =>
      for {
        r2t <- acc
        rri1 = descriptions.DescriptionsFactory.eINSTANCE.createReifiedRelationshipInstance()
        upd <- (
          r2t.conversions.ds.get(d0),
          r2t.conversions.reifiedRelationships.get(rri0.singletonReifiedRelationshipClassifier)) match {
          case (Some(d1), Some(rr1)) =>
            rri1.setDescriptionBox(d1)
            rri1.setName(normalizeName(rri0.name))
            rri1.setSingletonReifiedRelationshipClassifier(rr1)
            r2t.copy(conversions = r2t.conversions.copy(conceptualEntitySingletonInstances = r2t.conversions.conceptualEntitySingletonInstances + (rri0 -> rri1))).right
          case (d1, rr1) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertReifiedRelationshipInstance: Failed to resolve " +
                d1.fold(s" dbox: ${d0.iri}")(_ => "") +
                rr1.fold(s" reified relationship: ${rri0.singletonReifiedRelationshipClassifier}")(_ => ""))
            ).left
        }
      } yield upd
  }

  protected val convertReifiedRelationshipInstanceDomain
  : (ConversionResult, (api.ReifiedRelationshipInstanceDomain, api.DescriptionBox)) => ConversionResult
  = {
    case (acc, (rrid0, d0)) =>
      for {
        r2t <- acc
        rrid1 = descriptions.DescriptionsFactory.eINSTANCE.createReifiedRelationshipInstanceDomain()
        upd <- (
          r2t.conversions.ds.get(d0),
          r2t.conversions.conceptualEntitySingletonInstances.get(rrid0.reifiedRelationshipInstance),
          r2t.conversions.conceptualEntitySingletonInstances.get(rrid0.domain)) match {
          case (Some(d1), Some(rri1: descriptions.ReifiedRelationshipInstance), Some(di1)) =>
            rrid1.setDescriptionBox(d1)
            rrid1.setReifiedRelationshipInstance(rri1)
            rrid1.setDomain(di1)
            r2t.copy(conversions = r2t.conversions.copy(reifiedRelationshipInstanceDomains = r2t.conversions.reifiedRelationshipInstanceDomains + (rrid0 -> rrid1))).right
          case (d1, rr1, di1) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertReifiedRelationshipInstanceDomain: Failed to resolve " +
                d1.fold(s" dbox: ${d0.iri}")(_ => "") +
                rr1.fold(s" reified relationship instance: ${rrid0.reifiedRelationshipInstance}")(_ => "") +
                di1.fold(s" domain: ${rrid0.domain}")(_ => ""))
            ).left
        }
      } yield upd
  }

  protected val convertReifiedRelationshipInstanceRange
  : (ConversionResult, (api.ReifiedRelationshipInstanceRange, api.DescriptionBox)) => ConversionResult
  = {
    case (acc, (rrid0, d0)) =>
      for {
        r2t <- acc
        rrid1 = descriptions.DescriptionsFactory.eINSTANCE.createReifiedRelationshipInstanceRange()
        upd <- (
          r2t.conversions.ds.get(d0),
          r2t.conversions.conceptualEntitySingletonInstances.get(rrid0.reifiedRelationshipInstance),
          r2t.conversions.conceptualEntitySingletonInstances.get(rrid0.range)) match {
          case (Some(d1), Some(rri1: descriptions.ReifiedRelationshipInstance), Some(di1)) =>
            rrid1.setDescriptionBox(d1)
            rrid1.setReifiedRelationshipInstance(rri1)
            rrid1.setRange(di1)
            r2t.copy(conversions = r2t.conversions.copy(reifiedRelationshipInstanceRanges = r2t.conversions.reifiedRelationshipInstanceRanges + (rrid0 -> rrid1))).right
          case (d1, rr1, di1) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertReifiedRelationshipInstanceRange: Failed to resolve " +
                d1.fold(s" dbox: ${d0.iri}")(_ => "") +
                rr1.fold(s" reified relationship instance: ${rrid0.reifiedRelationshipInstance}")(_ => "") +
                di1.fold(s" range: ${rrid0.range}")(_ => ""))
            ).left
        }
      } yield upd
  }

  protected val convertUnreifiedRelationshipInstanceTuple
  : (ConversionResult, (api.UnreifiedRelationshipInstanceTuple, api.DescriptionBox)) => ConversionResult
  = {
    case (acc, (urit0, d0)) =>
      for {
        r2t <- acc
        urit1 = descriptions.DescriptionsFactory.eINSTANCE.createUnreifiedRelationshipInstanceTuple()
        upd <- (
          r2t.conversions.ds.get(d0),
          r2t.conversions.unreifiedRelationships.get(urit0.unreifiedRelationship),
          r2t.conversions.conceptualEntitySingletonInstances.get(urit0.domain),
          r2t.conversions.conceptualEntitySingletonInstances.get(urit0.range)) match {
          case (Some(d1), Some(ur1), Some(di1), Some(ri1)) =>
            urit1.setDescriptionBox(d1)
            urit1.setUnreifiedRelationship(ur1)
            urit1.setDomain(di1)
            urit1.setRange(ri1)
            r2t.copy(conversions = r2t.conversions.copy(unreifiedRelationshipInstanceTuples = r2t.conversions.unreifiedRelationshipInstanceTuples + (urit0 -> urit1))).right
          case (d1, ur1, di1, ri1) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertUnreifiedRelationshipInstanceTuple: Failed to resolve " +
                d1.fold(s" dbox: ${d0.iri}")(_ => "") +
                ur1.fold(s" unreified relationship: ${urit0.unreifiedRelationship}")(_ => "") +
                di1.fold(s" domain: ${urit0.domain}")(_ => "")+
                ri1.fold(s" range: ${urit0.range}")(_ => ""))
            ).left
        }
      } yield upd
  }

  protected val convertSingletonInstanceStructuredDataPropertyValue
  : (ConversionResult, (api.SingletonInstanceStructuredDataPropertyValue, api.DescriptionBox)) => ConversionResult
  = {
    case (acc, (si0, d0)) =>
      for {
        r2t <- acc
        si1 = descriptions.DescriptionsFactory.eINSTANCE.createSingletonInstanceStructuredDataPropertyValue()
        upd <- (
          r2t.conversions.ds.get(d0),
          r2t.conversions.conceptualEntitySingletonInstances.get(si0.singletonInstance),
          r2t.conversions.dataRelationshipToStructureLookup(si0.structuredDataProperty)) match {
          case (Some(d1), Some(ce1), Some(sdp1)) =>
            si1.setDescriptionBox(d1)
            si1.setSingletonInstance(ce1)
            si1.setStructuredDataProperty(sdp1)
            r2t.copy(conversions = r2t.conversions.copy(singletonInstanceStructuredDataPropertyValues = r2t.conversions.singletonInstanceStructuredDataPropertyValues + (si0 -> si1))).right
          case (d1, ur1, di1) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertSingletonInstanceStructuredDataPropertyValue: Failed to resolve " +
                d1.fold(s" dbox: ${d0.iri}")(_ => "") +
                ur1.fold(s" singleton instance ${si0.singletonInstance}")(_ => "") +
                di1.fold(s" structuredDataProperty: ${si0.structuredDataProperty}")(_ => "")
            )).left
        }
      } yield upd
  }

  protected val convertSingletonInstanceScalarDataPropertyValue
  : (ConversionResult, (api.SingletonInstanceScalarDataPropertyValue, api.DescriptionBox)) => ConversionResult
  = {
    case (acc, (si0, d0)) =>
      for {
        r2t <- acc
        si1 = descriptions.DescriptionsFactory.eINSTANCE.createSingletonInstanceScalarDataPropertyValue()
        upd <- (
          r2t.conversions.ds.get(d0),
          r2t.conversions.conceptualEntitySingletonInstances.get(si0.singletonInstance),
          r2t.conversions.entityScalarDataProperties.get(si0.scalarDataProperty)) match {
          case (Some(d1), Some(ce1), Some(sdp1)) =>
            si1.setDescriptionBox(d1)
            si1.setSingletonInstance(ce1)
            si1.setScalarDataProperty(sdp1)
            r2t.copy(conversions = r2t.conversions.copy(singletonInstanceScalarDataPropertyValues = r2t.conversions.singletonInstanceScalarDataPropertyValues + (si0 -> si1))).right
          case (d1, ur1, di1) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertSingletonInstanceScalarDataPropertyValue: Failed to resolve " +
                d1.fold(s" dbox: ${d0.iri}")(_ => "") +
                ur1.fold(s" singleton instance ${si0.singletonInstance}")(_ => "") +
                di1.fold(s" scalarDataProperty: ${si0.scalarDataProperty}")(_ => "")
            )).left
        }
      } yield upd
  }

  protected val convertStructuredDataPropertyTuple
  : (ConversionResult, (api.StructuredDataPropertyTuple, api.SingletonInstanceStructuredDataPropertyContext)) => ConversionResult
  = {
    case (acc, (si0, ctx0)) =>
      for {
        r2t <- acc
        si1 = descriptions.DescriptionsFactory.eINSTANCE.createStructuredDataPropertyTuple()
        upd <- (
          r2t.conversions.structuredDataPropertyContext(ctx0),
          r2t.conversions.dataRelationshipToStructureLookup(si0.structuredDataProperty)) match {
          case (Some(ctx1), Some(sdp1)) =>
            si1.setStructuredDataPropertyContext(ctx1)
            si1.setStructuredDataProperty(sdp1)
            r2t.copy(conversions = r2t.conversions.copy(structuredDataPropertyTuples = r2t.conversions.structuredDataPropertyTuples + (si0 -> si1))).right
          case (ctx1, sdp1) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertStructuredDataPropertyTuple: Failed to resolve " +
                ctx1.fold(s" structuredDataPropertyContext: $ctx0")(_ => "") +
                sdp1.fold(s" structuredDataProperty ${si0.structuredDataProperty}")(_ => "")
            )).left
        }
      } yield upd
  }

  protected val convertScalarDataPropertyValue
  : (ConversionResult, (api.ScalarDataPropertyValue, api.SingletonInstanceStructuredDataPropertyContext)) => ConversionResult
  = {
    case (acc, (si0, ctx0)) =>
      for {
        r2t <- acc
        si1 = descriptions.DescriptionsFactory.eINSTANCE.createScalarDataPropertyValue()
        upd <- (
          r2t.conversions.structuredDataPropertyContext(ctx0),
          r2t.conversions.dataRelationshipToScalarLookup(si0.scalarDataProperty),
          si0.valueType,
          si0.valueType.flatMap(r2t.conversions.dataRanges.get)) match {
          case (Some(ctx1), Some(sdp1), Some(_), Some(vt1)) =>
            si1.setStructuredDataPropertyContext(ctx1)
            si1.setScalarDataProperty(sdp1)
            si1.setValueType(vt1)
            r2t.copy(conversions = r2t.conversions.copy(scalarDataPropertyValues = r2t.conversions.scalarDataPropertyValues + (si0 -> si1))).right
          case (Some(ctx1), Some(sdp1), None, None) =>
            si1.setStructuredDataPropertyContext(ctx1)
            si1.setScalarDataProperty(sdp1)
            r2t.copy(conversions = r2t.conversions.copy(scalarDataPropertyValues = r2t.conversions.scalarDataPropertyValues + (si0 -> si1))).right
          case (ctx1, sdp1, vt0, vt1) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertScalarDataPropertyValue: Failed to resolve " +
                ctx1.fold(s" structuredDataPropertyContext: $ctx0")(_ => "") +
                sdp1.fold(s" scalarDataProperty ${si0.scalarDataProperty}")(_ => "") +
                vt0.fold(s" no value type")(v0 => s" given value type: $v0") +
                vt1.fold(s" no converted value type")(v1 => s" converted value type: $v1")
            )).left
        }
      } yield upd
  }

  protected val convertAnnotationPropertyValue
  : (ConversionResult, api.AnnotationPropertyValue) => ConversionResult
  = {
    case (acc, apv0) =>
      for {
        r2t <- acc
        apv1 = common.CommonFactory.eINSTANCE.createAnnotationPropertyValue()
        _ <- (
          r2t.conversions.aps.get(apv0.property),
          r2t.conversions.elementLookup(apv0.subject)
        ) match {
          case (Some(ap1), Some(e1)) =>
            val lit = common.CommonFactory.eINSTANCE.createLiteralRawString()
            lit.setString(new datatypes.RawStringValue(apv0.value))
            apv1.setProperty(ap1)
            apv1.setSubject(e1)
            apv1.setValue(lit)
            ().right
          case (ap1, e1) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertAnnotationPropertyValue: Failed to resolve " +
                ap1.fold(s" anotation property: ${apv0.property}")(_ => "") +
                e1.fold(s" subject ${apv0.subject}")(_ => "")
            )).left
        }
      } yield r2t
  }
}