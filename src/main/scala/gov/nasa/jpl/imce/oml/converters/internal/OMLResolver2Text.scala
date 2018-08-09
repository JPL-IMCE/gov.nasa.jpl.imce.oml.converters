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

import java.lang.IllegalArgumentException
import java.util.UUID

import gov.nasa.jpl.imce.oml.converters.{ConversionCommand, tables2emf}
import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable.filterable
import gov.nasa.jpl.imce.oml.converters.utils.EMFProblems
import gov.nasa.jpl.imce.oml.model._
import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.tables.{ClosedWorldDesignations, OpenWorldDefinitions, TerminologyKind => TTerminologyKind}
import gov.nasa.jpl.imce.oml.tables.{Final, Partial, DescriptionKind => TDescriptionKind}
import org.eclipse.xtext.resource.XtextResourceSet

import scala.collection.immutable._
import scala.{Boolean, None, Option, Some, StringContext}
import scala.Predef.{ArrowAssoc, String, require}
import scalaz.Scalaz._
import scalaz._

case class OMLResolver2Text
(moduleExtents: Map[api.Module, api.Extent] = Map.empty,
 mappings: Seq[(tables.taggedTypes.IRI, (api.Module, common.Module))] = Seq.empty,

 queue_edges: Map[api.taggedTypes.ModuleEdgeUUID, (api.Extent,api.ModuleEdge)] = Map.empty,
 queue_elements: Map[api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement)] = Map.empty,

 // Modules
 gs: Map[api.TerminologyGraph, graphs.TerminologyGraph] = Map.empty,
 bs: Map[api.Bundle, bundles.Bundle] = Map.empty,
 ds: Map[api.DescriptionBox, descriptions.DescriptionBox] = Map.empty,

 aps: Map[api.AnnotationProperty, common.AnnotationProperty] = Map.empty,

 aspects: Map[api.AspectKind, terminologies.AspectKind] = Map.empty,
 concepts: Map[api.ConceptKind, terminologies.ConceptKind] = Map.empty,
 cardinalityRestrictedReifiedRelationships: Map[api.CardinalityRestrictedReifiedRelationship, terminologies.CardinalityRestrictedReifiedRelationship] = Map.empty,
 reifiedRelationshipRestrictions: Map[api.ReifiedRelationshipRestriction, terminologies.ReifiedRelationshipRestriction] = Map.empty,
 reifiedRelationships: Map[api.ReifiedRelationship, terminologies.ReifiedRelationship] = Map.empty,
 forwardProperties: Map[api.ForwardProperty, terminologies.ForwardProperty] = Map.empty,
 inverseProperties: Map[api.InverseProperty, terminologies.InverseProperty] = Map.empty,
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
 segmentPredicates: Map[api.SegmentPredicate, terminologies.SegmentPredicate] = Map.empty) {

  def isResolved: Boolean
  = queue_edges.isEmpty &&
    queue_elements.isEmpty

  def lookupMap[U <: api.LogicalElement, V <: api.LogicalElement]
  (u: U, uv: Map[U, V])
  (implicit ext: api.Extent)
  : EMFProblems \/ V
  = uv.get(u) match {
    case Some(u) =>
      u.right[EMFProblems]
    case _ =>
      new EMFProblems(new IllegalArgumentException(
        s"OMLResolver2Text.lookupMap failed for: $u")).left
  }

  def moduleLookup(m: api.Module)
  : Option[common.Module]
  = m match {
    case g: api.TerminologyGraph => gs.get(g)
    case b: api.Bundle => bs.get(b)
    case d: api.DescriptionBox => ds.get(d)
  }

  def getTbox(m0: api.TerminologyBox): EMFProblems \/ terminologies.TerminologyBox
  = (m0 match {
    case mt: api.TerminologyGraph =>
      gs.get(mt)
    case mb: api.Bundle =>
      bs.get(mb)
  }) match {
    case Some(m1) =>
      m1.right
    case None =>
      new EMFProblems(new java.lang.IllegalArgumentException(
        s"OWLResolver2Ontology.getTbox(${m0.kind} ${m0.iri}) not found."
      )).left
  }

  def getBundle(m0: api.Bundle): EMFProblems \/ bundles.Bundle
  = bs.get(m0) match {
    case Some(m1) =>
      m1.right
    case None =>
      new EMFProblems(new java.lang.IllegalArgumentException(
        s"OWLResolver2Ontology.getBundle(${m0.kind} ${m0.iri}) not found."
      )).left
  }

  def getDbox(m0: api.DescriptionBox): EMFProblems \/ descriptions.DescriptionBox
  = ds.get(m0) match {
    case Some(m1) =>
      m1.right
    case None =>
      new EMFProblems(new java.lang.IllegalArgumentException(
        s"OWLResolver2Ontology.getDbox(${m0.iri}) not found."
      )).left
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

  def graphLookup(m: api.TerminologyBox)
  : Option[graphs.TerminologyGraph]
  = m match {
    case g: api.TerminologyGraph => gs.get(g)
    case _ => None
  }

  def entityLookup(e: api.Entity)
  : Option[terminologies.Entity] = e match {
    case a: api.AspectKind =>
      aspects.get(a)
    case c: api.ConceptKind =>
      concepts.get(c)
    case crr: api.CardinalityRestrictedReifiedRelationship =>
      cardinalityRestrictedReifiedRelationships.get(crr)
    case rr: api.ReifiedRelationshipRestriction =>
      reifiedRelationshipRestrictions.get(rr)
    case rr: api.ReifiedRelationship =>
      reifiedRelationships.get(rr)
    case _ => None
  }

  def restrictableRelationshipLookup(rr: api.RestrictableRelationship)
  : Option[terminologies.RestrictableRelationship]
  = rr match {
    case r0: api.UnreifiedRelationship =>
      unreifiedRelationships.get(r0)
    case r0: api.ForwardProperty =>
      forwardProperties.get(r0)
    case r0: api.InverseProperty =>
      inverseProperties.get(r0)
  }

  def conceptualRelationshipLookup(rl: api.ConceptualRelationship)
  : Option[terminologies.ConceptualRelationship] = rl match {
    case rr: api.CardinalityRestrictedReifiedRelationship =>
      cardinalityRestrictedReifiedRelationships.get(rr)
    case rr: api.ReifiedRelationshipRestriction =>
      reifiedRelationshipRestrictions.get(rr)
    case rr: api.ReifiedRelationship =>
      reifiedRelationships.get(rr)
  }

  def entityRelationshipLookup(rl: api.EntityRelationship)
  : Option[terminologies.EntityRelationship]
  = rl match {
    case rr: api.CardinalityRestrictedReifiedRelationship =>
      cardinalityRestrictedReifiedRelationships.get(rr)
    case rr: api.ReifiedRelationshipRestriction =>
      reifiedRelationshipRestrictions.get(rr)
    case rr: api.ReifiedRelationship =>
      reifiedRelationships.get(rr)
    case ur: api.UnreifiedRelationship =>
      unreifiedRelationships.get(ur)
    case _ =>
      None
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

  def annotationPropertyLookup(ap0: api.AnnotationProperty)
  : Option[common.AnnotationProperty]
  = aps.get(ap0).orElse {
    mappings.foldLeft[Option[common.AnnotationProperty]](None) {
      case (Some(ap1), _) =>
        Some(ap1)
      case (None, (_: tables.taggedTypes.IRI, (e0: api.Extent, e1: common.Extent))) =>
        if (e0.moduleOfAnnotationProperty.contains(ap0)) {
          import scala.collection.JavaConverters.asScalaBufferConverter

          require(e1.getModules.size == 1)
          val ap1 = e1.getModules.get(0).getAnnotationProperties.asScala.find(_.getIri == ap0.iri)
          ap1
        } else
          None
    }
  }

  def elementLookup(e: api.LogicalElement)
  : Option[common.LogicalElement]
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
    case ax: api.SegmentPredicate =>
      segmentPredicates.get(ax)
  }

}

object OMLResolver2Text {

  type ConversionResult = EMFProblems \/ OMLResolver2Text

  def convert
  (extents: Seq[api.Extent],
   rs: XtextResourceSet,
   conversions: OMLResolver2Text)
  : ConversionResult
  = for {

    // Modules

    c011 <- extents.flatMap{ ext => ext.terminologyGraphs.map(ext -> _)}.foldLeft(conversions.right[EMFProblems])(convertTerminologyGraph)
    c012 <- extents.flatMap{ ext => ext.bundles.map(ext -> _)}.foldLeft(c011.right[EMFProblems])(convertBundle)
    c013 <- extents.flatMap{ ext => ext.descriptionBoxes.map(ext -> _)}.foldLeft(c012.right[EMFProblems])(convertDescription)
    c01N = c013

    // AnnotationProperties

    c020 = c01N
    c021 <- extents.flatMap(_.annotationProperties).foldLeft(c01N.right[EMFProblems])(convertModuleAnnotationProperties)
    c02N = c021

    // TerminologyExtensions

    c030 = c02N
    c031 <- c030.queue_edges.foldLeft(c030.right[EMFProblems])(convertTerminologyExtension)
    c03N = c031

    // Atomic Entities & DataTypes

    c040 = c03N
    c041 <- c040.queue_elements.foldLeft(c040.right[EMFProblems])(convertAspectOrConcept)
    c042 <- c041.queue_elements.foldLeft(c041.right[EMFProblems])(convertStructure)
    c043 <- c042.queue_elements.foldLeft(c042.right[EMFProblems])(convertScalar)
    c04N = c043

    // Data Ranges

    c050 = c04N
    c051 <- convertRestrictedDataRanges(
      c050.right[EMFProblems],
      extents.flatMap(_.terminologyBoxOfTerminologyBoxStatement.selectByKindOf { case (dr: api.RestrictedDataRange, t: api.TerminologyBox) => dr -> t }),
      List.empty)
    c052 <- c051.queue_elements.foldLeft(c051.right[EMFProblems])(convertScalarOneOfLiteralAxiom)
    c05N = c052

    // Relational constructs

    c060 = c05N
    c061 <- eliminationConverter1(c060.right)
    c06N = c061

//    // Other ModuleEdges
//
//    c20 <- extents.flatMap(_.terminologyBoxOfTerminologyBoxAxiom).foldLeft(c10.right[EMFProblems])(convertConceptDesignationTerminologyAxiom)
//    c21 <- extents.flatMap(_.terminologyBoxOfTerminologyBoxAxiom).foldLeft(c20.right[EMFProblems])(convertTerminologyNestingAxiom)
//    c22 <- extents.flatMap(_.bundleOfTerminologyBundleAxiom).foldLeft(c21.right[EMFProblems])(convertBundledTerminologyAxiom)
//    c23 <- extents.flatMap(_.descriptionBoxOfDescriptionBoxExtendsClosedWorldDefinitions).foldLeft(c22.right[EMFProblems])(convertDescriptionBoxExtendsClosedWorldDefinition)
//    c24 <- extents.flatMap(_.descriptionBoxOfDescriptionBoxRefinement).foldLeft(c23.right[EMFProblems])(convertDescriptionBoxRefinement)
//
//    // Relationships
//
//    c30 = c24
//    c31 <- convertReifiedRelationships(
//      c30.right[EMFProblems],
//      extents.flatMap(_.terminologyBoxOfTerminologyBoxStatement.selectByKindOf { case (rr: api.ReifiedRelationship, t: api.TerminologyBox) => rr -> t }),
//      List.empty)
//    c32 <- convertReifiedRelationshipRestrictions(
//      c31.right[EMFProblems],
//      extents.flatMap(_.terminologyBoxOfTerminologyBoxStatement.selectByKindOf { case (rr: api.ReifiedRelationshipRestriction, t: api.TerminologyBox) => rr -> t }),
//      List.empty)
//    c33 <- extents.flatMap(_.terminologyBoxOfTerminologyBoxStatement).foldLeft(c32.right[EMFProblems])(convertUnreifiedRelationship)

    // DataRelationships

    c40 = c06N
    c41 <- c40.queue_elements.foldLeft(c40.right[EMFProblems])(convertEntityScalarDataProperty)
    c42 <- c41.queue_elements.foldLeft(c41.right[EMFProblems])(convertEntityStructuredDataProperty)
    c43 <- c42.queue_elements.foldLeft(c42.right[EMFProblems])(convertScalarDataProperty)
    c44 <- c43.queue_elements.foldLeft(c43.right[EMFProblems])(convertStructuredDataProperty)

    // Restrictions
    c50 = c44
    c51 <- c50.queue_elements.foldLeft(c50.right[EMFProblems])(convertEntityRestrictionAxiom)
    c52 <- c51.queue_elements.foldLeft(c51.right[EMFProblems])(convertDataPropertyRestrictionAxiom)

    // Specializations

    c53 <- c52.queue_elements.foldLeft(c52.right[EMFProblems])(convertSpecializationAxiom)

    // Sub{Data|Object}PropertyOfAxioms

    c54 <- c53.queue_elements.foldLeft(c53.right[EMFProblems])(convertSubPropertyOfAxiom)

    // Disjunctions
    c60 = c54
    c61 <- c60.queue_elements.foldLeft(c60.right[EMFProblems])(convertRootConceptTaxonomyAxiom)

    // ChainRule, RuleBodySegment, SegmentPredicates
    c70 = c61
    c71 <- c70.queue_elements.foldLeft(c70.right[EMFProblems])(convertChainRule)

    // ConceptualEntityInstances
    c80 = c71
    c81 <- c80.queue_elements.foldLeft(c80.right[EMFProblems])(convertConceptInstance)
    c82 <- c81.queue_elements.foldLeft(c81.right[EMFProblems])(convertReifiedRelationshipInstance)
    c83 <- c82.queue_elements.foldLeft(c82.right[EMFProblems])(convertReifiedRelationshipInstanceDomain)
    c84 <- c83.queue_elements.foldLeft(c83.right[EMFProblems])(convertReifiedRelationshipInstanceRange)
    c85 <- c84.queue_elements.foldLeft(c84.right[EMFProblems])(convertUnreifiedRelationshipInstanceTuple)

    // Data Property Values
    c90 = c85
    c91 <- c90.queue_elements.foldLeft(c90.right[EMFProblems])(convertSingletonInstanceStructuredDataPropertyValue)
    c92 <- c91.queue_elements.foldLeft(c91.right[EMFProblems])(convertSingletonInstanceScalarDataPropertyValue)
    c93 <- c92.queue_elements.foldLeft(c92.right[EMFProblems])(convertStructuredDataPropertyTuple)
    c94 <- c93.queue_elements.foldLeft(c93.right[EMFProblems])(convertScalarDataPropertyValue)

    // Annotations
    c100 = c94
    c101 <- extents.flatMap(_.logicalElementOfAnnotationPropertyValue.keys).foldLeft(c100.right[EMFProblems])(convertAnnotationPropertyValue)

    // Finished!
    result = c101

  } yield result

  private def normalizeName(n: String): String = n

  // Module

  private def convertTerminologyKind(k: TTerminologyKind): terminologies.TerminologyKind = k match {
    case OpenWorldDefinitions => terminologies.TerminologyKind.OPEN_WORLD_DEFINITIONS
    case ClosedWorldDesignations => terminologies.TerminologyKind.CLOSED_WORLD_DESIGNATIONS
  }

  private def convertDescriptionKind(k: TDescriptionKind): descriptions.DescriptionKind = k match {
    case Final => descriptions.DescriptionKind.FINAL
    case Partial => descriptions.DescriptionKind.PARTIAL
  }

  private val convertTerminologyGraph
  : (ConversionResult, (api.Extent, (UUID, api.TerminologyGraph))) => ConversionResult
  = {
    case (acc, (extent, (_, g0))) =>
      for {
        r2t <- acc
        g1 = graphs.GraphsFactory.eINSTANCE.createTerminologyGraph()
        _ = g1.setKind(convertTerminologyKind(g0.kind))
        _ = g1.setIri(g0.iri)
        edges = g0.moduleEdges()(extent).to[Vector].map { e0 => e0.uuid -> (extent -> e0) }.toMap
        elements = g0.moduleElements()(extent).to[Vector].map { e0 => e0.uuid -> (extent -> e0) }.toMap
        next = r2t.copy(
          queue_edges = r2t.queue_edges ++ edges,
          queue_elements = r2t.queue_elements ++ elements,
          moduleExtents = r2t.moduleExtents + (g0 -> extent),
          mappings = r2t.mappings :+ (g0.iri -> (g0 -> g1)),
          gs = r2t.gs + (g0 -> g1))
      } yield next
  }

  private val convertBundle
  : (ConversionResult, (api.Extent,(UUID, api.Bundle))) => ConversionResult
  = {
    case (acc, (extent, (_, b0))) =>
      for {
        r2t <- acc
        b1 = bundles.BundlesFactory.eINSTANCE.createBundle()
        _ = b1.setKind(convertTerminologyKind(b0.kind))
        _ = b1.setIri(b0.iri)
        edges = b0.moduleEdges()(extent).to[Vector].map { e0 => e0.uuid -> (extent -> e0) }.toMap
        elements = b0.moduleElements()(extent).to[Vector].map { e0 => e0.uuid -> (extent -> e0) }.toMap
        next = r2t.copy(
          queue_edges = r2t.queue_edges ++ edges,
          queue_elements = r2t.queue_elements ++ elements,
          moduleExtents = r2t.moduleExtents + (b0 -> extent),
          mappings = r2t.mappings :+ (b0.iri -> (b0 -> b1)),
          bs = r2t.bs + (b0 -> b1))
      } yield next
  }

  private val convertDescription
  : (ConversionResult, (api.Extent,(UUID, api.DescriptionBox))) => ConversionResult
  = {
    case (acc, (extent, (_, d0))) =>
      for {
        r2t <- acc
        d1 = descriptions.DescriptionsFactory.eINSTANCE.createDescriptionBox()
        _ = d1.setKind(convertDescriptionKind(d0.kind))
        _ = d1.setIri(d0.iri)
        edges = d0.moduleEdges()(extent).to[Vector].map { e0 => e0.uuid -> (extent -> e0) }.toMap
        elements = d0.moduleElements()(extent).to[Vector].map { e0 => e0.uuid -> (extent -> e0) }.toMap
        next = r2t.copy(
          queue_edges = r2t.queue_edges ++ edges,
          queue_elements = r2t.queue_elements ++ elements,
          moduleExtents = r2t.moduleExtents + (d0 -> extent),
          mappings = r2t.mappings :+ (d0.iri -> (d0 -> d1)),
          ds = r2t.ds + (d0 -> d1))
      } yield next
  }

  // AnnotationProperty

  private val convertModuleAnnotationProperties
  : (ConversionResult, (api.Module, Set[api.AnnotationProperty])) => ConversionResult
  = {
    case (acc, (m0, aps0)) =>
      for {
        r2t <- acc
        next <- r2t.moduleLookup(m0) match {
          case Some(m1) =>
            val next = aps0.foldLeft(r2t) { case (prev, ap0) =>
              val ap1 = common.CommonFactory.eINSTANCE.createAnnotationProperty()
              ap1.setModule(m1)
              ap1.setAbbrevIRI(ap0.abbrevIRI)
              ap1.setIri(ap0.iri)
              prev.copy(aps = prev.aps + (ap0 -> ap1))
            }
            next.right
          case None =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertModuleAnnotationProperties: Failed to resolve " +
                s"module: $m0")).left
        }
      } yield next
  }

  // TerminologyExtensions

  private def convertTerminologyExtension
  (acc: ConversionResult, pair: (api.taggedTypes.ModuleEdgeUUID, (api.Extent, api.ModuleEdge)))
  : ConversionResult
  = pair match {
    case (uuid, (e0, ax0: api.TerminologyExtensionAxiom)) =>
      for {
        r2t <- acc
        ax1 = terminologies.TerminologiesFactory.eINSTANCE.createTerminologyExtensionAxiom()
        upd <- ( r2t.tboxLookup(ax0.tbox),
                 r2t.tboxLookup(ax0.extendedTerminology)) match {
          case (Some(t1), Some(e1)) =>
            ax1.setTbox(t1)
            ax1.setExtendedTerminology(e1)
            r2t.copy(
              queue_edges = r2t.queue_edges - uuid,
              moduleEdges = r2t.moduleEdges + (ax0 -> ax1)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertTerminologyExtensions: Failed to resolve " +
                s"extending tbox: ${ax0.tbox}" +
                s"extended tbox: ${ax0.extendedTerminology}")).left
        }
      } yield upd
    case _ =>
      acc
  }

  // Atomic Entities

  private def convertAspectOrConcept
  (prev: ConversionResult, pair: (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement)))
  : ConversionResult
  = pair match {
    case (uuid, (e0, a0: api.Aspect)) =>
      for {
        r2t <- prev
        t1 <- r2t.lookupMap(a0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2t.getTbox)
        a1 = terminologies.TerminologiesFactory.eINSTANCE.createAspect()
        _ = a1.setTbox(t1)
        _ = a1.setName(normalizeName(a0.name))
      } yield r2t.copy(
        queue_elements = r2t.queue_elements - uuid,
        aspects = r2t.aspects + (a0 -> a1))
    case (uuid, (e0, c0: api.Concept)) =>
      for {
        r2t <- prev
        t1 <- r2t.lookupMap(c0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2t.getTbox)
        c1 = terminologies.TerminologiesFactory.eINSTANCE.createConcept()
        _ = c1.setTbox(t1)
        _ = c1.setName(normalizeName(c0.name))
      } yield r2t.copy(
        queue_elements = r2t.queue_elements - uuid,
        concepts = r2t.concepts + (c0 -> c1))
    case _ =>
      prev
  }

  // Other ModuleEdges

  private val convertConceptDesignationTerminologyAxiom
  : (ConversionResult, (api.taggedTypes.ModuleEdgeUUID, (api.TerminologyBoxAxiom, api.TerminologyBox))) => ConversionResult
  = {
    case (acc, (uuid, (ax0: api.ConceptDesignationTerminologyAxiom, t0))) =>
      for {
        r2t <- acc
        ax1 = graphs.GraphsFactory.eINSTANCE.createConceptDesignationTerminologyAxiom()
        upd <- (r2t.tboxLookup(t0),
          r2t.tboxLookup(ax0.designatedTerminology),
          r2t.concepts.get(ax0.designatedConcept)) match {
          case (Some(t1), Some(dt1), Some(dc1)) =>
            ax1.setTbox(t1)
            ax1.setDesignatedTerminology(dt1)
            ax1.setDesignatedConcept(dc1)
            r2t.copy(
              queue_edges = r2t.queue_edges - uuid,
              moduleEdges = r2t.moduleEdges + (ax0 -> ax1)).right
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

  private val convertTerminologyNestingAxiom
  : (ConversionResult, (api.taggedTypes.ModuleEdgeUUID, (api.TerminologyBoxAxiom, api.TerminologyBox))) => ConversionResult
  = {
    case (acc, (uuid, (ax0: api.TerminologyNestingAxiom, t0))) =>
      for {
        r2t <- acc
        ax1 = graphs.GraphsFactory.eINSTANCE.createTerminologyNestingAxiom()
        upd <- (r2t.tboxLookup(t0),
          r2t.tboxLookup(ax0.nestingTerminology),
          r2t.concepts.get(ax0.nestingContext)) match {
          case (Some(t1), Some(nt1), Some(nc1)) =>
            ax1.setTbox(t1)
            ax1.setNestingTerminology(nt1)
            ax1.setNestingContext(nc1)
            r2t.copy(
              queue_edges = r2t.queue_edges - uuid,
              moduleEdges = r2t.moduleEdges + (ax0 -> ax1)).right
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

  private val convertBundledTerminologyAxiom
  : (ConversionResult, (api.taggedTypes.ModuleEdgeUUID, (api.TerminologyBundleAxiom, api.Bundle))) => ConversionResult
  = {
    case (acc, (uuid, (ax0: api.BundledTerminologyAxiom, b0))) =>
      for {
        r2t <- acc
        ax1 = bundles.BundlesFactory.eINSTANCE.createBundledTerminologyAxiom()
        upd <- (r2t.bs.get(b0), r2t.tboxLookup(ax0.bundledTerminology)) match {
          case (Some(b1), Some(bt1)) =>
            ax1.setBundle(b1)
            ax1.setBundledTerminology(bt1)
            r2t.copy(
              queue_edges = r2t.queue_edges - uuid,
              moduleEdges = r2t.moduleEdges + (ax0 -> ax1)).right
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

  private val convertDescriptionBoxExtendsClosedWorldDefinition
  : (ConversionResult, (api.taggedTypes.ModuleEdgeUUID, (api.DescriptionBoxExtendsClosedWorldDefinitions, api.DescriptionBox))) => ConversionResult
  = {
    case (acc, (uuid, (ax0, d0))) =>
      for {
        r2t <- acc
        ax1 = descriptions.DescriptionsFactory.eINSTANCE.createDescriptionBoxExtendsClosedWorldDefinitions()
        upd <- (r2t.ds.get(d0), r2t.tboxLookup(ax0.closedWorldDefinitions)) match {
          case (Some(d1), Some(cwt1)) =>
            ax1.setDescriptionBox(d1)
            ax1.setClosedWorldDefinitions(cwt1)
            r2t.copy(
              queue_edges = r2t.queue_edges - uuid,
              moduleEdges = r2t.moduleEdges + (ax0 -> ax1)).right
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

  private val convertDescriptionBoxRefinement
  : (ConversionResult, (api.taggedTypes.ModuleEdgeUUID, (api.DescriptionBoxRefinement, api.DescriptionBox))) => ConversionResult
  = {
    case (acc, (uuid, (ax0, d0))) =>
      for {
        r2t <- acc
        ax1 = descriptions.DescriptionsFactory.eINSTANCE.createDescriptionBoxRefinement()
        upd <- (r2t.ds.get(d0), r2t.dboxLookup(ax0.refinedDescriptionBox)) match {
          case (Some(d1), Some(rd1)) =>
            ax1.setRefiningDescriptionBox(d1)
            ax1.setRefinedDescriptionBox(rd1)
            r2t.copy(
              queue_edges = r2t.queue_edges - uuid,
              moduleEdges = r2t.moduleEdges + (ax0 -> ax1)).right
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

  private def convertReifiedRelationshipRestrictions
  (acc: ConversionResult,
   rrs: Iterable[(api.ReifiedRelationshipRestriction, api.TerminologyBox)],
   queue: List[(api.ReifiedRelationshipRestriction, api.TerminologyBox)],
   progress: Boolean = false)
  : ConversionResult
  = if (rrs.isEmpty) {
    if (queue.isEmpty)
      acc
    else if (progress)
      convertReifiedRelationshipRestrictions(acc, queue, List.empty)
    else
      new EMFProblems(new IllegalArgumentException(
        ConversionCommand.explainProblems(
          s"convertReifiedRelationshipRestrictions: no progress with ${queue.size} reified relationships in the queue",
          queue.map(_._2.name)))).left
  } else acc match {
    case \/-(r2t) =>
      val (rr0, t0) = rrs.head
      val ext0 = r2t.moduleExtents(t0)
      val ns0 = rr0.allNestedElements()(ext0)
      ( r2t.tboxLookup(t0),
        r2t.entityLookup(rr0.source),
        r2t.entityLookup(rr0.target) ) match {
        case (Some(t1), Some(rs1), Some(rt1)) =>
          val rr1 = terminologies.TerminologiesFactory.eINSTANCE.createReifiedRelationshipRestriction()

          rr1.setTbox(t1)
          rr1.setName(normalizeName(rr0.name))
          rr1.setSource(rs1)
          rr1.setTarget(rt1)

          val next = r2t.copy(
            reifiedRelationshipRestrictions = r2t.reifiedRelationshipRestrictions + (rr0 -> rr1)
          ).right

          convertReifiedRelationshipRestrictions(next, rrs.tail, queue, progress = true)

        case (Some(_), _, _) =>
          val rest = rrs.tail
          if (rest.isEmpty)
            convertReifiedRelationshipRestrictions(acc, rrs.head :: queue, List.empty, progress)
          else
            convertReifiedRelationshipRestrictions(acc, rest, rrs.head :: queue)

        case (None, _, _) =>
          new EMFProblems(new java.lang.IllegalArgumentException(
            s"convertReifiedRelationshipRestrictions: Failed to resolve tbox: $t0 for defining ReifiedRelationship: $rr0"
          )).left
      }

    case _ =>
      acc
  }

  private def convertReifiedRelationships
  (acc: ConversionResult,
   rrs: Iterable[(api.ReifiedRelationship, api.TerminologyBox)],
   queue: List[(api.ReifiedRelationship, api.TerminologyBox)],
   progress: Boolean = false)
  : ConversionResult
  = if (rrs.isEmpty) {
    if (queue.isEmpty)
      acc
    else if (progress)
      convertReifiedRelationships(acc, queue, List.empty)
    else
      new EMFProblems(new IllegalArgumentException(
        ConversionCommand.explainProblems(
          s"convertReifiedRelationships: no progress with ${queue.size} reified relationships in the queue",
          queue.map(_._2.name)))).left
  } else acc match {
    case \/-(r2t) =>
      val (rr0, t0) = rrs.head
      val ext0 = r2t.moduleExtents(t0)
      val ns0 = rr0.allNestedElements()(ext0)
      ( ns0.selectByKindOf{ case f: api.ForwardProperty => f }.headOption,
        ns0.selectByKindOf{ case i: api.InverseProperty => i }.headOption,
        r2t.tboxLookup(t0),
        r2t.entityLookup(rr0.source),
        r2t.entityLookup(rr0.target) ) match {
        case (Some(fwd0), inv0, Some(t1), Some(rs1), Some(rt1)) =>
          val rr1 = terminologies.TerminologiesFactory.eINSTANCE.createReifiedRelationship()
          val fwd1 = terminologies.TerminologiesFactory.eINSTANCE.createForwardProperty()
          val inv1 = inv0.map { i0 => i0 -> terminologies.TerminologiesFactory.eINSTANCE.createInverseProperty() }

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

          fwd1.setName(fwd0.name)
          fwd1.setReifiedRelationship(rr1)

          inv1.foreach { case (i0, i1) =>
            i1.setName(i0.name)
            i1.setReifiedRelationship(rr1)
          }

          rr1.setSource(rs1)
          rr1.setTarget(rt1)

          val next = r2t.copy(
            reifiedRelationships = r2t.reifiedRelationships + (rr0 -> rr1),
            forwardProperties = r2t.forwardProperties + (fwd0 -> fwd1),
            inverseProperties = r2t.inverseProperties ++ inv1
          ).right

          convertReifiedRelationships(next, rrs.tail, queue, progress = true)

        case (Some(_), _, Some(_), _, _) =>
          val rest = rrs.tail
          if (rest.isEmpty)
            convertReifiedRelationships(acc, rrs.head :: queue, List.empty, progress)
          else
            convertReifiedRelationships(acc, rest, rrs.head :: queue)

        case (Some(_), _, None, _, _) =>
          new EMFProblems(new java.lang.IllegalArgumentException(
            s"convertReifiedRelationship: Failed to resolve tbox: $t0 for defining ReifiedRelationship: $rr0"
          )).left

        case (None, _, _, _, _) =>
          new EMFProblems(new IllegalArgumentException(
            s"Missing ForwardProperty on ${rr0.abbrevIRI()(ext0)}"
          )).left
      }

    case _ =>
      acc
  }

  private val convertUnreifiedRelationship
  : (ConversionResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ConversionResult
  = {
    case (acc, (ur0: api.UnreifiedRelationship, t0)) =>
      for {
        r2t <- acc
        ur1 = terminologies.TerminologiesFactory.eINSTANCE.createUnreifiedRelationship()
        upd <- (r2t.tboxLookup(t0),
          r2t.entityLookup(ur0.source),
          r2t.entityLookup(ur0.target)) match {
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
            r2t.copy(unreifiedRelationships = r2t.unreifiedRelationships + (ur0 -> ur1)).right
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

  // Data Ranges

  private val convertStructure
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, s0: api.Structure))) =>
      for {
        r2t <- acc
        t1 <- r2t.lookupMap(s0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2t.getTbox)
        s1 = terminologies.TerminologiesFactory.eINSTANCE.createStructure()
        _ = s1.setTbox(t1)
        _ = s1.setName(normalizeName(s0.name))
      } yield r2t.copy(
        queue_elements = r2t.queue_elements - uuid,
        structures = r2t.structures + (s0 -> s1))
    case (acc, _) =>
      acc
  }

  private val convertScalar
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, s0: api.Scalar))) =>
      for {
        r2t <- acc
        t1 <- r2t.lookupMap(s0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2t.getTbox)
        s1 = terminologies.TerminologiesFactory.eINSTANCE.createScalar()
        _ = s1.setTbox(t1)
        _ = s1.setName(normalizeName(s0.name))
      } yield r2t.copy(
        queue_elements = r2t.queue_elements - uuid,
        dataRanges = r2t.dataRanges + (s0 -> s1))
    case (acc, _) =>
      acc
  }

  @scala.annotation.tailrec
  private def convertRestrictedDataRanges
  (acc: ConversionResult,
   drs: Iterable[(api.RestrictedDataRange, api.TerminologyBox)],
   queue: List[(api.RestrictedDataRange, api.TerminologyBox)],
   progress: Boolean = false)
  : ConversionResult
  = if (drs.isEmpty) {
    if (queue.isEmpty)
      acc
    else if (progress)
      convertRestrictedDataRanges(acc, queue, List.empty)
    else
      new EMFProblems(new java.lang.IllegalArgumentException(
        ConversionCommand.explainProblems(
          s"convertRestrictedDataRanges: no progress with ${queue.size} data ranges in the queue",
          queue.map(_._1.name)))).left
  }
  else
    acc match {
      case \/-(r2t) =>
        val (dr0, t0) = drs.head
        (r2t.tboxLookup(t0), r2t.dataRanges.get(dr0.restrictedRange)) match {
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
            val next = r2t.copy(dataRanges = r2t.dataRanges + (dr0 -> dr1)).right[EMFProblems]
            convertRestrictedDataRanges(next, drs.tail, queue, progress = true)
          case (Some(_), None) =>
            val rest = drs.tail
            if (rest.isEmpty)
              convertRestrictedDataRanges(acc, drs.head :: queue, List.empty, progress)
            else
              convertRestrictedDataRanges(acc, rest, drs.head :: queue)
          case (None, _) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRestrictedDataRanges: Failed to resolve " +
                s"tbox: $t0")).left
        }
      case _ =>
        acc
    }

  private val convertScalarOneOfLiteralAxiom
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, s0: api.ScalarOneOfLiteralAxiom))) =>
      for {
        r2t <- acc
        t1 <- r2t.lookupMap(s0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2t.getTbox)
        s1 = terminologies.TerminologiesFactory.eINSTANCE.createScalarOneOfLiteralAxiom()
        upd <- r2t.dataRanges.get(s0.axiom) match {
          case Some(a1: terminologies.ScalarOneOfRestriction) =>
            s1.setTbox(t1)
            s1.setAxiom(a1)
            s1.setValue(tables2emf(s0.value))
            val next = r2t.copy(
              queue_elements = r2t.queue_elements - uuid,
              scalarOneOfLiterals = r2t.scalarOneOfLiterals + (s0 -> s1)).right
            s0.valueType match {
              case Some(vt0) =>
                r2t.dataRanges.get(vt0) match {
                  case Some(vt1) =>
                    s1.setValueType(vt1)
                    next
                  case None =>
                    new EMFProblems(new java.lang.IllegalArgumentException(
                      s"convertScalarOneOfLiteralAxiom: Failed to resolve " +
                        s" value type: $vt0 " +
                        s" for defining ScalarOneOfLiteralAxiom: $s0")).left
                }
              case None =>
                next
            }

          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertScalarOneOfLiteralAxiom: Failed to resolve ScalarOneOfLiteralAxiom: $s0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  // Resolvers & Updaters

  implicit def toCardinalityRestrictionKind
  (k: tables.CardinalityRestrictionKind)
  : terminologies.CardinalityRestrictionKind
  = k match {
    case tables.MinCardinalityRestriction =>
      terminologies.CardinalityRestrictionKind.MIN
    case tables.MaxCardinalityRestriction =>
      terminologies.CardinalityRestrictionKind.MAX
    case tables.ExactCardinalityRestriction =>
      terminologies.CardinalityRestrictionKind.EXACT
  }

  case class ResolvableCardinalityAspectRestriction
  (uuid: api.taggedTypes.ModuleElementUUID,
   extent: api.Extent,
   cr: api.CardinalityRestrictedAspect,
   tbox: terminologies.TerminologyBox,
   rel: terminologies.RestrictableRelationship,
   range: Option[terminologies.Entity])

  private def resolvableCardinalityAspectRestrictions
  (r2t: OMLResolver2Text)
  : Iterable[ResolvableCardinalityAspectRestriction]
  = r2t
    .queue_elements
    .collect { case (uuid, (extent: api.Extent, x: api.CardinalityRestrictedAspect)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.terminologyBoxOfTerminologyBoxStatement.get(x).flatMap(r2t.tboxLookup),
        r2t.restrictableRelationshipLookup(x.restrictedRelationship),
        x.restrictedRange,
        x.restrictedRange.flatMap(r2t.entityLookup)) match {
        case (Some(tbox), Some(rel), Some(_), Some(range)) =>
          Some(ResolvableCardinalityAspectRestriction(uuid, extent, x, tbox, rel, Some(range)))
        case (Some(tbox), Some(rel), None, _) =>
          Some(ResolvableCardinalityAspectRestriction(uuid, extent, x, tbox, rel, None))
        case _ =>
          None
      }
    }

  private def updateCardinalityAspectRestrictions
  (current: ConversionResult,
   x: ResolvableCardinalityAspectRestriction)
  : ConversionResult
  = for {
    r2t <- current
    y = terminologies.TerminologiesFactory.eINSTANCE.createCardinalityRestrictedAspect()
    _ = y.setTbox(x.tbox)
    _ = y.setName(x.cr.name)
    _ = y.setRestrictionKind(x.cr.restrictionKind)
    _ = y.setRestrictedRelationship(x.rel)
    _ = x.range.foreach(y.setRestrictedRange)
    _ = y.setRestrictedCardinality(new datatypes.PositiveIntegerValue(x.cr.restrictedCardinality))
    next = r2t.copy(
      queue_elements = r2t.queue_elements - x.uuid,
      aspects = r2t.aspects + (x.cr -> y))
  } yield next

  case class ResolvableCardinalityConceptRestriction
  (uuid: api.taggedTypes.ModuleElementUUID,
   extent: api.Extent,
   cr: api.CardinalityRestrictedConcept,
   tbox: terminologies.TerminologyBox,
   rel: terminologies.RestrictableRelationship,
   range: Option[terminologies.Entity])

  private def resolvableCardinalityConceptRestrictions
  (r2t: OMLResolver2Text)
  : Iterable[ResolvableCardinalityConceptRestriction]
  = r2t
    .queue_elements
    .collect { case (uuid, (extent: api.Extent, x: api.CardinalityRestrictedConcept)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.terminologyBoxOfTerminologyBoxStatement.get(x).flatMap(r2t.tboxLookup),
        r2t.restrictableRelationshipLookup(x.restrictedRelationship),
        x.restrictedRange,
        x.restrictedRange.flatMap(r2t.entityLookup)) match {
        case (Some(tbox), Some(rel), Some(_), Some(range)) =>
          Some(ResolvableCardinalityConceptRestriction(uuid, extent, x, tbox, rel, Some(range)))
        case (Some(tbox), Some(rel), None, _) =>
          Some(ResolvableCardinalityConceptRestriction(uuid, extent, x, tbox, rel, None))
        case _ =>
          None
      }
    }

  private def updateCardinalityConceptRestrictions
  (current: ConversionResult,
   x: ResolvableCardinalityConceptRestriction)
  : ConversionResult
  = for {
    r2t <- current
    y = terminologies.TerminologiesFactory.eINSTANCE.createCardinalityRestrictedConcept()
    _ = y.setTbox(x.tbox)
    _ = y.setName(x.cr.name)
    _ = y.setRestrictionKind(x.cr.restrictionKind)
    _ = y.setRestrictedRelationship(x.rel)
    _ = x.range.foreach(y.setRestrictedRange)
    _ = y.setRestrictedCardinality(new datatypes.PositiveIntegerValue(x.cr.restrictedCardinality))
    next = r2t.copy(
      queue_elements = r2t.queue_elements - x.uuid,
      concepts = r2t.concepts + (x.cr -> y))
  } yield next

  case class ResolvableCardinalityReifiedRelationshipRestriction
  (uuid: api.taggedTypes.ModuleElementUUID,
   extent: api.Extent,
   cr: api.CardinalityRestrictedReifiedRelationship,
   tbox: terminologies.TerminologyBox,
   rel: terminologies.RestrictableRelationship,
   range: Option[terminologies.Entity])

  private def resolvableCardinalityReifiedRelationshipRestrictions
  (r2t: OMLResolver2Text)
  : Iterable[ResolvableCardinalityReifiedRelationshipRestriction]
  = r2t
    .queue_elements
    .collect { case (uuid, (extent: api.Extent, x: api.CardinalityRestrictedReifiedRelationship)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.terminologyBoxOfTerminologyBoxStatement.get(x).flatMap(r2t.tboxLookup),
        r2t.restrictableRelationshipLookup(x.restrictedRelationship),
        x.restrictedRange,
        x.restrictedRange.flatMap(r2t.entityLookup) ) match {
        case (Some(tbox), Some(rel), Some(_), Some(range)) =>
          Some(ResolvableCardinalityReifiedRelationshipRestriction(uuid, extent, x, tbox, rel, Some(range)))
        case (Some(tbox), Some(rel), None, _) =>
          Some(ResolvableCardinalityReifiedRelationshipRestriction(uuid, extent, x, tbox, rel, None))
        case _ =>
          None
      }
    }

  private def updateCardinalityReifiedRelationshipRestrictions
  (current: ConversionResult,
   x: ResolvableCardinalityReifiedRelationshipRestriction)
  : ConversionResult
  = for {
    r2t <- current
    y = terminologies.TerminologiesFactory.eINSTANCE.createCardinalityRestrictedReifiedRelationship
    _ = y.setTbox(x.tbox)
    _ = y.setName(x.cr.name)
    _ = y.setRestrictionKind(x.cr.restrictionKind)
    _ = y.setRestrictedRelationship(x.rel)
    _ = x.range.foreach(y.setRestrictedRange)
    _ = y.setRestrictedCardinality(new datatypes.PositiveIntegerValue(x.cr.restrictedCardinality))
    next = r2t.copy(
      queue_elements = r2t.queue_elements - x.uuid,
      cardinalityRestrictedReifiedRelationships = r2t.cardinalityRestrictedReifiedRelationships + (x.cr -> y))
  } yield next

  case class ResolvableConceptDesignationTerminologyAxiom
  (uuid: api.taggedTypes.ModuleEdgeUUID,
   extent: api.Extent,
   ax: api.ConceptDesignationTerminologyAxiom,
   tbox: terminologies.TerminologyBox,
   c: terminologies.ConceptKind,
   desTbox: terminologies.TerminologyBox)

  private def resolvableConceptDesignationTerminologyAxioms
  (r2t: OMLResolver2Text)
  : Iterable[ResolvableConceptDesignationTerminologyAxiom]
  = r2t
    .queue_edges
    .collect { case (uuid, (extent: api.Extent, x: api.ConceptDesignationTerminologyAxiom)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.terminologyBoxOfTerminologyBoxAxiom.get(x).flatMap(r2t.tboxLookup),
        r2t.concepts.get(x.designatedConcept),
        r2t.tboxLookup(x.designatedTerminology)) match {
        case (Some(tbox), Some(c), Some(desTbox)) =>
          Some(ResolvableConceptDesignationTerminologyAxiom(uuid, extent, x, tbox, c, desTbox))
        case _ =>
          None
      }
    }

  private def updateConceptDesignationTerminologyAxioms
  (current: ConversionResult,
   x: ResolvableConceptDesignationTerminologyAxiom)
  : ConversionResult
  = for {
    r2t <- current
    y = graphs.GraphsFactory.eINSTANCE.createConceptDesignationTerminologyAxiom
    _ = y.setTbox(x.tbox)
    _ = y.setDesignatedConcept(x.c)
    _ = y.setDesignatedTerminology(x.desTbox)
    next = r2t.copy(
      queue_edges = r2t.queue_edges - x.uuid)
  } yield next

  case class ResolvableTerminologyNestingAxiom
  (uuid: api.taggedTypes.ModuleEdgeUUID,
   extent: api.Extent,
   ax: api.TerminologyNestingAxiom,
   tg: graphs.TerminologyGraph,
   nestingC: terminologies.ConceptKind,
   nestingTbox: terminologies.TerminologyBox)

  private def resolvableTerminologyNestingAxioms
  (r2t: OMLResolver2Text)
  : Iterable[ResolvableTerminologyNestingAxiom]
  = r2t
    .queue_edges
    .collect { case (uuid, (extent: api.Extent, x: api.TerminologyNestingAxiom)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.terminologyBoxOfTerminologyBoxAxiom.get(x).flatMap(r2t.graphLookup),
        r2t.concepts.get(x.nestingContext),
        r2t.tboxLookup(x.nestingTerminology)) match {
        case (Some(tg), Some(c), Some(nestingTbox)) =>
          Some(ResolvableTerminologyNestingAxiom(uuid, extent, x, tg, c, nestingTbox))
        case _ =>
          None
      }
    }

  private def updateTerminologyNestingAxioms
  (current: ConversionResult,
   x: ResolvableTerminologyNestingAxiom)
  : ConversionResult
  = for {
    r2t <- current
    y = graphs.GraphsFactory.eINSTANCE.createTerminologyNestingAxiom()
    _ = y.setTbox(x.nestingTbox)
    _ = y.setNestingContext(x.nestingC)
    _ = y.setNestingTerminology(x.tg)
    next = r2t.copy(
      queue_edges = r2t.queue_edges - x.uuid)
  } yield next

  case class ResolvableBundledTerminologyAxiom
  (uuid: api.taggedTypes.ModuleEdgeUUID,
   extent: api.Extent,
   ax: api.BundledTerminologyAxiom,
   bundle: bundles.Bundle,
   bundledTbox: terminologies.TerminologyBox)

  private def resolvableBundledTerminologyAxioms
  (r2t: OMLResolver2Text)
  : Iterable[ResolvableBundledTerminologyAxiom]
  = r2t
    .queue_edges
    .collect { case (uuid, (extent: api.Extent, x: api.BundledTerminologyAxiom)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.bundleOfTerminologyBundleAxiom.get(x).flatMap(r2t.bs.get),
        r2t.tboxLookup(x.bundledTerminology)) match {
        case (Some(bundle), Some(bundledTbox)) =>
          Some(ResolvableBundledTerminologyAxiom(uuid, extent, x, bundle, bundledTbox))
        case _ =>
          None
      }
    }

  private def updateBundledTerminologyAxioms
  (current: ConversionResult,
   x: ResolvableBundledTerminologyAxiom)
  : ConversionResult
  = for {
    r2t <- current
    y = bundles.BundlesFactory.eINSTANCE.createBundledTerminologyAxiom() 
    _ = y.setBundle(x.bundle)
    _ = y.setBundledTerminology(x.bundledTbox)
    next = r2t.copy(
      queue_edges = r2t.queue_edges - x.uuid)
  } yield next

  case class ResolvableDescriptionBoxExtendsClosedWorldDefinitions
  (uuid: api.taggedTypes.ModuleEdgeUUID,
   extent: api.Extent,
   ax: api.DescriptionBoxExtendsClosedWorldDefinitions,
   dbox: descriptions.DescriptionBox,
   tbox: terminologies.TerminologyBox)

  private def resolvableDescriptionBoxExtendsClosedWorldDefinitions
  (r2t: OMLResolver2Text)
  : Iterable[ResolvableDescriptionBoxExtendsClosedWorldDefinitions]
  = r2t
    .queue_edges
    .collect { case (uuid, (extent: api.Extent, x: api.DescriptionBoxExtendsClosedWorldDefinitions)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.descriptionBoxOfDescriptionBoxExtendsClosedWorldDefinitions.get(x).flatMap(r2t.ds.get),
        r2t.tboxLookup(x.closedWorldDefinitions)) match {
        case (Some(dbox), Some(tbox)) =>
          Some(ResolvableDescriptionBoxExtendsClosedWorldDefinitions(uuid, extent, x, dbox, tbox))
        case _ =>
          None
      }
    }

  private def updateDescriptionBoxExtendsClosedWorldDefinitions
  (current: ConversionResult,
   x: ResolvableDescriptionBoxExtendsClosedWorldDefinitions)
  : ConversionResult
  = for {
    r2t <- current
    y = descriptions.DescriptionsFactory.eINSTANCE.createDescriptionBoxExtendsClosedWorldDefinitions()
    _ = y.setDescriptionBox(x.dbox)
    _ = y.setClosedWorldDefinitions(x.tbox)
    next = r2t.copy(
      queue_edges = r2t.queue_edges - x.uuid)
  } yield next

  case class ResolvableDescriptionBoxRefinement
  (uuid: api.taggedTypes.ModuleEdgeUUID,
   extent: api.Extent,
   ax: api.DescriptionBoxRefinement,
   dbox: descriptions.DescriptionBox,
   refined: descriptions.DescriptionBox)

  private def resolvableDescriptionBoxRefinements
  (r2t: OMLResolver2Text)
  : Iterable[ResolvableDescriptionBoxRefinement]
  = r2t
    .queue_edges
    .collect { case (uuid, (extent: api.Extent, x: api.DescriptionBoxRefinement)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.descriptionBoxOfDescriptionBoxRefinement.get(x).flatMap(r2t.ds.get),
        r2t.dboxLookup(x.refinedDescriptionBox)) match {
        case (Some(dbox), Some(refined)) =>
          Some(ResolvableDescriptionBoxRefinement(uuid, extent, x, dbox, refined))
        case _ =>
          None
      }
    }

  private def updateDescriptionBoxRefinements
  (current: ConversionResult,
   x: ResolvableDescriptionBoxRefinement)
  : ConversionResult
  = for {
    r2t <- current
    y = descriptions.DescriptionsFactory.eINSTANCE.createDescriptionBoxRefinement()
    _ = y.setRefiningDescriptionBox(x.dbox)
    _ = y.setRefinedDescriptionBox(x.refined)
    next = r2t.copy(
      queue_edges = r2t.queue_edges - x.uuid)
  } yield next

  case class ResolvableReifiedRelationship
  (uuid: api.taggedTypes.ModuleElementUUID,
   extent: api.Extent,
   rr: api.ReifiedRelationship,
   tbox: terminologies.TerminologyBox,
   source: terminologies.Entity,
   target: terminologies.Entity,
   fwd: api.ForwardProperty,
   inv: Option[api.InverseProperty])

  private def resolvableReifiedRelationships
  (r2t: OMLResolver2Text)
  : Iterable[ResolvableReifiedRelationship]
  = r2t
    .queue_elements
    .collect { case (uuid, (extent: api.Extent, x: api.ReifiedRelationship)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.terminologyBoxOfTerminologyBoxStatement.get(x).flatMap(r2t.tboxLookup),
        r2t.entityLookup(x.source),
        r2t.entityLookup(x.target),
        x.allNestedElements()(extent).collectFirst { case f: api.ForwardProperty => f },
        x.allNestedElements()(extent).collectFirst { case i: api.InverseProperty => i } ) match {
        case (Some(tbox), Some(source), Some(target), Some(fwd), inv) =>
          Some(ResolvableReifiedRelationship(uuid, extent, x, tbox, source, target, fwd, inv))
        case _ =>
          None
      }
    }

  private def updateReifiedRelationships
  (current: ConversionResult,
   x: ResolvableReifiedRelationship)
  : ConversionResult
  = for {
    r2t <- current
    y = terminologies.TerminologiesFactory.eINSTANCE.createReifiedRelationship()
    _ = y.setTbox(x.tbox)
    _ = y.setName(x.rr.name)
    _ = y.setSource(x.source)
    _ = y.setTarget(x.target)
    fwd1 = terminologies.TerminologiesFactory.eINSTANCE.createForwardProperty()
    _ = fwd1.setName(x.fwd.name)
    _ = fwd1.setReifiedRelationship(y)
    invPair = x.inv.map { i0 =>
      val i1 = terminologies.TerminologiesFactory.eINSTANCE.createInverseProperty()
      i1.setName(i0.name)
      i1.setReifiedRelationship(y)
      i0 -> i1
    }
    _ = y.setIsAsymmetric(x.rr.isAsymmetric)
    _ = y.setIsEssential(x.rr.isEssential)
    _ = y.setIsFunctional(x.rr.isFunctional)
    _ = y.setIsInverseEssential(x.rr.isInverseEssential)
    _ = y.setIsInverseFunctional(x.rr.isInverseFunctional)
    _ = y.setIsIrreflexive(x.rr.isIrreflexive)
    _ = y.setIsReflexive(x.rr.isReflexive)
    _ = y.setIsSymmetric(x.rr.isSymmetric)
    _ = y.setIsTransitive(x.rr.isTransitive)
    next = r2t.copy(
      queue_elements = r2t.queue_elements - x.uuid,
      reifiedRelationships = r2t.reifiedRelationships + (x.rr -> y),
      forwardProperties = r2t.forwardProperties + (x.fwd -> fwd1),
      inverseProperties = r2t.inverseProperties ++ invPair)
  } yield next

  case class ResolvableReifiedRelationshipRestriction
  (uuid: api.taggedTypes.ModuleElementUUID,
   extent: api.Extent,
   rrr: api.ReifiedRelationshipRestriction,
   tbox: terminologies.TerminologyBox,
   source: terminologies.Entity,
   target: terminologies.Entity)

  private def resolvableReifiedRelationshipRestrictions
  (r2t: OMLResolver2Text)
  : Iterable[ResolvableReifiedRelationshipRestriction]
  = r2t
    .queue_elements
    .collect { case (uuid, (extent: api.Extent, x: api.ReifiedRelationshipRestriction)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.terminologyBoxOfTerminologyBoxStatement.get(x).flatMap(r2t.tboxLookup),
        r2t.entityLookup(x.source),
        r2t.entityLookup(x.target) ) match {
        case (Some(tbox), Some(source), Some(target)) =>
          Some(ResolvableReifiedRelationshipRestriction(uuid, extent, x, tbox, source, target))
        case _ =>
          None
      }
    }

  private def updateReifiedRelationshipRestrictions
  (current: ConversionResult,
   x: ResolvableReifiedRelationshipRestriction)
  : ConversionResult
  = for {
    r2t <- current
    y = terminologies.TerminologiesFactory.eINSTANCE.createReifiedRelationshipRestriction()
    _ = y.setTbox(x.tbox)
    _ = y.setName(x.rrr.name)
    _ = y.setSource(x.source)
    _ = y.setTarget(x.target)
    next = r2t.copy(
      queue_elements = r2t.queue_elements - x.uuid,
      reifiedRelationshipRestrictions = r2t.reifiedRelationshipRestrictions + (x.rrr -> y))
  } yield next
  
  case class ResolvableUnreifiedRelationship
  (uuid: api.taggedTypes.ModuleElementUUID,
   extent: api.Extent,
   ur: api.UnreifiedRelationship,
   tbox: terminologies.TerminologyBox,
   source: terminologies.Entity,
   target: terminologies.Entity)

  private def resolvableUnreifiedRelationships
  (r2t: OMLResolver2Text)
  : Iterable[ResolvableUnreifiedRelationship]
  = r2t
    .queue_elements
    .collect { case (uuid, (extent: api.Extent, x: api.UnreifiedRelationship)) => (uuid, extent, x) }
    .flatMap { case (uuid, extent, x) =>
      ( extent.terminologyBoxOfTerminologyBoxStatement.get(x).flatMap(r2t.tboxLookup),
        r2t.entityLookup(x.source),
        r2t.entityLookup(x.target) ) match {
        case (Some(tbox), Some(source), Some(target)) =>
          Some(ResolvableUnreifiedRelationship(uuid, extent, x, tbox, source, target))
        case _ =>
          None
      }
    }

  private def updateUnreifiedRelationships
  (current: ConversionResult,
   x: ResolvableUnreifiedRelationship)
  : ConversionResult
  = for {
    r2t <- current
    y = terminologies.TerminologiesFactory.eINSTANCE.createUnreifiedRelationship()
    _ = y.setTbox(x.tbox)
    _ = y.setName(x.ur.name)
    _ = y.setSource(x.source)
    _ = y.setTarget(x.target)

    _ = y.setIsAsymmetric(x.ur.isAsymmetric)
    _ = y.setIsEssential(x.ur.isEssential)
    _ = y.setIsFunctional(x.ur.isFunctional)
    _ = y.setIsInverseEssential(x.ur.isInverseEssential)
    _ = y.setIsInverseFunctional(x.ur.isInverseFunctional)
    _ = y.setIsIrreflexive(x.ur.isIrreflexive)
    _ = y.setIsReflexive(x.ur.isReflexive)
    _ = y.setIsTransitive(x.ur.isTransitive)

    next = r2t.copy(
      queue_elements = r2t.queue_elements - x.uuid,
      unreifiedRelationships = r2t.unreifiedRelationships + (x.ur -> y))
  } yield next

  // Relational Constructs

  private def eliminationConverter1
  (c0: ConversionResult)
  : ConversionResult
  = c0 match {
    case \/-(r2t) =>
      if (r2t.isResolved)
        \/-(r2t)
      else {
        val r1 = resolvableCardinalityAspectRestrictions(r2t)
        val r2 = resolvableCardinalityConceptRestrictions(r2t)
        val r3 = resolvableCardinalityReifiedRelationshipRestrictions(r2t)
        val r4 = resolvableConceptDesignationTerminologyAxioms(r2t)
        val r5 = resolvableTerminologyNestingAxioms(r2t)
        val r6 = resolvableBundledTerminologyAxioms(r2t)
        val r7 = resolvableDescriptionBoxExtendsClosedWorldDefinitions(r2t)
        val r8 = resolvableDescriptionBoxRefinements(r2t)
        val r9 = resolvableReifiedRelationships(r2t)
        val rA = resolvableUnreifiedRelationships(r2t)
        val rB = resolvableReifiedRelationshipRestrictions(r2t)

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

  private val convertEntityScalarDataProperty
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, dp0: api.EntityScalarDataProperty))) =>
      for {
        r2t <- acc
        t1 <- r2t.lookupMap(dp0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2t.getTbox)
        dp1 = terminologies.TerminologiesFactory.eINSTANCE.createEntityScalarDataProperty()
        upd <- (r2t.entityLookup(dp0.domain), r2t.dataRanges.get(dp0.range)) match {
          case (Some(e1), Some(r1)) =>
            dp1.setTbox(t1)
            dp1.setName(normalizeName(dp0.name))
            dp1.setIsIdentityCriteria(dp0.isIdentityCriteria)
            dp1.setDomain(e1)
            dp1.setRange(r1)
            r2t.copy(
              queue_elements = r2t.queue_elements - uuid,
              entityScalarDataProperties = r2t.entityScalarDataProperties + (dp0 -> dp1)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertEntityScalarDataProperty: Failed to resolve " +
                s"EntityScalarDataProperty: $dp0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  private val convertEntityStructuredDataProperty
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, dp0: api.EntityStructuredDataProperty))) =>
      for {
        r2t <- acc
        t1 <- r2t.lookupMap(dp0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2t.getTbox)
        dp1 = terminologies.TerminologiesFactory.eINSTANCE.createEntityStructuredDataProperty()
        upd <- (r2t.entityLookup(dp0.domain), r2t.structures.get(dp0.range)) match {
          case (Some(e1), Some(r1)) =>
            dp1.setTbox(t1)
            dp1.setName(normalizeName(dp0.name))
            dp1.setIsIdentityCriteria(dp0.isIdentityCriteria)
            dp1.setDomain(e1)
            dp1.setRange(r1)
            r2t.copy(
              queue_elements = r2t.queue_elements - uuid,
              entityStructuredDataProperties = r2t.entityStructuredDataProperties + (dp0 -> dp1)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertEntityStructuredDataProperty: Failed to resolve " +
                s"EntityStructuredDataProperty: $dp0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  private val convertScalarDataProperty
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, dp0: api.ScalarDataProperty))) =>
      for {
        r2t <- acc
        t1 <- r2t.lookupMap(dp0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2t.getTbox)
        dp1 = terminologies.TerminologiesFactory.eINSTANCE.createScalarDataProperty()
        upd <- (r2t.structures.get(dp0.domain), r2t.dataRanges.get(dp0.range)) match {
          case (Some(e1), Some(r1)) =>
            dp1.setTbox(t1)
            dp1.setName(normalizeName(dp0.name))
            dp1.setDomain(e1)
            dp1.setRange(r1)
            r2t.copy(
              queue_elements = r2t.queue_elements - uuid,
              scalarDataProperties = r2t.scalarDataProperties + (dp0 -> dp1)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertScalarDataProperty: Failed to resolve " +
                s"ScalarDataProperty: $dp0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  private val convertStructuredDataProperty
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, dp0: api.StructuredDataProperty))) =>
      for {
        r2t <- acc
        t1 <- r2t.lookupMap(dp0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2t.getTbox)
        dp1 = terminologies.TerminologiesFactory.eINSTANCE.createStructuredDataProperty()
        upd <- (r2t.structures.get(dp0.domain), r2t.structures.get(dp0.range)) match {
          case (Some(e1), Some(r1)) =>
            dp1.setTbox(t1)
            dp1.setName(normalizeName(dp0.name))
            dp1.setDomain(e1)
            dp1.setRange(r1)
            r2t.copy(
              queue_elements = r2t.queue_elements - uuid,
              structuredDataProperties = r2t.structuredDataProperties + (dp0 -> dp1)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertStructuredDataProperty: Failed to resolve " +
                s"StructuredDataProperty: $dp0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  private val convertEntityRestrictionAxiom
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, er0: api.EntityRestrictionAxiom))) =>
      for {
        r2t <- acc
        t1 <- r2t.lookupMap(er0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2t.getTbox)
        er1 = er0 match {
          case _: api.EntityExistentialRestrictionAxiom =>
            terminologies.TerminologiesFactory.eINSTANCE.createEntityExistentialRestrictionAxiom()
          case _: api.EntityUniversalRestrictionAxiom =>
            terminologies.TerminologiesFactory.eINSTANCE.createEntityUniversalRestrictionAxiom()
        }
        upd <- (
          r2t.entityLookup(er0.restrictedDomain),
          r2t.restrictableRelationshipLookup(er0.restrictedRelationship),
          r2t.entityLookup(er0.restrictedRange)) match {
          case (Some(d1), Some(rel1), Some(r1)) =>
            er1.setTbox(t1)
            er1.setRestrictedDomain(d1)
            er1.setRestrictedRelationship(rel1)
            er1.setRestrictedRange(r1)
            r2t.copy(
              queue_elements = r2t.queue_elements - uuid,
              termAxioms = r2t.termAxioms + (er0 -> er1)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertEntityRestrictionAxiom: Failed to resolve " +
                s"EntityRestrictionAxiom: $er0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  private def convertDataPropertyRestrictionAxiom
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, er0: api.EntityScalarDataPropertyRestrictionAxiom))) =>
      for {
        r2t <- acc
        t1 <- r2t.lookupMap(er0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2t.getTbox)
        upd <- (
          r2t.entityLookup(er0.restrictedEntity),
          r2t.entityScalarDataProperties.get(er0.scalarProperty)) match {
          case (Some(e1), Some(dp1)) =>
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
            r2t.copy(
              queue_elements = r2t.queue_elements - uuid,
              termAxioms = r2t.termAxioms + (er0 -> er1)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertDataPropertyRestrictionAxiom: Failed to resolve " +
                s"EntityScalarDataPropertyRestrictionAxiom: $er0")).left
        }
      } yield upd
    case (acc, (uuid, (e0, er0: api.EntityStructuredDataPropertyParticularRestrictionAxiom))) =>
      for {
        r2t1 <- acc
        t1 <- r2t1.lookupMap(er0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2t1.getTbox)
        upd <-
        ( r2t1.entityLookup(er0.restrictedEntity),
          r2t1.dataRelationshipToStructureLookup(er0.structuredDataProperty)) match {
          case (Some(e1), Some(dp1)) =>
            val er1 = terminologies.TerminologiesFactory.eINSTANCE.createEntityStructuredDataPropertyParticularRestrictionAxiom()
            er1.setTbox(t1)
            er1.setRestrictedEntity(e1)
            er1.setStructuredDataProperty(dp1)
            val r2t2 =
              r2t1.copy(
                queue_elements = r2t1.queue_elements - uuid,
                termAxioms = r2t1.termAxioms + (er0 -> er1))
            val next =
              convertRestrictionStructuredDataPropertyContext(e0, r2t2.right, Seq(er0 -> er1))
            next
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertDataPropertyRestrictionAxiom: Failed to resolve " +
                s"EntityStructuredDataPropertyParticularRestrictionAxiom: $er0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  @scala.annotation.tailrec
  private final def convertRestrictionStructuredDataPropertyContext
  (ext: api.Extent,
   r2t: ConversionResult,
   cs: Seq[(api.RestrictionStructuredDataPropertyContext, terminologies.RestrictionStructuredDataPropertyContext)])
  : ConversionResult
  = if (cs.isEmpty)
    r2t
  else {
    val (c0, c1) = cs.head
    val values
    : ConversionResult
    = ext
      .restrictionStructuredDataPropertyContextOfRestrictionScalarDataPropertyValue
      .foldLeft(r2t) { case (acc, (vi, ci)) =>
        if (c0 != ci)
          acc
        else
          for {
            r2t1 <- acc
            dp <- r2t1.dataRelationshipToScalarLookup(vi.scalarDataProperty) match {
              case Some(dp1) =>
                dp1.right[EMFProblems]
              case None =>
                new EMFProblems(new java.lang.IllegalArgumentException(
                  s"convertRestrictionStructuredDataPropertyContext: failed to resolved scalar data property; ${vi.scalarDataProperty}"
                )).left
            }
            vt <- vi.valueType match {
              case Some(dt0) =>
                r2t1.dataRanges.get(dt0) match {
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
      (OMLResolver2Text, Seq[(api.RestrictionStructuredDataPropertyContext, terminologies.RestrictionStructuredDataPropertyContext)])
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
            dp <- r2t1.dataRelationshipToStructureLookup(ti.structuredDataProperty) match {
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
        convertRestrictionStructuredDataPropertyContext(ext, next.right, queue)
      case -\/(errors) =>
        -\/(errors)
    }
  }

  private val convertSpecializationAxiom
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, ax0: api.SpecializationAxiom))) =>
      for {
        r2t <- acc
        t1 <- r2t.lookupMap(ax0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2t.getTbox)
        ax1 <- (ax0, r2t.entityLookup(ax0.parent()), r2t.entityLookup(ax0.child())) match {
          case (_: api.ConceptSpecializationAxiom,
          Some(sup: terminologies.ConceptKind),
          Some(sub: terminologies.ConceptKind)) =>
            val s1 = terminologies.TerminologiesFactory.eINSTANCE.createConceptSpecializationAxiom()
            s1.setTbox(t1)
            s1.setSuperConcept(sup)
            s1.setSubConcept(sub)
            s1.right[EMFProblems]
          case (_: api.AspectSpecializationAxiom,
          Some(sup: terminologies.AspectKind),
          Some(sub)) =>
            val s1 = terminologies.TerminologiesFactory.eINSTANCE.createAspectSpecializationAxiom()
            s1.setTbox(t1)
            s1.setSuperAspect(sup)
            s1.setSubEntity(sub)
            s1.right[EMFProblems]
          case (_: api.ReifiedRelationshipSpecializationAxiom,
          Some(sup: terminologies.ConceptualRelationship),
          Some(sub: terminologies.ConceptualRelationship)) =>
            val s1 = terminologies.TerminologiesFactory.eINSTANCE.createReifiedRelationshipSpecializationAxiom()
            s1.setTbox(t1)
            s1.setSuperRelationship(sup)
            s1.setSubRelationship(sub)
            s1.right[EMFProblems]
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertSpecializationAxiom: Failed to resolve " +
                s"SpecializationAxiom: $ax0")).left[terminologies.SpecializationAxiom]
        }
      } yield r2t.copy(termAxioms = r2t.termAxioms + (ax0 -> ax1))
    case (acc, _) =>
      acc
  }

  private val convertSubPropertyOfAxiom
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, ax0: api.SubDataPropertyOfAxiom))) =>
      for {
        r2t <- acc
        t1 <- r2t.lookupMap(ax0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2t.getTbox)
        ax1 <- (
          r2t.entityScalarDataProperties.get(ax0.subProperty),
          r2t.entityScalarDataProperties.get(ax0.superProperty)) match {
          case (
            Some(sub: terminologies.EntityScalarDataProperty),
            Some(sup: terminologies.EntityScalarDataProperty)) =>
            val s1 = terminologies.TerminologiesFactory.eINSTANCE.createSubDataPropertyOfAxiom()
            s1.setTbox(t1)
            s1.setSubProperty(sub)
            s1.setSuperProperty(sup)
            s1.right[EMFProblems]
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertSubPropertyOfAxiom: Failed to resolve " +
                s"SubDataPropertyOfAxiom: $ax0")).left[terminologies.SpecializationAxiom]
        }
      } yield r2t.copy(termAxioms = r2t.termAxioms + (ax0 -> ax1))
    case (acc, (uuid, (e0, ax0: api.SubObjectPropertyOfAxiom))) =>
      for {
        r2t <- acc
        t1 <- r2t.lookupMap(ax0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2t.getTbox)
        ax1 <- (
          r2t.unreifiedRelationships.get(ax0.subProperty),
          r2t.unreifiedRelationships.get(ax0.superProperty)) match {
            case (
              Some(sub: terminologies.UnreifiedRelationship),
              Some(sup: terminologies.UnreifiedRelationship)) =>
              val s1 = terminologies.TerminologiesFactory.eINSTANCE.createSubObjectPropertyOfAxiom()
              s1.setTbox(t1)
              s1.setSubProperty(sub)
              s1.setSuperProperty(sup)
              s1.right[EMFProblems]
            case _ =>
              new EMFProblems(new java.lang.IllegalArgumentException(
                s"convertSubPropertyOfAxiom: Failed to resolve " +
                  s"SubDataPropertyOfAxiom: $ax0")).left[terminologies.SpecializationAxiom]
          }
      } yield r2t.copy(termAxioms = r2t.termAxioms + (ax0 -> ax1))
    case (acc, _) =>
      acc
  }

  private val convertRootConceptTaxonomyAxiom
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, ax0: api.RootConceptTaxonomyAxiom))) =>
      for {
        r2t <- acc
        b1 <- r2t.lookupMap(ax0, e0.bundleOfTerminologyBundleStatement)(e0).flatMap(r2t.getBundle)
        ax1 = bundles.BundlesFactory.eINSTANCE.createRootConceptTaxonomyAxiom()
        upd <- r2t.concepts.get(ax0.root) match {
          case Some(r1) =>
            ax1.setBundle(b1)
            ax1.setRoot(r1)
            val next = r2t.copy(conceptTreeDisjunctions = r2t.conceptTreeDisjunctions + (ax0 -> ax1))
            val disjuncts = disjunctsForConceptTreeDisjunction(e0, ax0, ax1)
            convertConceptTreeDisjunctions(next, e0, disjuncts)
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRootConceptTaxonomyAxiom: Failed to resolve " +
                s"RootConceptTaxonomyAxiom: $ax0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  private def disjunctsForConceptTreeDisjunction
  (ext: api.Extent, ctd0: api.ConceptTreeDisjunction, ctd1: bundles.ConceptTreeDisjunction)
  : Seq[(api.DisjointUnionOfConceptsAxiom, bundles.ConceptTreeDisjunction)]
  = ext.disjunctions.getOrElse(ctd0, Set.empty).to[Seq].map(_ -> ctd1)

  @scala.annotation.tailrec
  private def convertConceptTreeDisjunctions
  (r2t: OMLResolver2Text, ext: api.Extent, disjuncts: Seq[(api.DisjointUnionOfConceptsAxiom, bundles.ConceptTreeDisjunction)])
  : ConversionResult
  = if (disjuncts.isEmpty)
    r2t.right
  else {
    val (dis0, ctd1) = disjuncts.head
    dis0 match {
      case ax0: api.AnonymousConceptUnionAxiom =>
        val ax1 = bundles.BundlesFactory.eINSTANCE.createAnonymousConceptUnionAxiom()
        ax1.setName(normalizeName(ax0.name))
        val next = r2t.copy(disjointUnionOfConceptAxioms = r2t.disjointUnionOfConceptAxioms + (ax0 -> ax1))
        val children = disjunctsForConceptTreeDisjunction(ext, ax0, ax1)
        convertConceptTreeDisjunctions(next, ext, children ++ disjuncts.tail)

      case ax0: api.SpecificDisjointConceptAxiom =>
        val ax1 = bundles.BundlesFactory.eINSTANCE.createSpecificDisjointConceptAxiom()
        r2t.concepts.get(ax0.disjointLeaf) match {
          case Some(l1) =>
            ax1.setDisjointLeaf(l1)
            ax1.setDisjointTaxonomyParent(ctd1)
            val next = r2t.copy(disjointUnionOfConceptAxioms = r2t.disjointUnionOfConceptAxioms + (ax0 -> ax1))
            convertConceptTreeDisjunctions(next, ext, disjuncts.tail)
          case None =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertConceptTreeDisjunctions: Failed to resolve " +
                s"tbox: ${ax0.disjointLeaf}")).left
        }
    }
  }

  private val convertChainRule
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, ax0: api.ChainRule))) =>
      for {
        r2t <- acc
        t1 <- r2t.lookupMap(ax0, e0.terminologyBoxOfTerminologyBoxStatement)(e0).flatMap(r2t.getTbox)
        ax1 = terminologies.TerminologiesFactory.eINSTANCE.createChainRule()
        upd <- (
          r2t.unreifiedRelationships.get(ax0.head),
          e0.firstSegment.get(ax0)) match {
          case (Some(h1), Some(s0)) =>
            e0.predicate.get(s0) match {
              case Some(p0) =>
                ax1.setTbox(t1)
                ax1.setHead(h1)
                ax1.setName(ax0.name)
                val next = r2t.copy(chainRules = r2t.chainRules + (ax0 -> ax1))
                convertRuleBodySegmentPredicate(next, e0, s0, Some(ax1), None, p0)
              case _ =>
                new EMFProblems(new java.lang.IllegalArgumentException(
                  s"convertChainRule: Failed to resolve " +
                    s" unreified relationship: ${ax0.head}" +
                    s" first segment for $ax0")).left
            }
          case (h2, s0) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertChainRule: Failed to resolve $ax0\n" +
                s"=> $t1\n" +
                s"unreified relationship: ${ax0.head}\n" +
                s"=> $h2\n" +
                s"first segment? $s0")).left
        }
      } yield upd
    case (acc, _) =>
      acc
  }

  private def convertPredicate
  (r2t0: OMLResolver2Text,
   ext: api.Extent,
   p0: api.SegmentPredicate)
  : EMFProblems \/ (terminologies.SegmentPredicate,OMLResolver2Text)
  = {
    val p1 = terminologies.TerminologiesFactory.eINSTANCE.createSegmentPredicate()

    (p0.predicate,
      p0.reifiedRelationshipSource,
      p0.reifiedRelationshipInverseSource,
      p0.reifiedRelationshipTarget,
      p0.reifiedRelationshipInverseTarget,
      p0.unreifiedRelationshipInverse) match {

      case (Some(a0: api.Aspect), _, _, _, _, _) =>
        r2t0.aspects.get(a0) match {
          case Some(a1) =>
            p1.setPredicate(a1)
            (p1, r2t0.copy(segmentPredicates = r2t0.segmentPredicates + (p0 -> p1))).right
          case None =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRuleBodySegmentPredicate: Failed to resolve " +
                s"SegmentPredicate(Aspect): ${a0.abbrevIRI()(ext)}")).left
        }

      case (Some(c0: api.Concept), _, _, _, _, _) =>
        r2t0.concepts.get(c0) match {
          case Some(c1) =>
            p1.setPredicate(c1)
            (p1, r2t0.copy(segmentPredicates = r2t0.segmentPredicates + (p0 -> p1))).right
          case None =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRuleBodySegmentPredicate: Failed to resolve " +
                s"SegmentPredicate(Concept): ${c0.abbrevIRI()(ext)}")).left
        }

      case (Some(rr0: api.ReifiedRelationship), _, _, _, _, _) =>
        r2t0.reifiedRelationships.get(rr0) match {
          case Some(rr1) =>
            p1.setPredicate(rr1)
            (p1, r2t0.copy(segmentPredicates = r2t0.segmentPredicates + (p0 -> p1))).right
          case None =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRuleBodySegmentPredicate: Failed to resolve " +
                s"SegmentPredicate(ReifiedRelationship): ${rr0.abbrevIRI()(ext)}")).left
        }

      case (Some(fwd0: api.ForwardProperty), _, _, _, _, _) =>
        r2t0.reifiedRelationships.get(fwd0.reifiedRelationship) match {
          case Some(rr1) =>
            val fwd1 = rr1.getForwardProperty
            require(null != fwd1)
            p1.setPredicate(fwd1)
            (p1, r2t0.copy(segmentPredicates = r2t0.segmentPredicates + (p0 -> p1))).right
          case None =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRuleBodySegmentPredicate: Failed to resolve " +
                s"SegmentPredicate(ForwardProperty): ${fwd0.abbrevIRI()(ext)}")).left
        }

      case (Some(inv0: api.InverseProperty), _, _, _, _, _) =>
        r2t0.reifiedRelationships.get(inv0.reifiedRelationship) match {
          case Some(rr1) =>
            val inv1 = rr1.getInverseProperty
            require(null != inv1)
            p1.setPredicate(inv1)
            (p1, r2t0.copy(segmentPredicates = r2t0.segmentPredicates + (p0 -> p1))).right
          case None =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRuleBodySegmentPredicate: Failed to resolve " +
                s"SegmentPredicate(InverseProperty): ${inv0.abbrevIRI()(ext)}")).left
        }

      case (Some(ur0: api.UnreifiedRelationship), _, _, _, _, _) =>
        r2t0.unreifiedRelationships.get(ur0) match {
          case Some(ur1) =>
            p1.setPredicate(ur1)
            (p1, r2t0.copy(segmentPredicates = r2t0.segmentPredicates + (p0 -> p1))).right
          case None =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRuleBodySegmentPredicate: Failed to resolve " +
                s"SegmentPredicate(UnreifiedRelationship): ${ur0.abbrevIRI()(ext)}")).left
        }

      case (None, Some(rr0), _, _, _, _) =>
        r2t0.reifiedRelationships.get(rr0) match {
          case Some(rr1) =>
            p1.setReifiedRelationshipSource(rr1)
            (p1, r2t0.copy(segmentPredicates = r2t0.segmentPredicates + (p0 -> p1))).right
          case None =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRuleBodySegmentPredicate: Failed to resolve " +
                s"ReifiedRelationshipSourcePropertyPredicate: $p0")).left
        }

      case (None, _, Some(rr0), _, _, _) =>
        r2t0.reifiedRelationships.get(rr0) match {
          case Some(rr1) =>
            p1.setReifiedRelationshipInverseSource(rr1)
            (p1, r2t0.copy(segmentPredicates = r2t0.segmentPredicates + (p0 -> p1))).right
          case None =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRuleBodySegmentPredicate: Failed to resolve " +
                s"ReifiedRelationshipSourceInversePropertyPredicate: $p0")).left
        }

      case (None, _, _,Some(rr0), _, _) =>
        r2t0.reifiedRelationships.get(rr0) match {
          case Some(rr1) =>
            p1.setReifiedRelationshipTarget(rr1)
            (p1, r2t0.copy(segmentPredicates = r2t0.segmentPredicates + (p0 -> p1))).right
          case None =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRuleBodySegmentPredicate: Failed to resolve " +
                s"ReifiedRelationshipTargetPropertyPredicate: $p0")).left
        }

      case (None, _, _, _, Some(rr0), _) =>
        r2t0.reifiedRelationships.get(rr0) match {
          case Some(rr1) =>
            p1.setReifiedRelationshipInverseTarget(rr1)
            (p1, r2t0.copy(segmentPredicates = r2t0.segmentPredicates + (p0 -> p1))).right
          case None =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRuleBodySegmentPredicate: Failed to resolve " +
                s"ReifiedRelationshipTargetInversePropertyPredicate: $p0")).left
        }

      case (None, _, _, _, _, Some(ur0)) =>
        r2t0.unreifiedRelationships.get(ur0) match {
          case Some(ur1) =>
            p1.setUnreifiedRelationshipInverse(ur1)
            (p1, r2t0.copy(segmentPredicates = r2t0.segmentPredicates + (p0 -> p1))).right
          case None =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRuleBodySegmentPredicate: Failed to resolve " +
                s"UnreifiedRelationshipInversePropertyPredicate: $p0")).left
        }

      case (pred, source, isource, target, itarget, uinv) =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"OWLResolver2Ontology.convertRuleBodySegment(ruleBodySegment=$p0) unresolved:\n" +
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
  }

  @scala.annotation.tailrec
  private final def convertRuleBodySegmentPredicate
  (r2t0: OMLResolver2Text,
   ext: api.Extent,
   s0: api.RuleBodySegment,
   rule: Option[terminologies.ChainRule],
   previousSegment: Option[terminologies.RuleBodySegment],
   predicate: api.SegmentPredicate)
  : ConversionResult
  = convertPredicate(r2t0, ext, predicate) match {
    case \/-((p1, r2t1)) =>
      val s1 = terminologies.TerminologiesFactory.eINSTANCE.createRuleBodySegment()
      val r2t2 = r2t1.copy(ruleBodySegments = r2t1.ruleBodySegments + (s0 -> s1))
      s1.setPredicate(p1)
      rule.foreach(s1.setRule)
      previousSegment.foreach(s1.setPreviousSegment)
      ext.nextSegment.get(s0) match {
        case Some(n0) =>
          ext.predicate.get(n0) match {
            case Some(p0) =>
              convertRuleBodySegmentPredicate(r2t2, ext, n0, None, Some(s1), p0)
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

  private val convertConceptInstance
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, ci0: api.ConceptInstance))) =>
      for {
        r2t <- acc
        d1 <- r2t.lookupMap(ci0, e0.descriptionBoxOfConceptInstance)(e0).flatMap(r2t.getDbox)
        ci1 = descriptions.DescriptionsFactory.eINSTANCE.createConceptInstance()
        upd <- r2t.concepts.get(ci0.singletonConceptClassifier) match {
          case Some(c1) =>
            ci1.setDescriptionBox(d1)
            ci1.setName(normalizeName(ci0.name))
            ci1.setSingletonConceptClassifier(c1)
            r2t.copy(conceptualEntitySingletonInstances = r2t.conceptualEntitySingletonInstances + (ci0 -> ci1)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertConceptInstance: Failed to resolve concept: ${ci0.singletonConceptClassifier}")
            ).left
        }
      } yield upd
  }

  private val convertReifiedRelationshipInstance
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, rri0: api.ReifiedRelationshipInstance))) =>
      for {
        r2t <- acc
        d1 <- r2t.lookupMap(rri0, e0.descriptionBoxOfReifiedRelationshipInstance)(e0).flatMap(r2t.getDbox)
        rri1 = descriptions.DescriptionsFactory.eINSTANCE.createReifiedRelationshipInstance()
        upd <- r2t.conceptualRelationshipLookup(rri0.singletonConceptualRelationshipClassifier) match {
          case Some(rr1) =>
            rri1.setDescriptionBox(d1)
            rri1.setName(normalizeName(rri0.name))
            rri1.setSingletonConceptualRelationshipClassifier(rr1)
            r2t.copy(conceptualEntitySingletonInstances = r2t.conceptualEntitySingletonInstances + (rri0 -> rri1)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertReifiedRelationshipInstance: Failed to resolve reified relationship: ${rri0.singletonConceptualRelationshipClassifier}")
            ).left
        }
      } yield upd
  }

  private val convertReifiedRelationshipInstanceDomain
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, rrid0: api.ReifiedRelationshipInstanceDomain))) =>
      for {
        r2t <- acc
        d1 <- r2t.lookupMap(rrid0, e0.descriptionBoxOfReifiedRelationshipInstanceDomain)(e0).flatMap(r2t.getDbox)
        rrid1 = descriptions.DescriptionsFactory.eINSTANCE.createReifiedRelationshipInstanceDomain()
        upd <- (
          r2t.conceptualEntitySingletonInstances.get(rrid0.reifiedRelationshipInstance),
          r2t.conceptualEntitySingletonInstances.get(rrid0.domain)) match {
          case (Some(rri1: descriptions.ReifiedRelationshipInstance), Some(di1)) =>
            rrid1.setDescriptionBox(d1)
            rrid1.setReifiedRelationshipInstance(rri1)
            rrid1.setDomain(di1)
            r2t.copy(reifiedRelationshipInstanceDomains = r2t.reifiedRelationshipInstanceDomains + (rrid0 -> rrid1)).right
          case (rr1, di1) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertReifiedRelationshipInstanceDomain: Failed to resolve " +
                rr1.fold(s" reified relationship instance: ${rrid0.reifiedRelationshipInstance}")(_ => "") +
                di1.fold(s" domain: ${rrid0.domain}")(_ => ""))
            ).left
        }
      } yield upd
  }

  private val convertReifiedRelationshipInstanceRange
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, rrir0: api.ReifiedRelationshipInstanceRange))) =>
      for {
        r2t <- acc
        d1 <- r2t.lookupMap(rrir0, e0.descriptionBoxOfReifiedRelationshipInstanceRange)(e0).flatMap(r2t.getDbox)
        rrir1 = descriptions.DescriptionsFactory.eINSTANCE.createReifiedRelationshipInstanceRange()
        upd <- (
          r2t.conceptualEntitySingletonInstances.get(rrir0.reifiedRelationshipInstance),
          r2t.conceptualEntitySingletonInstances.get(rrir0.range)) match {
          case (Some(rri1: descriptions.ReifiedRelationshipInstance), Some(ri1)) =>
            rrir1.setDescriptionBox(d1)
            rrir1.setReifiedRelationshipInstance(rri1)
            rrir1.setRange(ri1)
            r2t.copy(reifiedRelationshipInstanceRanges = r2t.reifiedRelationshipInstanceRanges + (rrir0 -> rrir1)).right
          case (rr1, di1) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertReifiedRelationshipInstanceRange: Failed to resolve " +
                rr1.fold(s" reified relationship instance: ${rrir0.reifiedRelationshipInstance}")(_ => "") +
                di1.fold(s" range: ${rrir0.range}")(_ => ""))
            ).left
        }
      } yield upd
  }

  private val convertUnreifiedRelationshipInstanceTuple
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, urit0: api.UnreifiedRelationshipInstanceTuple))) =>
      for {
        r2t <- acc
        d1 <- r2t.lookupMap(urit0, e0.descriptionBoxOfUnreifiedRelationshipInstanceTuple)(e0).flatMap(r2t.getDbox)
        urit1 = descriptions.DescriptionsFactory.eINSTANCE.createUnreifiedRelationshipInstanceTuple()
        upd <- (
          r2t.unreifiedRelationships.get(urit0.unreifiedRelationship),
          r2t.conceptualEntitySingletonInstances.get(urit0.domain),
          r2t.conceptualEntitySingletonInstances.get(urit0.range)) match {
          case (Some(ur1), Some(di1), Some(ri1)) =>
            urit1.setDescriptionBox(d1)
            urit1.setUnreifiedRelationship(ur1)
            urit1.setDomain(di1)
            urit1.setRange(ri1)
            r2t.copy(unreifiedRelationshipInstanceTuples = r2t.unreifiedRelationshipInstanceTuples + (urit0 -> urit1)).right
          case (ur1, di1, ri1) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertUnreifiedRelationshipInstanceTuple: Failed to resolve " +
                ur1.fold(s" unreified relationship: ${urit0.unreifiedRelationship}")(_ => "") +
                di1.fold(s" domain: ${urit0.domain}")(_ => "")+
                ri1.fold(s" range: ${urit0.range}")(_ => ""))
            ).left
        }
      } yield upd
  }

  private val convertSingletonInstanceStructuredDataPropertyValue
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, si0: api.SingletonInstanceStructuredDataPropertyValue))) =>
      for {
        r2t <- acc
        d1 <- r2t.lookupMap(si0, e0.descriptionBoxOfSingletonInstanceStructuredDataPropertyValue)(e0).flatMap(r2t.getDbox)
        si1 = descriptions.DescriptionsFactory.eINSTANCE.createSingletonInstanceStructuredDataPropertyValue()
        upd <- (
          r2t.conceptualEntitySingletonInstances.get(si0.singletonInstance),
          r2t.dataRelationshipToStructureLookup(si0.structuredDataProperty)) match {
          case (Some(ce1), Some(sdp1)) =>
            si1.setDescriptionBox(d1)
            si1.setSingletonInstance(ce1)
            si1.setStructuredDataProperty(sdp1)
            r2t.copy(singletonInstanceStructuredDataPropertyValues = r2t.singletonInstanceStructuredDataPropertyValues + (si0 -> si1)).right
          case (ur1, di1) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertSingletonInstanceStructuredDataPropertyValue: Failed to resolve " +
                ur1.fold(s" singleton instance ${si0.singletonInstance}")(_ => "") +
                di1.fold(s" structuredDataProperty: ${si0.structuredDataProperty}")(_ => "")
            )).left
        }
      } yield upd
  }

  private val convertSingletonInstanceScalarDataPropertyValue
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, si0: api.SingletonInstanceScalarDataPropertyValue))) =>
      for {
        r2t <- acc
        d1 <- r2t.lookupMap(si0, e0.descriptionBoxOfSingletonInstanceScalarDataPropertyValue)(e0).flatMap(r2t.getDbox)
        si1 = descriptions.DescriptionsFactory.eINSTANCE.createSingletonInstanceScalarDataPropertyValue()
        upd <- (
          r2t.conceptualEntitySingletonInstances.get(si0.singletonInstance),
          r2t.entityScalarDataProperties.get(si0.scalarDataProperty)) match {
          case (Some(ce1), Some(sdp1)) =>
            si1.setDescriptionBox(d1)
            si1.setSingletonInstance(ce1)
            si1.setScalarDataProperty(sdp1)
            r2t.copy(singletonInstanceScalarDataPropertyValues = r2t.singletonInstanceScalarDataPropertyValues + (si0 -> si1)).right
          case (ur1, di1) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertSingletonInstanceScalarDataPropertyValue: Failed to resolve " +
                ur1.fold(s" singleton instance ${si0.singletonInstance}")(_ => "") +
                di1.fold(s" scalarDataProperty: ${si0.scalarDataProperty}")(_ => "")
            )).left
        }
      } yield upd
  }

  private val convertStructuredDataPropertyTuple
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, si0: api.StructuredDataPropertyTuple))) =>
      for {
        r2t <- acc
        ctx0 <- r2t.lookupMap(si0, e0.singletonInstanceStructuredDataPropertyContextOfStructuredDataPropertyTuple)(e0)
        si1 = descriptions.DescriptionsFactory.eINSTANCE.createStructuredDataPropertyTuple()
        upd <- (
          r2t.structuredDataPropertyContext(ctx0),
          r2t.dataRelationshipToStructureLookup(si0.structuredDataProperty)) match {
          case (Some(ctx1), Some(sdp1)) =>
            si1.setStructuredDataPropertyContext(ctx1)
            si1.setStructuredDataProperty(sdp1)
            r2t.copy(structuredDataPropertyTuples = r2t.structuredDataPropertyTuples + (si0 -> si1)).right
          case (ctx1, sdp1) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertStructuredDataPropertyTuple: Failed to resolve " +
                ctx1.fold(s" structuredDataPropertyContext: $ctx0")(_ => "") +
                sdp1.fold(s" structuredDataProperty ${si0.structuredDataProperty}")(_ => "")
            )).left
        }
      } yield upd
  }

  private val convertScalarDataPropertyValue
  : (ConversionResult, (api.taggedTypes.ModuleElementUUID, (api.Extent, api.ModuleElement))) => ConversionResult
  = {
    case (acc, (uuid, (e0, si0: api.ScalarDataPropertyValue))) =>
      for {
        r2t <- acc
        ctx0 <- r2t.lookupMap(si0, e0.singletonInstanceStructuredDataPropertyContextOfScalarDataPropertyValue)(e0)
        si1 = descriptions.DescriptionsFactory.eINSTANCE.createScalarDataPropertyValue()
        upd <- (
          r2t.structuredDataPropertyContext(ctx0),
          r2t.dataRelationshipToScalarLookup(si0.scalarDataProperty),
          si0.valueType,
          si0.valueType.flatMap(r2t.dataRanges.get)) match {
          case (Some(ctx1), Some(sdp1), Some(_), Some(vt1)) =>
            si1.setStructuredDataPropertyContext(ctx1)
            si1.setScalarDataProperty(sdp1)
            si1.setValueType(vt1)
            r2t.copy(scalarDataPropertyValues = r2t.scalarDataPropertyValues + (si0 -> si1)).right
          case (Some(ctx1), Some(sdp1), None, None) =>
            si1.setStructuredDataPropertyContext(ctx1)
            si1.setScalarDataProperty(sdp1)
            r2t.copy(scalarDataPropertyValues = r2t.scalarDataPropertyValues + (si0 -> si1)).right
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

  private val convertAnnotationPropertyValue
  : (ConversionResult, api.AnnotationPropertyValue) => ConversionResult
  = {
    case (acc, apv0) =>
      for {
        r2t <- acc
        apv1 = common.CommonFactory.eINSTANCE.createAnnotationPropertyValue()
        _ <- (
          r2t.annotationPropertyLookup(apv0.property),
          r2t.elementLookup(apv0.subject)
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