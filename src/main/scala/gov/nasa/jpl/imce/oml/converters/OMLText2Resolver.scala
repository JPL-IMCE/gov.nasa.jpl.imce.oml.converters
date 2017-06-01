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

import gov.nasa.jpl.imce.oml.converters.utils.{EMFProblems, OMLResourceSet}
import gov.nasa.jpl.imce.oml.model.bundles._
import gov.nasa.jpl.imce.oml.model.common._
import gov.nasa.jpl.imce.oml.model.descriptions._
import gov.nasa.jpl.imce.oml.model.graphs._
import gov.nasa.jpl.imce.oml.model.terminologies._
import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.tables.{ClosedWorldDesignations, OpenWorldDefinitions, TerminologyKind => TTerminologyKind}
import gov.nasa.jpl.imce.oml.tables.{Final, Partial, DescriptionKind => TDescriptionKind}
import gov.nasa.jpl.imce.oml.converters.utils.{EMFProblems, OMLResourceSet}
import org.eclipse.xtext.resource.XtextResourceSet

import scala.collection.JavaConverters._
import scala.collection.immutable._
import scala.Predef.ArrowAssoc
import scala.{None, Option, Some, StringContext, Tuple2, Tuple3}
import scalaz._, Scalaz._

/*
 Manage 1 OML Extent => 1 OMLText2Resolver.
 */
case class OMLText2Resolver
(rextent: api.Extent,
 tboxes: Map[TerminologyBox, api.TerminologyBox] = Map.empty,
 tboxAxioms: Set[TerminologyBoxAxiom] = Set.empty,
 tboxStatements: Set[TerminologyBoxStatement] = Set.empty,
 bundleAxioms: Set[TerminologyBundleAxiom] = Set.empty,
 bundleStatements: Set[TerminologyBundleStatement] = Set.empty,
 dboxes: Map[DescriptionBox, api.DescriptionBox] = Map.empty,
 dboxDefinitions: Set[DescriptionBoxExtendsClosedWorldDefinitions] = Set.empty,
 dboxRefinements: Set[DescriptionBoxRefinement] = Set.empty,

 aps: Map[AnnotationProperty, api.AnnotationProperty] = Map.empty,

 aspects: Map[Aspect, api.Aspect] = Map.empty,
 concepts: Map[Concept, api.Concept] = Map.empty,
 reifiedRelationships: Map[ReifiedRelationship, api.ReifiedRelationship] = Map.empty,
 unreifiedRelationships: Map[UnreifiedRelationship, api.UnreifiedRelationship] = Map.empty,

 dataRanges: Map[DataRange, api.DataRange] = Map.empty,
 structures: Map[Structure, api.Structure] = Map.empty,
 scalarOneOfLiterals: Map[ScalarOneOfLiteralAxiom, api.ScalarOneOfLiteralAxiom] = Map.empty,

 entityScalarDataProperties: Map[EntityScalarDataProperty, api.EntityScalarDataProperty] = Map.empty,
 entityStructuredDataProperties: Map[EntityStructuredDataProperty, api.EntityStructuredDataProperty] = Map.empty,
 scalarDataProperties: Map[ScalarDataProperty, api.ScalarDataProperty] = Map.empty,
 structuredDataProperties: Map[StructuredDataProperty, api.StructuredDataProperty] = Map.empty,

 moduleEdges: Map[ModuleEdge, api.ModuleEdge] = Map.empty,
 terminologyAxioms: Map[TerminologyAxiom, api.TerminologyAxiom] = Map.empty,
 termAxioms: Map[TermAxiom, api.TermAxiom] = Map.empty,
 conceptTreeDisjunctions: Map[ConceptTreeDisjunction, api.ConceptTreeDisjunction] = Map.empty,
 disjointUnionOfConceptAxioms: Map[DisjointUnionOfConceptsAxiom, api.DisjointUnionOfConceptsAxiom] = Map.empty,
 conceptualEntitySingletonInstances: Map[ConceptualEntitySingletonInstance, api.ConceptualEntitySingletonInstance] = Map.empty) {

  def entityLookup(e: Entity): Option[api.Entity] = e match {
    case a: Aspect => aspects.get(a)
    case c: Concept => concepts.get(c)
    case rr: ReifiedRelationship => reifiedRelationships.get(rr)
    case _ => None
  }

  def entityRelationshipLookup(rl: EntityRelationship): Option[api.EntityRelationship] = rl match {
    case rr: ReifiedRelationship => reifiedRelationships.get(rr)
    case ur: UnreifiedRelationship => unreifiedRelationships.get(ur)
    case _ => None
  }

  def dataRelationshipToStructureLookup(dp: DataRelationshipToStructure): Option[api.DataRelationshipToStructure] = dp match {
    case edp: EntityStructuredDataProperty =>
      entityStructuredDataProperties.get(edp)
    case sdp: StructuredDataProperty =>
      structuredDataProperties.get(sdp)
  }

  def dataRelationshipToScalarLookup(dp: DataRelationshipToScalar): Option[api.DataRelationshipToScalar] = dp match {
    case edp: EntityScalarDataProperty =>
      entityScalarDataProperties.get(edp)
    case sdp: ScalarDataProperty =>
      scalarDataProperties.get(sdp)
  }
}

object OMLText2Resolver {

  def convert
  (rs: XtextResourceSet)
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = for {
    extents <- OMLResourceSet.getOMLExtents(rs)
    c00 = extents.map {
      _ -> OMLText2Resolver(rextent = api.Extent())
    }

    // Modules
    c01 <- c00.foldLeft {
      Map.empty[Extent, OMLText2Resolver].right[EMFProblems]
    } { case (acc, (e, omlt2r)) =>
      for {
        ci <- acc
        updated <- convertModules(omlt2r, e.getModules.asScala.to[Set])
      } yield ci + (e -> updated)
    }
  } yield c01

//    // AnnotationProperties
//    c02 <- convertAnnotationProperties(c01,
//      extents.flatMap(_.getAnnotationProperties.asScala))
//
//    // TerminologyExtensions
//
//    c03 <- convertTerminologyExtensions(c02,
//      c02.tboxAxioms.selectByKindOf { case ax: TerminologyExtensionAxiom => ax})
//
//    // Atomic Entities
//
//    c10 <- convertAspects(c03,
//      c03.tboxStatements.selectByKindOf { case s: Aspect => s})
//    c11 <- convertConcepts(c10,
//      c10.tboxStatements.selectByKindOf { case s: Concept => s})
//
//    // Other ModuleEdges
//
//    c20 <- convertConceptDesignationTerminologyAxioms(c11,
//      c11.tboxAxioms.selectByKindOf { case ax: ConceptDesignationTerminologyAxiom => ax})
//    c21 <- convertTerminologyNestingAxioms(c20,
//      c20.tboxAxioms.selectByKindOf { case ax: TerminologyNestingAxiom => ax})
//    c22 <- convertBundledTerminologyAxioms(c21,
//      c21.tboxAxioms.selectByKindOf { case ax: BundledTerminologyAxiom => ax})
//    c23 <- convertDescriptionBoxExtendsClosedWorldDefinitions(c22,
//      c22.dboxDefinitions)
//    c24 <- convertDescriptionBoxRefinements(c23,
//      c23.dboxRefinements)
//
//    // Relationships
//
//    c30 <- convertReifiedRelationships(c24,
//      c24.tboxStatements.selectByKindOf { case s: ReifiedRelationship => s})
//    c31 <- convertUnreifiedRelationships(c30,
//      c30.tboxStatements.selectByKindOf { case s: UnreifiedRelationship => s})
//
//    // DataTypes
//
//    c32 <- convertStructures(c31, c31.tboxStatements.selectByKindOf { case s: Structure => s})
//    c33 <- convertScalars(c32, c32.tboxStatements.selectByKindOf { case s: Scalar => s})
//    c34 <- convertRestrictedDataRanges(c33,
//      c33.tboxStatements.selectByKindOf { case s: RestrictedDataRange => s})
//    c35 <- convertScalarOneOfLiteralAxioms(c34,
//      c34.tboxStatements.selectByKindOf { case s: ScalarOneOfLiteralAxiom => s})
//
//    // DataRelationships
//
//    c40 <- convertEntityScalarDataProperties(c35,
//      c35.tboxStatements.selectByKindOf { case s: EntityScalarDataProperty => s})
//    c41 <- convertEntityStructuredDataProperties(c40,
//      c40.tboxStatements.selectByKindOf { case s: EntityStructuredDataProperty => s})
//    c42 <- convertScalarDataProperties(c41,
//      c41.tboxStatements.selectByKindOf { case s: ScalarDataProperty => s})
//    c43 <- convertStructuredDataProperties(c42,
//      c42.tboxStatements.selectByKindOf { case s: StructuredDataProperty => s})
//
//    // Restrictions
//
//    c50 <- convertEntityRestrictionAxioms(c43,
//      c43.tboxStatements.selectByKindOf { case ax: EntityRestrictionAxiom => ax})
//    c51 <- convertEntityScalarDataPropertyRestrictionAxioms(c50,
//      c50.tboxStatements.selectByKindOf { case ax: EntityScalarDataPropertyRestrictionAxiom => ax })
//
//    // Specializations
//
//    c52 <- convertSpecializationAxioms(c51,
//      c51.tboxStatements.selectByKindOf { case ax: SpecializationAxiom => ax })
//
//    // Disjunctions
//
//    c60 <- convertRootConceptTaxonomyAxioms(c52,
//      c52.bundleStatements.selectByKindOf { case ax: RootConceptTaxonomyAxiom => ax})
//
//    // ConceptualEntityInstances
//
//    c70 <- convertConceptInstances(c60,
//      c60.dboxes.map { case (dboxi, dboxj) => (dboxi, dboxj, dboxi.getConceptInstances.asScala.to[List])}.to[List])
//    c71 <- convertReifiedRelationshipInstances(c70,
//      c70.dboxes.map { case (dboxi, dboxj) => (dboxi, dboxj, dboxi.getReifiedRelationshipInstances.asScala.to[List])}.to[List])
//    c72 <- convertReifiedRelationshipInstanceDomains(c71,
//      c71.dboxes.map { case (dboxi, dboxj) => (dboxi, dboxj, dboxi.getReifiedRelationshipInstanceDomains.asScala.to[List])}.to[List])
//    c73 <- convertReifiedRelationshipInstanceRanges(c72,
//      c72.dboxes.map { case (dboxi, dboxj) => (dboxi, dboxj, dboxi.getReifiedRelationshipInstanceRanges.asScala.to[List])}.to[List])
//    c74 <- convertUnreifiedRelationshipInstances(c73,
//      c73.dboxes.map { case (dboxi, dboxj) => (dboxi, dboxj, dboxi.getUnreifiedRelationshipInstanceTuples.asScala.to[List])}.to[List])
//
//    // Data Property Values
//
//    c80 <- convertSingletonScalarDataPropertyValues(c74,
//      c74.dboxes.map { case (dboxi, dboxj) => (dboxi, dboxj, dboxi.getSingletonScalarDataPropertyValues.asScala.to[List])}.to[List])
//    c81 <- convertSingletonStructuredDataPropertyValues(c80,
//      c80.dboxes.map { case (dboxi, dboxj) => (dboxi, dboxj, dboxi.getSingletonStructuredDataPropertyValues.asScala.to[List])}.to[List])
//  } yield c81

  protected def convertTerminologyGraphKind(k: TerminologyKind): TTerminologyKind = k match {
    case TerminologyKind.OPEN_WORLD_DEFINITIONS =>
      OpenWorldDefinitions
    case TerminologyKind.CLOSED_WORLD_DESIGNATIONS =>
      ClosedWorldDesignations
  }

  protected def convertDescriptionKind(k: DescriptionKind): TDescriptionKind = k match {
    case DescriptionKind.FINAL =>
      Final
    case DescriptionKind.PARTIAL =>
      Partial
  }

  protected def convertModules
  (o2r: OMLText2Resolver, modules: Set[Module])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = {
    val result: OMLText2Resolver = modules.foldLeft[OMLText2Resolver](o2r) { case (ri, mi) =>
      mi match {
        case gi: TerminologyGraph =>
          val (rj, gj) = f.createTerminologyGraph(ri.rextent, convertTerminologyGraphKind(gi.getKind), gi.iri())
          ri.copy(
            rextent = rj,
            tboxes = ri.tboxes + (gi -> gj),
            tboxAxioms = ri.tboxAxioms ++ gi.getBoxAxioms.asScala.to[Set],
            tboxStatements = ri.tboxStatements ++ gi.getBoxStatements.asScala.to[Set])
        case bi: Bundle =>
          val (rj, bj) = f.createBundle(ri.rextent, convertTerminologyGraphKind(bi.getKind), bi.iri())
          ri.copy(
            rextent = rj,
            tboxes = ri.tboxes + (bi -> bj),
            tboxAxioms = ri.tboxAxioms ++ bi.getBoxAxioms.asScala.to[Set],
            tboxStatements = ri.tboxStatements ++ bi.getBoxStatements.asScala.to[Set],
            bundleAxioms = ri.bundleAxioms ++ bi.getBundleAxioms.asScala.to[Set],
            bundleStatements = ri.bundleStatements ++ bi.getBundleStatements.asScala.to[Set])
        case di: DescriptionBox =>
          val (rj, dj) = f.createDescriptionBox(ri.rextent, convertDescriptionKind(di.getKind), di.iri())
          ri.copy(
            rextent = rj,
            dboxes = ri.dboxes + (di -> dj),
            dboxDefinitions = ri.dboxDefinitions ++ di.getClosedWorldDefinitions.asScala.to[Set],
            dboxRefinements = ri.dboxRefinements ++ di.getDescriptionBoxRefinements.asScala.to[Set])
      }
    }
    result.right
  }

  protected def convertAnnotationProperties
  (o2r: OMLText2Resolver, aps: Set[AnnotationProperty])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = {
    val result = aps.foldLeft[OMLText2Resolver](o2r) { case (ri, pi) =>
      val (rj, pj) = f.createAnnotationProperty(ri.rextent, pi.getIri, pi.getAbbrevIRI)
      ri.copy(rextent = rj, aps = ri.aps + (pi -> pj))
    }
    result.right
  }

  @scala.annotation.tailrec
  protected def convertTerminologyExtensions
  (o2r: OMLText2Resolver, axs: Iterable[TerminologyExtensionAxiom])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (axs.isEmpty)
    o2r.right
  else {
    val (axi, remaining) = (axs.head, axs.tail)
    (o2r.tboxes.get(axi.extendingTerminology()), o2r.tboxes.get(axi.getExtendedTerminology)) match {
      case ((Some(extending), Some(extended))) =>
        val (rj, axj) = f.createTerminologyExtensionAxiom(o2r.rextent, extending, extended)
        val updated = o2r.copy(rextent = rj, moduleEdges = o2r.moduleEdges + (axi -> axj))
        convertTerminologyExtensions(updated, remaining)
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"convertTerminologyExtensions: Failed to resolve extending: ${axi.extendingTerminology().abbrevIRI()} / extended: ${axi.getExtendedTerminology.abbrevIRI()}"
        )).left
    }
  }

  @scala.annotation.tailrec
  protected def convertConceptDesignationTerminologyAxioms
  (o2r: OMLText2Resolver, axs: Iterable[ConceptDesignationTerminologyAxiom])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (axs.isEmpty)
    o2r.right
  else {
    val (axi, remaining) = (axs.head, axs.tail)
    (o2r.tboxes.get(axi.designationTerminologyGraph()), o2r.tboxes.get(axi.getDesignatedTerminology), o2r.concepts.get(axi.getDesignatedConcept)) match {
      case (Some(designationG), Some(designatedT), Some(designatedC)) =>
        val (rj, axj) = f.createConceptDesignationTerminologyAxiom(o2r.rextent, designationG, designatedC, designatedT)
        val updated = o2r.copy(rextent = rj, moduleEdges = o2r.moduleEdges + (axi -> axj))
        convertConceptDesignationTerminologyAxioms(updated, remaining)
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"convertConceptDesignationTerminologyAxioms: Failed to resolve "+
            s"designationG: ${axi.designationTerminologyGraph().abbrevIRI()}; "+
            s"designatedT: ${axi.getDesignatedTerminology.abbrevIRI()}; "+
            s"designatedC: ${axi.getDesignatedConcept.abbrevIRI()}"
        )).left
    }
  }

  @scala.annotation.tailrec
  protected def convertTerminologyNestingAxioms
  (o2r: OMLText2Resolver, axs: Iterable[TerminologyNestingAxiom])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (axs.isEmpty)
    o2r.right
  else {
    val (axi, remaining) = (axs.head, axs.tail)
    (o2r.tboxes.get(axi.nestedTerminology()), o2r.tboxes.get(axi.getNestingTerminology), o2r.concepts.get(axi.getNestingContext)) match {
      case (Some(nestedG: api.TerminologyGraph), Some(nestingT), Some(nestingC)) =>
        val (rj, axj) = f.createTerminologyNestingAxiom(o2r.rextent, nestedG, nestingT, nestingC)
        val updated = o2r.copy(rextent = rj, moduleEdges = o2r.moduleEdges + (axi -> axj))
        convertTerminologyNestingAxioms(updated, remaining)
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"convertTerminologyNestingAxioms: Failed to resolve "+
            s"nested G: ${axi.nestedTerminology.abbrevIRI()}; "+
            s"nestingT: ${axi.getNestingTerminology.abbrevIRI()}; "+
            s"nestingC: ${axi.getNestingContext.abbrevIRI()}"
        )).left
    }
  }

  @scala.annotation.tailrec
  protected def convertBundledTerminologyAxioms
  (o2r: OMLText2Resolver, axs: Iterable[BundledTerminologyAxiom])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (axs.isEmpty)
    o2r.right
  else {
    val (axi, remaining) = (axs.head, axs.tail)
    (o2r.tboxes.get(axi.getBundle), o2r.tboxes.get(axi.getBundledTerminology)) match {
      case (Some(bundle: api.Bundle), Some(bundled: api.TerminologyBox)) =>
        val (rj, axj) = f.createBundledTerminologyAxiom(o2r.rextent, bundled, bundle)
        val updated = o2r.copy(rextent = rj, moduleEdges = o2r.moduleEdges + (axi -> axj))
        convertBundledTerminologyAxioms(updated, remaining)
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"convertBundledTerminologyAxioms: Failed to resolve "+
            s"bundle: ${axi.getBundle.abbrevIRI()}; "+
            s"bundled: ${axi.getBundledTerminology.abbrevIRI()}"
        )).left
    }
  }

  @scala.annotation.tailrec
  protected def convertDescriptionBoxExtendsClosedWorldDefinitions
  (o2r: OMLText2Resolver, axs: Iterable[DescriptionBoxExtendsClosedWorldDefinitions])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (axs.isEmpty)
    o2r.right
  else {
    val (axi, remaining) = (axs.head, axs.tail)
    (o2r.dboxes.get(axi.getDescriptionBox), o2r.tboxes.get(axi.getClosedWorldDefinitions)) match {
      case (Some(dbox), Some(definitions)) =>
        val (rj, axj) = f.createDescriptionBoxExtendsClosedWorldDefinitions(o2r.rextent, dbox, definitions)
        val updated = o2r.copy(rextent = rj, moduleEdges = o2r.moduleEdges + (axi -> axj))
        convertDescriptionBoxExtendsClosedWorldDefinitions(updated, remaining)
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"convertDescriptionBoxExtendsClosedWorldDefinitions: Failed to resolve "+
            s"dbox: ${axi.getDescriptionBox.abbrevIRI()}; "+
            s"definitions tbox: ${axi.getClosedWorldDefinitions.abbrevIRI()}"
        )).left
    }
  }

  @scala.annotation.tailrec
  protected def convertDescriptionBoxRefinements
  (o2r: OMLText2Resolver, axs: Iterable[DescriptionBoxRefinement])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (axs.isEmpty)
    o2r.right
  else {
    val (axi, remaining) = (axs.head, axs.tail)
    (o2r.dboxes.get(axi.getRefiningDescriptionBox), o2r.dboxes.get(axi.getRefinedDescriptionBox)) match {
      case (Some(refining), Some(refined)) =>
        val (rj, axj) = f.createDescriptionBoxRefinement(o2r.rextent, refining, refined)
        val updated = o2r.copy(rextent = rj, moduleEdges = o2r.moduleEdges + (axi -> axj))
        convertDescriptionBoxRefinements(updated, remaining)
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"convertDescriptionBoxRefinements: Failed to resolve "+
            s"refining: ${axi.getRefiningDescriptionBox.abbrevIRI()}; "+
            s"refined: ${axi.getRefinedDescriptionBox.abbrevIRI()}"
        )).left
    }
  }

  protected def convertAspects
  (o2r: OMLText2Resolver, as: Set[Aspect])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = {
    val result = as.foldLeft[OMLText2Resolver](o2r) { case (ri, ai) =>
      val (rj, aj) = f.createAspect(ri.rextent, ri.tboxes(ai.getTbox), ai.name())
      ri.copy(rextent = rj, aspects = ri.aspects + (ai -> aj))
    }
    result.right
  }

  protected def convertConcepts
  (o2r: OMLText2Resolver, cs: Set[Concept])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = {
    val result = cs.foldLeft[OMLText2Resolver](o2r) { case (ri, ci) =>
      val (rj, cj) = f.createConcept(ri.rextent, ri.tboxes(ci.getTbox), ci.name())
      ri.copy(rextent = rj, concepts = ri.concepts + (ci -> cj))
    }
    result.right
  }

  @scala.annotation.tailrec
  protected def convertReifiedRelationships
  (o2r: OMLText2Resolver, rrs: Set[ReifiedRelationship])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (rrs.nonEmpty) {
    rrs
      .map { rr => Tuple3(rr, o2r.entityLookup(rr.getSource), o2r.entityLookup(rr.getTarget)) }
      .find { t3 => t3._2.isDefined && t3._3.isDefined } match {
      case Some((rri, Some(isource), Some(itarget))) =>
        val (extj, rrj) = f.createReifiedRelationship(
          extent = o2r.rextent,
          tbox = o2r.tboxes(rri.getTbox),
          source = isource,
          target = itarget,
          isAsymmetric = rri.isIsAsymmetric,
          isEssential = rri.isIsEssential,
          isFunctional = rri.isIsFunctional,
          isInverseEssential = rri.isIsInverseEssential,
          isInverseFunctional = rri.isIsInverseFunctional,
          isIrreflexive = rri.isIsIrreflexive,
          isReflexive = rri.isIsReflexive,
          isSymmetric = rri.isIsSymmetric,
          isTransitive = rri.isIsTransitive,
          name = rri.name(),
          unreifiedPropertyName = rri.getUnreifiedPropertyName,
          unreifiedInversePropertyName = Option.apply(rri.getUnreifiedInversePropertyName))
        val remaining = rrs - rri
        val updated = o2r.copy(rextent = extj, reifiedRelationships = o2r.reifiedRelationships + (rri -> rrj))
        if (remaining.isEmpty)
          updated.right
        else
          convertReifiedRelationships(updated, remaining)
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"Cannot find any convertible ReifiedRelationship among ${rrs.size}: " +
            rrs.map(_.abbrevIRI()).mkString(", "))).left
    }
  } else
    o2r.right

  @scala.annotation.tailrec
  protected def convertUnreifiedRelationships
  (o2r: OMLText2Resolver, urs: Set[UnreifiedRelationship])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (urs.nonEmpty) {
    urs
      .map { ur => Tuple3(ur, o2r.entityLookup(ur.getSource), o2r.entityLookup(ur.getTarget)) }
      .find { t3 => t3._2.isDefined && t3._3.isDefined } match {
      case Some((uri, Some(isource), Some(itarget))) =>
        val (extj, urj) = f.createUnreifiedRelationship(
          extent = o2r.rextent,
          tbox = o2r.tboxes(uri.getTbox),
          source = isource,
          target = itarget,
          isAsymmetric = uri.isIsAsymmetric,
          isEssential = uri.isIsEssential,
          isFunctional = uri.isIsFunctional,
          isInverseEssential = uri.isIsInverseEssential,
          isInverseFunctional = uri.isIsInverseFunctional,
          isIrreflexive = uri.isIsIrreflexive,
          isReflexive = uri.isIsReflexive,
          isSymmetric = uri.isIsSymmetric,
          isTransitive = uri.isIsTransitive,
          name = uri.name())
        val remaining = urs - uri
        val updated = o2r.copy(rextent = extj, unreifiedRelationships = o2r.unreifiedRelationships + (uri -> urj))
        if (remaining.isEmpty)
          updated.right
        else
          convertUnreifiedRelationships(updated, remaining)
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"Cannot find any convertible ReifiedRelationship among ${urs.size}: " +
            urs.map(_.abbrevIRI()).mkString(", "))).left
    }
  } else
    o2r.right

  protected def convertStructures
  (o2r: OMLText2Resolver, scs: Set[Structure])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = {
    val result = scs.foldLeft[OMLText2Resolver](o2r) { case (ri, sci) =>
      val (rj, scj) = f.createStructure(ri.rextent, ri.tboxes(sci.getTbox), sci.name())
      ri.copy(rextent = rj, structures = ri.structures + (sci -> scj))
    }
    result.right
  }

  protected def convertScalars
  (o2r: OMLText2Resolver, scs: Set[Scalar])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = {
    val result = scs.foldLeft[OMLText2Resolver](o2r) { case (ri, sci) =>
      val (rj, scj) = f.createScalar(ri.rextent, ri.tboxes(sci.getTbox), sci.name())
      ri.copy(rextent = rj, dataRanges = ri.dataRanges + (sci -> scj))
    }
    result.right
  }

  @scala.annotation.tailrec
  protected def convertRestrictedDataRanges
  (o2r: OMLText2Resolver, rdrs: Set[RestrictedDataRange])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (rdrs.nonEmpty) {
    rdrs
      .map { rdr => Tuple2(rdr, o2r.dataRanges.get(rdr.getRestrictedRange)) }
      .find { t2 => t2._2.isDefined } match {
      case Some((rdri, Some(rsi))) =>
        val (extj, rdrj) = rdri match {
          case rri: BinaryScalarRestriction =>
            f.createBinaryScalarRestriction(
              extent = o2r.rextent,
              tbox = o2r.tboxes(rdri.getTbox),
              restrictedRange = rsi,
              length = Option.apply(rri.getLength),
              minLength = Option.apply(rri.getMinLength),
              maxLength = Option.apply(rri.getMaxLength),
              name = rri.name)
          case rri: IRIScalarRestriction =>
            f.createIRIScalarRestriction(
              extent = o2r.rextent,
              tbox = o2r.tboxes(rdri.getTbox),
              restrictedRange = rsi,
              length = Option.apply(rri.getLength),
              minLength = Option.apply(rri.getMinLength),
              maxLength = Option.apply(rri.getMaxLength),
              pattern = Option.apply(rri.getPattern),
              name = rri.name)
          case rri: NumericScalarRestriction =>
            f.createNumericScalarRestriction(
              extent = o2r.rextent,
              tbox = o2r.tboxes(rdri.getTbox),
              restrictedRange = rsi,
              minExclusive = Option.apply(rri.getMinExclusive),
              minInclusive = Option.apply(rri.getMinInclusive),
              maxExclusive = Option.apply(rri.getMaxExclusive),
              maxInclusive = Option.apply(rri.getMaxInclusive),
              name = rri.name)
          case rri: PlainLiteralScalarRestriction =>
            f.createPlainLiteralScalarRestriction(
              extent = o2r.rextent,
              tbox = o2r.tboxes(rdri.getTbox),
              restrictedRange = rsi,
              length = Option.apply(rri.getLength),
              minLength = Option.apply(rri.getMinLength),
              maxLength = Option.apply(rri.getMaxLength),
              pattern = Option.apply(rri.getPattern),
              langRange = Option.apply(rri.getLangRange),
              name = rri.name)
          case rri: ScalarOneOfRestriction =>
            f.createScalarOneOfRestriction(
              extent = o2r.rextent,
              tbox = o2r.tboxes(rdri.getTbox),
              restrictedRange = rsi,
              name = rri.name)
          case rri: StringScalarRestriction =>
            f.createStringScalarRestriction(
              extent = o2r.rextent,
              tbox = o2r.tboxes(rdri.getTbox),
              restrictedRange = rsi,
              length = Option.apply(rri.getLength),
              minLength = Option.apply(rri.getMinLength),
              maxLength = Option.apply(rri.getMaxLength),
              pattern = Option.apply(rri.getPattern),
              name = rri.name)
          case rri: SynonymScalarRestriction =>
            f.createSynonymScalarRestriction(
              extent = o2r.rextent,
              tbox = o2r.tboxes(rdri.getTbox),
              restrictedRange = rsi,
              name = rri.name)
          case rri: TimeScalarRestriction =>
            f.createTimeScalarRestriction(
              extent = o2r.rextent,
              tbox = o2r.tboxes(rdri.getTbox),
              restrictedRange = rsi,
              minExclusive = Option.apply(rri.getMinExclusive),
              minInclusive = Option.apply(rri.getMinInclusive),
              maxExclusive = Option.apply(rri.getMaxExclusive),
              maxInclusive = Option.apply(rri.getMaxInclusive),
              name = rri.name)
        }
        val remaining = rdrs - rdri
        val updated = o2r.copy(rextent = extj, dataRanges = o2r.dataRanges + (rdri -> rdrj))
        if (remaining.isEmpty)
          updated.right
        else
          convertRestrictedDataRanges(updated, remaining)
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"Cannot find any convertible RestrictedDataRange among ${rdrs.size}: " +
            rdrs.map(_.abbrevIRI()).mkString(", "))).left
    }
  } else
    o2r.right

  @scala.annotation.tailrec
  protected def convertScalarOneOfLiteralAxioms
  (o2r: OMLText2Resolver, lits: Set[ScalarOneOfLiteralAxiom])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (lits.isEmpty)
    o2r.right
  else {
    val (li, remaining) = (lits.head, lits.tail)
    o2r.dataRanges.get(li.getAxiom) match {
      case Some(sc: api.ScalarOneOfRestriction) =>
        val (rj, lj) = f.createScalarOneOfLiteralAxiom(
          o2r.rextent,
          o2r.tboxes(li.getTbox),
          sc,
          li.getValue)
        val next = o2r.copy(
          rextent = rj,
          scalarOneOfLiterals = o2r.scalarOneOfLiterals + (li -> lj),
          termAxioms = o2r.termAxioms + (li -> lj))
        convertScalarOneOfLiteralAxioms(next, remaining)
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"convertScalarOneLiteralAxioms: Cannot find ScalarOneOfRestriction for ${li.getAxiom.abbrevIRI()}"
        )).left
    }
  }

  @scala.annotation.tailrec
  protected def convertEntityScalarDataProperties
  (o2r: OMLText2Resolver, dps: Set[EntityScalarDataProperty])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (dps.isEmpty)
    o2r.right
  else {
    val (dpi, remaining) = (dps.head, dps.tail)
    (o2r.entityLookup(dpi.getDomain), o2r.dataRanges.get(dpi.getRange)) match {
      case (Some(domain: api.Entity), Some(range: api.DataRange)) =>
        val (rj, dpj) = f.createEntityScalarDataProperty(
          o2r.rextent,
          o2r.tboxes(dpi.getTbox),
          domain, range,
          dpi.isIsIdentityCriteria,
          dpi.name())
        val next = o2r.copy(rextent = rj, entityScalarDataProperties = o2r.entityScalarDataProperties + (dpi -> dpj))
        convertEntityScalarDataProperties(next, remaining)
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"convertEntityScalarDataProperties: Cannot find EntityScalarDataProperty for ${dpi.abbrevIRI()}"
        )).left
    }
  }

  @scala.annotation.tailrec
  protected def convertEntityStructuredDataProperties
  (o2r: OMLText2Resolver, dps: Set[EntityStructuredDataProperty])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (dps.isEmpty)
    o2r.right
  else {
    val (dpi, remaining) = (dps.head, dps.tail)
    (o2r.entityLookup(dpi.getDomain), o2r.structures.get(dpi.getRange)) match {
      case (Some(domain: api.Entity), Some(range: api.Structure)) =>
        val (rj, dpj) = f.createEntityStructuredDataProperty(
          o2r.rextent,
          o2r.tboxes(dpi.getTbox),
          domain, range,
          dpi.isIsIdentityCriteria,
          dpi.name())
        val next = o2r.copy(rextent = rj, entityStructuredDataProperties = o2r.entityStructuredDataProperties + (dpi -> dpj))
        convertEntityStructuredDataProperties(next, remaining)
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"convertEntityStructuredDataProperties: Cannot find EntityStructuredDataProperty for ${dpi.abbrevIRI()}"
        )).left
    }
  }

  @scala.annotation.tailrec
  protected def convertScalarDataProperties
  (o2r: OMLText2Resolver, dps: Set[ScalarDataProperty])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (dps.isEmpty)
    o2r.right
  else {
    val (dpi, remaining) = (dps.head, dps.tail)
    (o2r.structures.get(dpi.getDomain), o2r.dataRanges.get(dpi.getRange)) match {
      case (Some(domain: api.Structure), Some(range: api.DataRange)) =>
        val (rj, dpj) = f.createScalarDataProperty(
          o2r.rextent,
          o2r.tboxes(dpi.getTbox),
          domain, range,
          dpi.name())
        val next = o2r.copy(rextent = rj, scalarDataProperties = o2r.scalarDataProperties + (dpi -> dpj))
        convertScalarDataProperties(next, remaining)
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"convertScalarDataProperties: Cannot find ScalarDataProperty for ${dpi.abbrevIRI()}"
        )).left
    }
  }

  @scala.annotation.tailrec
  protected def convertStructuredDataProperties
  (o2r: OMLText2Resolver, dps: Set[StructuredDataProperty])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (dps.isEmpty)
    o2r.right
  else {
    val (dpi, remaining) = (dps.head, dps.tail)
    (o2r.structures.get(dpi.getDomain), o2r.structures.get(dpi.getRange)) match {
      case (Some(domain: api.Structure), Some(range: api.Structure)) =>
        val (rj, dpj) = f.createStructuredDataProperty(
          o2r.rextent,
          o2r.tboxes(dpi.getTbox),
          domain, range,
          dpi.name())
        val next = o2r.copy(rextent = rj, structuredDataProperties = o2r.structuredDataProperties + (dpi -> dpj))
        convertStructuredDataProperties(next, remaining)
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"convertStructuredDataProperties: Cannot find StructuredDataProperty for ${dpi.abbrevIRI()}"
        )).left
    }
  }

  @scala.annotation.tailrec
  protected def convertEntityRestrictionAxioms
  (o2r: OMLText2Resolver, axs: Set[EntityRestrictionAxiom])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (axs.isEmpty)
    o2r.right
  else {
    val (axi, remaining) = (axs.head, axs.tail)
    ( o2r.entityLookup(axi.getRestrictedDomain),
      o2r.entityLookup(axi.getRestrictedRange),
      o2r.entityRelationshipLookup(axi.getRestrictedRelation)) match {
      case (Some(domain), Some(range), Some(rl)) =>
        val (rj, axj) = axi match {
          case _: EntityExistentialRestrictionAxiom =>
            f.createEntityExistentialRestrictionAxiom(o2r.rextent, o2r.tboxes(axi.getTbox), rl, domain, range)
          case _: EntityUniversalRestrictionAxiom =>
            f.createEntityUniversalRestrictionAxiom(o2r.rextent, o2r.tboxes(axi.getTbox), rl, domain, range)
        }
        val next = o2r.copy(rextent = rj, termAxioms = o2r.termAxioms + (axi -> axj))
        convertEntityRestrictionAxioms(next, remaining)
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"convertEntityRestrictionAxioms: Cannot find EntityRestrictionAxiom for"+
            s": domain: ${axi.getRestrictedDomain.abbrevIRI()}" +
            s", relation: ${axi.getRestrictedRelation.abbrevIRI()}" +
            s", range: ${axi.getRestrictedRange.abbrevIRI()}"
        )).left
    }
  }

  @scala.annotation.tailrec
  protected def convertEntityScalarDataPropertyRestrictionAxioms
  (o2r: OMLText2Resolver, axs: Set[EntityScalarDataPropertyRestrictionAxiom])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (axs.isEmpty)
    o2r.right
  else {
    val (axi, remaining) = (axs.head, axs.tail)
    ( o2r.entityLookup(axi.getRestrictedEntity),
      o2r.entityScalarDataProperties.get(axi.getScalarProperty)) match {
      case (Some(e), Some(dp)) =>
        val (rj, axj: \/[EMFProblems, api.EntityScalarDataPropertyRestrictionAxiom]) = axi match {
          case axpi: EntityScalarDataPropertyParticularRestrictionAxiom =>
            f
              .createEntityScalarDataPropertyParticularRestrictionAxiom(
                o2r.rextent, o2r.tboxes(axi.getTbox), e, dp, axpi.getLiteralValue) match { case (rk, ak) => rk -> ak.right }
          case axui: EntityScalarDataPropertyExistentialRestrictionAxiom =>
            o2r.dataRanges.get(axui.getScalarRestriction) match {
              case Some(dr) =>
                f
                  .createEntityScalarDataPropertyExistentialRestrictionAxiom(
                    o2r.rextent, o2r.tboxes(axi.getTbox), e, dp, dr) match { case (rk, ak) => rk -> ak.right }
              case _ =>
                o2r.rextent -> new EMFProblems(new java.lang.IllegalArgumentException(
                  s"convertEntityScalarDataPropertyRestrictionAxioms: "+
                    s"Cannot find DataRange: ${axui.getScalarRestriction.abbrevIRI()}"
                )).left
            }
          case axui: EntityScalarDataPropertyUniversalRestrictionAxiom =>
            o2r.dataRanges.get(axui.getScalarRestriction) match {
              case Some(dr) =>
                f
                  .createEntityScalarDataPropertyUniversalRestrictionAxiom(
                    o2r.rextent, o2r.tboxes(axi.getTbox), e, dp, dr) match { case (rk, ak) => rk -> ak.right }
              case _ =>
                o2r.rextent -> new EMFProblems(new java.lang.IllegalArgumentException(
                  s"convertEntityScalarDataPropertyRestrictionAxioms: "+
                    s"Cannot find DataRange: ${axui.getScalarRestriction.abbrevIRI()}"
                )).left
            }
        }
        axj match {
          case \/-(axrj) =>
            val next = o2r.copy(rextent = rj, termAxioms = o2r.termAxioms + (axi -> axrj))
            convertEntityScalarDataPropertyRestrictionAxioms(next, remaining)
          case -\/(error) =>
            error.left
        }
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"convertEntityScalarDataPropertyRestrictionAxioms: Cannot find EntityScalarDataPropertyRestrictionAxiom for"+
            s": restricted entity: ${axi.getRestrictedEntity.abbrevIRI()}" +
            s", scalar data property: ${axi.getScalarProperty.abbrevIRI()}"
        )).left
    }
  }

  @scala.annotation.tailrec
  protected def convertSpecializationAxioms
  (o2r: OMLText2Resolver, axs: Set[SpecializationAxiom])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (axs.isEmpty)
    o2r.right
  else {
    val (axi, remaining) = (axs.head, axs.tail)
    val (rj, axj: \/[EMFProblems, api.SpecializationAxiom]) =
      ( axi,
      o2r.tboxes.get(axi.getTbox),
      o2r.entityLookup(axi.parent),
      o2r.entityLookup(axi.child) ) match {
      case (_:ConceptSpecializationAxiom, Some(tbox), Some(sup: api.Concept), Some(sub: api.Concept)) =>
        f.createConceptSpecializationAxiom(o2r.rextent, tbox, sup, sub) match {
          case (rk, ak) => rk -> ak.right }
      case (_:AspectSpecializationAxiom, Some(tbox), Some(sup: api.Aspect), Some(sub)) =>
        f.createAspectSpecializationAxiom(o2r.rextent, tbox, sup, sub) match {
          case (rk, ak) => rk -> ak.right }
      case (_:ReifiedRelationshipSpecializationAxiom, Some(tbox), Some(sup: api.ReifiedRelationship), Some(sub: api.ReifiedRelationship)) =>
        f.createReifiedRelationshipSpecializationAxiom(o2r.rextent, tbox, sup, sub) match {
          case (rk, ak) => rk -> ak.right }
      case _ =>
        o2r.rextent -> new EMFProblems(new java.lang.IllegalArgumentException(
          s"convertSpecializationAxioms: Cannot find: "+
            s"tbox: ${axi.getTbox.abbrevIRI()}"+
            s"general: ${axi.parent.abbrevIRI()}"+
            s"specific: ${axi.child.abbrevIRI()}"
        )).left
      }
    axj match {
      case \/-(axrj) =>
        val next = o2r.copy(rextent = rj, termAxioms = o2r.termAxioms + (axi -> axrj))
        convertSpecializationAxioms(next, remaining)
      case -\/(error) =>
        error.left
    }
  }

  @scala.annotation.tailrec
  protected def convertRootConceptTaxonomyAxioms
  (o2r: OMLText2Resolver, axs: Set[RootConceptTaxonomyAxiom])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (axs.isEmpty)
    o2r.right
  else {
    val (axi, remaining) = (axs.head, axs.tail)
    convertConceptTreeDisjunction(o2r, axi) match {
      case \/-(next) =>
        convertRootConceptTaxonomyAxioms(next, remaining)
      case -\/(error) =>
        error.left
    }
  }

  protected def convertConceptTreeDisjunction
  (o2r: OMLText2Resolver, ax: ConceptTreeDisjunction)
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = ax match {
    case rxi: RootConceptTaxonomyAxiom =>
      (o2r.tboxes.get(rxi.bundleContainer), o2r.concepts.get(rxi.getRoot)) match {
        case (Some(b: api.Bundle), Some(c)) =>
          val (rj, rxj) = f.createRootConceptTaxonomyAxiom(o2r.rextent, b, c)
          val next = o2r.copy(
            rextent = rj,
            conceptTreeDisjunctions = o2r.conceptTreeDisjunctions + (rxi -> rxj))
          convertDisjointUnionOfConceptAxioms(next, rxi.getDisjunctions.asScala.to[Set].map { dx => dx -> rxj })
        case _ =>
          new EMFProblems(new java.lang.IllegalArgumentException(
            s"convertConceptTreeDisjunction: Cannot find: " +
              s"bundle: ${rxi.bundleContainer.abbrevIRI()}" +
              s"root C: ${rxi.getRoot.abbrevIRI()}"
          )).left
      }
    case axi: AnonymousConceptUnionAxiom =>
      o2r.conceptTreeDisjunctions.get(axi.conceptTreeDisjunctionParent) match {
        case Some(p) =>
          val (rj, axj) = f.createAnonymousConceptUnionAxiom(o2r.rextent, p, axi.getName)
          val next = o2r.copy(
            rextent = rj,
            conceptTreeDisjunctions = o2r.conceptTreeDisjunctions + (axi -> axj),
            disjointUnionOfConceptAxioms = o2r.disjointUnionOfConceptAxioms + (axi -> axj))
          convertDisjointUnionOfConceptAxioms(next, axi.getDisjunctions.asScala.to[Set].map { dx => dx -> axj })
        case _ =>
          new EMFProblems(new java.lang.IllegalArgumentException(
            s"convertConceptTreeDisjunction: Cannot find: " +
              s"parent: ${axi.conceptTreeDisjunctionParent.uuid}"
          )).left
      }
    }

  @scala.annotation.tailrec
  protected def convertDisjointUnionOfConceptAxioms
  (o2r: OMLText2Resolver, dxs: Set[(DisjointUnionOfConceptsAxiom, api.ConceptTreeDisjunction)])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (dxs.isEmpty)
    o2r.right
  else {
    val ((dxi, cxj), remaining) = (dxs.head, dxs.tail)
    dxi match {
      case axi: AnonymousConceptUnionAxiom =>
        val (rj, axj) = f.createAnonymousConceptUnionAxiom(o2r.rextent, cxj, axi.getName)
        val next = o2r.copy(
          rextent = rj,
          conceptTreeDisjunctions = o2r.conceptTreeDisjunctions + (axi -> axj),
          disjointUnionOfConceptAxioms = o2r.disjointUnionOfConceptAxioms + (axi -> axj))
        convertDisjointUnionOfConceptAxioms(next, remaining ++ axi.getDisjunctions.asScala.to[Set].map { dx => dx -> axj })
      case sxi: SpecificDisjointConceptAxiom =>
        o2r.concepts.get(sxi.getDisjointLeaf) match {
          case Some(leaf) =>
            val (rj, sxj) = f.createSpecificDisjointConceptAxiom(o2r.rextent, cxj, leaf)
            val next = o2r.copy(
              rextent = rj,
              disjointUnionOfConceptAxioms = o2r.disjointUnionOfConceptAxioms + (sxi -> sxj))
            convertDisjointUnionOfConceptAxioms(next, remaining)
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertDisjointUnionOfConceptAxioms: Cannot find: " +
                s"leaf concept: ${sxi.getDisjointLeaf.abbrevIRI}"
            )).left

        }
    }
  }

  @scala.annotation.tailrec
  protected def convertConceptInstances
  (o2r: OMLText2Resolver, cxs: List[(DescriptionBox, api.DescriptionBox, List[ConceptInstance])])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (cxs.isEmpty)
      o2r.right
  else {
    val ((di, dj, cis), remaining) = (cxs.head, cxs.tail)
    cis match {
      case ci :: cit =>
        o2r.concepts.get(ci.getSingletonConceptClassifier) match {
          case Some(ck) =>
            val (rj, cj) = f.createConceptInstance(o2r.rextent, dj, ck, ci.getName)
            val next = o2r.copy(
              rextent = rj,
              conceptualEntitySingletonInstances = o2r.conceptualEntitySingletonInstances + (ci -> cj))
            convertConceptInstances(next, (di, dj, cit) :: remaining)
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertConceptInstances: Cannot find: " +
                s"concept: ${ci.getSingletonConceptClassifier.abbrevIRI}"
            )).left
        }
      case _ =>
        convertConceptInstances(o2r, remaining)
    }
  }

  @scala.annotation.tailrec
  protected def convertReifiedRelationshipInstances
  (o2r: OMLText2Resolver, rrxs: List[(DescriptionBox, api.DescriptionBox, List[ReifiedRelationshipInstance])])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (rrxs.isEmpty)
    o2r.right
  else {
    val ((di, dj, rris), remaining) = (rrxs.head, rrxs.tail)
    rris match {
      case rri :: rrit =>
        o2r.reifiedRelationships.get(rri.getSingletonReifiedRelationshipClassifier) match {
          case Some(rrk) =>
            val (rj, rrj) = f.createReifiedRelationshipInstance(o2r.rextent, dj, rrk, rri.getName)
            val next = o2r.copy(
              rextent = rj,
              conceptualEntitySingletonInstances = o2r.conceptualEntitySingletonInstances + (rri -> rrj))
            convertReifiedRelationshipInstances(next, (di, dj, rrit) :: remaining)
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertReifiedRelationshipInstances: Cannot find: " +
                s"concept: ${rri.getSingletonReifiedRelationshipClassifier.abbrevIRI}"
            )).left
        }
      case _ =>
        convertReifiedRelationshipInstances(o2r, remaining)
    }
  }

  @scala.annotation.tailrec
  protected def convertReifiedRelationshipInstanceDomains
  (o2r: OMLText2Resolver, rrxs: List[(DescriptionBox, api.DescriptionBox, List[ReifiedRelationshipInstanceDomain])])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (rrxs.isEmpty)
    o2r.right
  else {
    val ((di, dj, rris), remaining) = (rrxs.head, rrxs.tail)
    rris match {
      case rri :: rrit =>
        ( o2r.conceptualEntitySingletonInstances.get(rri.getReifiedRelationshipInstance),
          o2r.conceptualEntitySingletonInstances.get(rri.getDomain) ) match {
          case ( Some(rrk: api.ReifiedRelationshipInstance), Some(rrdj) ) =>
            val (rj, _) = f.createReifiedRelationshipInstanceDomain(o2r.rextent, dj, rrk, rrdj)
            val next = o2r.copy(rextent = rj)
            convertReifiedRelationshipInstanceDomains(next, (di, dj, rrit) :: remaining)
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertReifiedRelationshipInstanceDomains: Cannot find: " +
                s"reified relationship instance: ${rri.getReifiedRelationshipInstance.abbrevIRI}" +
                s"reified relationship domain: ${rri.getDomain.abbrevIRI}"
            )).left
        }
      case _ =>
        convertReifiedRelationshipInstanceDomains(o2r, remaining)
    }
  }

  @scala.annotation.tailrec
  protected def convertReifiedRelationshipInstanceRanges
  (o2r: OMLText2Resolver, rrxs: List[(DescriptionBox, api.DescriptionBox, List[ReifiedRelationshipInstanceRange])])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (rrxs.isEmpty)
    o2r.right
  else {
    val ((di, dj, rris), remaining) = (rrxs.head, rrxs.tail)
    rris match {
      case rri :: rrit =>
        ( o2r.conceptualEntitySingletonInstances.get(rri.getReifiedRelationshipInstance),
          o2r.conceptualEntitySingletonInstances.get(rri.getRange) ) match {
          case ( Some(rrk: api.ReifiedRelationshipInstance), Some(rrdj) ) =>
            val (rj, _) = f.createReifiedRelationshipInstanceRange(o2r.rextent, dj, rrk, rrdj)
            val next = o2r.copy(rextent = rj)
            convertReifiedRelationshipInstanceRanges(next, (di, dj, rrit) :: remaining)
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertReifiedRelationshipInstanceRanges: Cannot find: " +
                s"reified relationship instance: ${rri.getReifiedRelationshipInstance.abbrevIRI}" +
                s"reified relationship range: ${rri.getRange.abbrevIRI}"
            )).left
        }
      case _ =>
        convertReifiedRelationshipInstanceRanges(o2r, remaining)
    }
  }

  @scala.annotation.tailrec
  protected def convertUnreifiedRelationshipInstances
  (o2r: OMLText2Resolver, urxs: List[(DescriptionBox, api.DescriptionBox, List[UnreifiedRelationshipInstanceTuple])])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (urxs.isEmpty)
    o2r.right
  else {
    val ((di, dj, uris), remaining) = (urxs.head, urxs.tail)
    uris match {
      case uri :: urit =>
        ( o2r.conceptualEntitySingletonInstances.get(uri.getDomain),
          o2r.conceptualEntitySingletonInstances.get(uri.getRange),
          o2r.unreifiedRelationships.get(uri.getUnreifiedRelationship) ) match {
          case ( Some(urjd), Some(urjr), Some(urj) ) =>
            val (rj, _) = f.createUnreifiedRelationshipInstanceTuple(o2r.rextent, dj, urj, urjd, urjr)
            val next = o2r.copy(rextent = rj)
            convertUnreifiedRelationshipInstances(next, (di, dj, urit) :: remaining)
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertUnreifiedRelationshipInstances: Cannot find: " +
                s"domain: ${uri.getDomain.abbrevIRI}" +
                s"range: ${uri.getRange.abbrevIRI}" +
                s"unreified relationship: ${uri.getUnreifiedRelationship.abbrevIRI}"
            )).left
        }
      case _ =>
        convertUnreifiedRelationshipInstances(o2r, remaining)
    }
  }

  @scala.annotation.tailrec
  protected def convertSingletonScalarDataPropertyValues
  (o2r: OMLText2Resolver, vs: List[(DescriptionBox, api.DescriptionBox, List[SingletonInstanceScalarDataPropertyValue])])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (vs.isEmpty)
    o2r.right
  else {
    val ((di, dj, vis), remaining) = (vs.head, vs.tail)
    vis match {
      case vi :: vit =>
        ( o2r.conceptualEntitySingletonInstances.get(vi.getSingletonInstance),
          o2r.entityScalarDataProperties.get(vi.getScalarDataProperty) ) match {
          case ( Some(cj), Some(scj) ) =>
            val (rj, _) = f.createSingletonInstanceScalarDataPropertyValue(o2r.rextent, dj, cj, scj, vi.getScalarPropertyValue)
            val next = o2r.copy(rextent = rj)
            convertSingletonScalarDataPropertyValues(next, (di, dj, vit) :: remaining)
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertSingletonScalarDataPropertyValues: Cannot find: " +
                s"singleton instance: ${vi.getSingletonInstance.abbrevIRI}" +
                s"scalar data property: ${vi.getScalarDataProperty.abbrevIRI}"
            )).left
        }
      case _ =>
        convertSingletonScalarDataPropertyValues(o2r, remaining)
    }
  }

  @scala.annotation.tailrec
  protected def convertSingletonStructuredDataPropertyValues
  (o2r: OMLText2Resolver, vs: List[(DescriptionBox, api.DescriptionBox, List[SingletonInstanceStructuredDataPropertyValue])])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (vs.isEmpty)
    o2r.right
  else {
    val ((di, dj, vis), remaining) = (vs.head, vs.tail)
    vis match {
      case vi :: vit =>
        ( o2r.conceptualEntitySingletonInstances.get(vi.getSingletonInstance),
          o2r.dataRelationshipToStructureLookup(vi.getStructuredDataProperty) ) match {
          case ( Some(cj), Some(scj: api.StructuredDataProperty) ) =>
            val (rj, vj) = f.createSingletonInstanceStructuredDataPropertyValue(o2r.rextent, dj, cj, scj)
            convertSingletonInstanceStructuredDataPropertyContext(
              o2r.copy(rextent = rj),
              vi.getScalarDataPropertyValues.asScala.to[Seq].map(vj -> _),
              vi.getStructuredPropertyTuples.asScala.to[Seq].map(vj -> _)) match {
              case \/-(next) =>
                convertSingletonStructuredDataPropertyValues(next, (di, dj, vit) :: remaining)
              case -\/(error) =>
                error.left
            }
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertSingletonStructuredDataPropertyValues: Cannot find: " +
                s"singleton instance: ${vi.getSingletonInstance.abbrevIRI}" +
                s"structured data property: ${vi.getStructuredDataProperty.abbrevIRI}"
            )).left
        }
      case _ =>
        convertSingletonStructuredDataPropertyValues(o2r, remaining)
    }
  }

  @scala.annotation.tailrec
  protected def convertSingletonInstanceStructuredDataPropertyContext
  (o2r: OMLText2Resolver,
   scs: Seq[(api.SingletonInstanceStructuredDataPropertyContext, ScalarDataPropertyValue)],
   sts: Seq[(api.SingletonInstanceStructuredDataPropertyContext, StructuredDataPropertyTuple)])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (sts.nonEmpty) {
    val ((ctx, sti), stt) = (sts.head, sts.tail)
    o2r.dataRelationshipToStructureLookup(sti.getStructuredDataProperty) match {
      case Some(stk) =>
        val (rj, stj) = f.createStructuredDataPropertyTuple(o2r.rextent, stk, ctx)
        convertSingletonInstanceStructuredDataPropertyContext(
          o2r.copy(rextent = rj),
          scs ++ sti.getScalarDataPropertyValues.asScala.to[Seq].map(stj -> _),
          stt ++ sti.getStructuredPropertyTuples.asScala.to[Seq].map(stj -> _))
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"convertSingletonInstanceStructuredDataPropertyContext: Cannot find: " +
            s"structured data property: ${sti.getStructuredDataProperty.abbrevIRI}"
        )).left
    }
  } else if (scs.nonEmpty) {
    val ((ctx, sci), sct) = (scs.head, scs.tail)
    o2r.dataRelationshipToScalarLookup(sci.getScalarDataProperty) match {
      case Some(sck) =>
        val (rj, _) = f.createScalarDataPropertyValue(o2r.rextent, sck, sci.getScalarPropertyValue, ctx)
        val next = o2r.copy(rextent = rj)
        convertSingletonInstanceStructuredDataPropertyContext(next, sct, Nil)
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"convertSingletonInstanceStructuredDataPropertyContext: Cannot find: " +
            s"scalar data property: ${sci.getScalarDataProperty.abbrevIRI}"
        )).left
    }
  } else
    o2r.right

}
