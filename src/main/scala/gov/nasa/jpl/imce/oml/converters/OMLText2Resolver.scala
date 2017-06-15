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

import gov.nasa.jpl.imce.oml.converters.utils.EMFProblems
import gov.nasa.jpl.imce.oml.model.bundles._
import gov.nasa.jpl.imce.oml.model.common._
import gov.nasa.jpl.imce.oml.model.descriptions._
import gov.nasa.jpl.imce.oml.model.graphs._
import gov.nasa.jpl.imce.oml.model.terminologies._
import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.tables.{ClosedWorldDesignations, OpenWorldDefinitions, TerminologyKind => TTerminologyKind}
import gov.nasa.jpl.imce.oml.tables.{Final, Partial, DescriptionKind => TDescriptionKind}

import scala.collection.JavaConverters._
import scala.collection.immutable._
import scala.Predef.{ArrowAssoc, augmentString}
import scala.{None, Option, Some, StringContext}
import scalaz._
import Scalaz._

case class OMLText2Resolver
(omlFile: File,
 rextent: api.Extent,

 tboxes: Map[TerminologyBox, api.TerminologyBox] = Map.empty,
 dboxes: Map[DescriptionBox, api.DescriptionBox] = Map.empty,

 moduleEdges: Map[ModuleEdge, api.ModuleEdge] = Map.empty,

 aps: Map[AnnotationProperty, api.AnnotationProperty] = Map.empty,

// TerminologyBoxStatements

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

 termAxioms: Map[TermAxiom, api.TermAxiom] = Map.empty,

 // statements in Bundles
 conceptTreeDisjunctions
 : Map[ConceptTreeDisjunction, api.ConceptTreeDisjunction]
 = Map.empty,

 disjointUnionOfConceptAxioms
 : Map[DisjointUnionOfConceptsAxiom, api.DisjointUnionOfConceptsAxiom]
 = Map.empty,

 // statements in DescriptionBoxes
 conceptualEntitySingletonInstances
 : Map[ConceptualEntitySingletonInstance, api.ConceptualEntitySingletonInstance]
 = Map.empty,

 reifiedRelationshipInstanceDomains
 : Map[ReifiedRelationshipInstanceDomain, api.ReifiedRelationshipInstanceDomain]
 = Map.empty,

 reifiedRelationshipInstanceRanges
 : Map[ReifiedRelationshipInstanceRange, api.ReifiedRelationshipInstanceRange]
 = Map.empty,

 unreifiedRelationshipInstanceTuples
 : Map[UnreifiedRelationshipInstanceTuple, api.UnreifiedRelationshipInstanceTuple]
 = Map.empty,

 singletonInstanceStructuredDataPropertyValues
 : Map[SingletonInstanceStructuredDataPropertyValue, api.SingletonInstanceStructuredDataPropertyValue]
 = Map.empty,

 singletonInstanceScalarDataPropertyValues
 : Map[SingletonInstanceScalarDataPropertyValue, api.SingletonInstanceScalarDataPropertyValue]
 = Map.empty,

 scalarDataPropertyValues
 : Map[ScalarDataPropertyValue, api.ScalarDataPropertyValue]
 = Map.empty,

 structuredDataPropertyTuples
 : Map[StructuredDataPropertyTuple, api.StructuredDataPropertyTuple]
 = Map.empty) {

  def toOMLTablesFile: File = new File(omlFile.getPath.stripSuffix(".oml")+".oml.json.zip")

  def allAccessibleModules: Map[Module, api.Module] =
    tboxes ++ dboxes

  def moduleLookup(m: Module): Option[api.Module] = m match {
    case t: TerminologyBox =>
      tboxes.get(t)
    case d: DescriptionBox =>
      dboxes.get(d)
  }

  def elementLookup(e: Element): Option[api.Element] = e match {
    case x: TerminologyBox =>
      tboxes.get(x)
    case x: DescriptionBox =>
      dboxes.get(x)
    case x: ModuleEdge =>
      moduleEdges.get(x)
    case x: Aspect =>
      aspects.get(x)
    case x: Concept =>
      concepts.get(x)
    case x: ReifiedRelationship =>
      reifiedRelationships.get(x)
    case x: UnreifiedRelationship =>
      unreifiedRelationships.get(x)
    case x: DataRange =>
      dataRanges.get(x)
    case x: Structure =>
      structures.get(x)
    case x: ScalarOneOfLiteralAxiom =>
      scalarOneOfLiterals.get(x)
    case x: EntityScalarDataProperty =>
      entityScalarDataProperties.get(x)
    case x: EntityStructuredDataProperty =>
      entityStructuredDataProperties.get(x)
    case x: ScalarDataProperty =>
      scalarDataProperties.get(x)
    case x: StructuredDataProperty =>
      structuredDataProperties.get(x)
    case x: TermAxiom =>
      termAxioms.get(x)
    case x: ConceptTreeDisjunction =>
      conceptTreeDisjunctions.get(x)
    case x: DisjointUnionOfConceptsAxiom =>
      disjointUnionOfConceptAxioms.get(x)
    case x: ConceptualEntitySingletonInstance =>
      conceptualEntitySingletonInstances.get(x)
    case x: ReifiedRelationshipInstanceDomain =>
      reifiedRelationshipInstanceDomains.get(x)
    case x: ReifiedRelationshipInstanceRange =>
      reifiedRelationshipInstanceRanges.get(x)
    case x: UnreifiedRelationshipInstanceTuple =>
      unreifiedRelationshipInstanceTuples.get(x)
    case x: SingletonInstanceStructuredDataPropertyValue =>
      singletonInstanceStructuredDataPropertyValues.get(x)
    case x: SingletonInstanceScalarDataPropertyValue =>
      singletonInstanceScalarDataPropertyValues.get(x)
    case x: ScalarDataPropertyValue =>
      scalarDataPropertyValues.get(x)
    case x: StructuredDataPropertyTuple =>
      structuredDataPropertyTuples.get(x)
  }

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
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    val (e, _) = entry
    e.getModules.asScala.to[Set].foldLeft(state) { case (acc, mi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        o2rj = mi match {
          case gi: TerminologyGraph =>
            val (rj, gj) = f.createTerminologyGraph(o2ri.rextent, convertTerminologyGraphKind(gi.getKind), gi.iri())
            o2ri.copy(rextent = rj, tboxes = o2ri.tboxes + (gi -> gj))
          case bi: Bundle =>
            val (rj, bj) = f.createBundle(o2ri.rextent, convertTerminologyGraphKind(bi.getKind), bi.iri())
            o2ri.copy(rextent = rj, tboxes = o2ri.tboxes + (bi -> bj))
          case di: DescriptionBox =>
            val (rj, dj) = f.createDescriptionBox(o2ri.rextent, convertDescriptionKind(di.getKind), di.iri())
            o2ri.copy(rextent = rj, dboxes = o2ri.dboxes + (di -> dj))
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertAnnotationProperties
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    val (e, _) = entry
    e.getAnnotationProperties.asScala.to[Set].foldLeft(state) { case (acc, pi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        (rj, pj) = f.createAnnotationProperty(o2ri.rextent, pi.getIri, pi.getAbbrevIRI)
        o2rj = o2ri.copy(rextent = rj, aps = o2ri.aps + (pi -> pj))
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertTerminologyExtensions
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val axs = tboxes.flatMap(_.getBoxAxioms.selectByKindOf { case ax: TerminologyExtensionAxiom => ax })
    axs.foldLeft(state) { case (acc, axi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        extendingi = axi.extendingTerminology()
        extendedi = axi.getExtendedTerminology
        o2rj <-
        ( prev.get(extendingi.getExtent).flatMap(_.tboxes.get(extendingi)),
          prev.get(extendedi.getExtent).flatMap(_.tboxes.get(extendedi)) ) match {
          case ((Some(extendingj), Some(extendedj))) =>
            val (rj, axj) = f.createTerminologyExtensionAxiom(o2ri.rextent, extendingj, extendedj)
            o2ri.copy(rextent = rj, moduleEdges = o2ri.moduleEdges + (axi -> axj)).right
          case ((Some(_), None)) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertTerminologyExtensions: Failed to resolve extended: " + extendedi.getIri
            )).left
          case ((None, Some(_))) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertTerminologyExtensions: Failed to resolve extending: " + extendingi.getIri
            )).left
          case ((None, None)) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertTerminologyExtensions: Failed to resolve extending: " +
                extendingi.getIri +
                " and extended " +
                extendedi.getIri
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertConceptDesignationTerminologyAxioms
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val axs = tboxes.flatMap(_.getBoxAxioms.selectByKindOf { case ax: ConceptDesignationTerminologyAxiom => ax })
    axs.foldLeft(state) { case (acc, axi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        designationi = axi.designationTerminologyGraph()
        designatedi = axi.getDesignatedTerminology
        designatedci = axi.getDesignatedConcept
        o2rj <- (prev.get(designationi.getExtent).flatMap(_.tboxes.get(designationi)),
          prev.get(designatedi.getExtent).flatMap(_.tboxes.get(designatedi)),
          prev.get(designatedci.getTbox.getExtent).flatMap(_.concepts.get(designatedci))) match {
          case (Some(designationG), Some(designatedT), Some(designatedC)) =>
            val (rj, axj) = f.createConceptDesignationTerminologyAxiom(o2ri.rextent, designationG, designatedC, designatedT)
            o2ri.copy(rextent = rj, moduleEdges = o2ri.moduleEdges + (axi -> axj)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertConceptDesignationTerminologyAxioms: Failed to resolve " +
                s"designationG: ${axi.designationTerminologyGraph().abbrevIRI()}; " +
                s"designatedT: ${axi.getDesignatedTerminology.abbrevIRI()}; " +
                s"designatedC: ${axi.getDesignatedConcept.abbrevIRI()}"
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertTerminologyNestingAxioms
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val axs = tboxes.flatMap(_.getBoxAxioms.selectByKindOf { case ax: TerminologyNestingAxiom => ax })
    axs.foldLeft(state) { case (acc, axi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        nestedi = axi.nestedTerminology()
        nestingi = axi.getNestingTerminology
        nestingci = axi.getNestingContext
        o2rj <-
        (prev.get(nestedi.getExtent).flatMap(_.tboxes.get(nestedi)),
          prev.get(nestingi.getExtent).flatMap(_.tboxes.get(nestingi)),
          prev.get(nestingci.getTbox.getExtent).flatMap(_.concepts.get(nestingci))) match {
          case (Some(nestedG: api.TerminologyGraph), Some(nestingT), Some(nestingC)) =>
            val (rj, axj) = f.createTerminologyNestingAxiom(o2ri.rextent, nestedG, nestingT, nestingC)
            o2ri.copy(rextent = rj, moduleEdges = o2ri.moduleEdges + (axi -> axj)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertTerminologyNestingAxioms: Failed to resolve " +
                s"nested G: ${axi.nestedTerminology.abbrevIRI()}; " +
                s"nestingT: ${axi.getNestingTerminology.abbrevIRI()}; " +
                s"nestingC: ${axi.getNestingContext.abbrevIRI()}"
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertBundledTerminologyAxioms
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val bundles = e.getModules.selectByKindOf { case b: Bundle => b }
    val axs = bundles.flatMap(_.getBundleAxioms.selectByKindOf { case ax: BundledTerminologyAxiom => ax })
    axs.foldLeft(state) { case (acc, axi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        bundlei = axi.getBundle
        bundledi = axi.getBundledTerminology
        o2rj <-
        (prev.get(bundlei.getExtent).flatMap(_.tboxes.get(bundlei)),
         prev.get(bundledi.getExtent).flatMap(_.tboxes.get(bundledi))) match {
          case (Some(bundle: api.Bundle), Some(bundled: api.TerminologyBox)) =>
            val (rj, axj) = f.createBundledTerminologyAxiom(o2ri.rextent, bundled, bundle)
            o2ri.copy(rextent = rj, moduleEdges = o2ri.moduleEdges + (axi -> axj)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertBundledTerminologyAxioms: Failed to resolve " +
                s"bundle: ${axi.getBundle.abbrevIRI()}; " +
                s"bundled: ${axi.getBundledTerminology.abbrevIRI()}"
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertDescriptionBoxExtendsClosedWorldDefinitions
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val dboxes = e.getModules.selectByKindOf { case dbox: DescriptionBox => dbox }
    val axs = dboxes.flatMap(_.getClosedWorldDefinitions.asScala.to[Set])
    axs.foldLeft(state) { case (acc, axi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        dboxi = axi.getDescriptionBox
        definitionsi = axi.getClosedWorldDefinitions
        o2rj <-
        (prev.get(dboxi.getExtent).flatMap(_.dboxes.get(dboxi)),
          prev.get(definitionsi.getExtent).flatMap(_.tboxes.get(definitionsi))) match {
          case (Some(dbox), Some(definitions)) =>
            val (rj, axj) = f.createDescriptionBoxExtendsClosedWorldDefinitions(o2ri.rextent, dbox, definitions)
            o2ri.copy(rextent = rj, moduleEdges = o2ri.moduleEdges + (axi -> axj)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertDescriptionBoxExtendsClosedWorldDefinitions: Failed to resolve " +
                s"dbox: ${axi.getDescriptionBox.abbrevIRI()}; " +
                s"definitions tbox: ${axi.getClosedWorldDefinitions.abbrevIRI()}"
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertDescriptionBoxRefinements
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val dboxes = e.getModules.selectByKindOf { case dbox: DescriptionBox => dbox }
    val axs = dboxes.flatMap(_.getDescriptionBoxRefinements.asScala.to[Set])
    axs.foldLeft(state) { case (acc, axi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        refiningi = axi.getRefiningDescriptionBox
        refinedi = axi.getRefinedDescriptionBox
        o2rj <-
        (prev.get(refiningi.getExtent).flatMap(_.dboxes.get(refiningi)),
          prev.get(refinedi.getExtent).flatMap(_.dboxes.get(refinedi))) match {
          case (Some(refining), Some(refined)) =>
            val (rj, axj) = f.createDescriptionBoxRefinement(o2ri.rextent, refining, refined)
            o2ri.copy(rextent = rj, moduleEdges = o2ri.moduleEdges + (axi -> axj)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertDescriptionBoxRefinements: Failed to resolve " +
                s"refining: ${axi.getRefiningDescriptionBox.abbrevIRI()}; " +
                s"refined: ${axi.getRefinedDescriptionBox.abbrevIRI()}"
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertAspects
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: Aspect => s })
    ss.foldLeft(state) { case (acc, ai) =>
      for {
        prev <- acc
        o2ri = prev(e)
        (rj, aj) = f.createAspect(o2ri.rextent, o2ri.tboxes(ai.getTbox), ai.name())
        o2rj = o2ri.copy(rextent = rj, aspects = o2ri.aspects + (ai -> aj))
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertConcepts
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: Concept => s })
    ss.foldLeft(state) { case (acc, ci) =>
      for {
        prev <- acc
        o2ri = prev(e)
        (rj, cj) = f.createConcept(o2ri.rextent, o2ri.tboxes(ci.getTbox), ci.name())
        o2rj = o2ri.copy(rextent = rj, concepts = o2ri.concepts + (ci -> cj))
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertReifiedRelationships
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: ReifiedRelationship => s }).to[Set]
    ss.foldLeft(state) { case (acc, rri) =>
      for {
        prev <- acc
        o2ri = prev(e)
        sourcei = rri.getSource
        targeti = rri.getTarget
        tboxi = rri.getTbox
        o2rj <-
        (prev.get(tboxi.getExtent).flatMap(_.tboxes.get(tboxi)),
          prev.get(sourcei.getTbox.getExtent).flatMap(_.entityLookup(sourcei)),
          prev.get(targeti.getTbox.getExtent).flatMap(_.entityLookup(targeti))) match {
          case (Some(tboxj), Some(sourcej), Some(targetj)) =>
            val (rj, rrj) = f.createReifiedRelationship(
              extent = o2ri.rextent,
              tbox = tboxj,
              source = sourcej,
              target = targetj,
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
            o2ri.copy(rextent = rj, reifiedRelationships = o2ri.reifiedRelationships + (rri -> rrj)).right
          case (tboxj, sourcej, targetj) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertReifiedRelationship: Failed to resolve " +
                tboxj.fold(s" tbox: ${tboxi.getIri}")(_ => "") +
                sourcej.fold(s" source: ${sourcei.getName}")(_ => "") +
                targetj.fold(s" target: ${targeti.getName}")(_ => "")
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertUnreifiedRelationships
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: UnreifiedRelationship => s }).to[Set]
    ss.foldLeft(state) { case (acc, uri) =>
      for {
        prev <- acc
        o2ri = prev(e)
        sourcei = uri.getSource
        targeti = uri.getTarget
        tboxi = uri.getTbox
        o2rj <-
        (prev.get(tboxi.getExtent).flatMap(_.tboxes.get(tboxi)),
          prev.get(sourcei.getTbox.getExtent).flatMap(_.entityLookup(sourcei)),
          prev.get(targeti.getTbox.getExtent).flatMap(_.entityLookup(targeti))) match {
          case (Some(tboxj), Some(sourcej), Some(targetj)) =>
            val (rj, urj) = f.createUnreifiedRelationship(
              extent = o2ri.rextent,
              tbox = tboxj,
              source = sourcej,
              target = targetj,
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
            o2ri.copy(rextent = rj, unreifiedRelationships = o2ri.unreifiedRelationships + (uri -> urj)).right
          case (tboxj, sourcej, targetj) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertUnreifiedRelationship: Failed to resolve " +
                tboxj.fold(s" tbox: ${tboxi.getIri}")(_ => "") +
                sourcej.fold(s" source: ${sourcei.getName}")(_ => "") +
                targetj.fold(s" target: ${targeti.getName}")(_ => "")
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertStructures
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: Structure => s })
    ss.foldLeft(state) { case (acc, sci) =>
      for {
        prev <- acc
        o2ri = prev(e)
        (rj, scj) = f.createStructure(o2ri.rextent, o2ri.tboxes(sci.getTbox), sci.name())
        o2rj = o2ri.copy(rextent = rj, structures = o2ri.structures + (sci -> scj))
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertScalars
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: Scalar => s })
    ss.foldLeft(state) { case (acc, sci) =>
      for {
        prev <- acc
        o2ri = prev(e)
        (rj, scj) = f.createScalar(o2ri.rextent, o2ri.tboxes(sci.getTbox), sci.name())
        o2rj = o2ri.copy(rextent = rj, dataRanges = o2ri.dataRanges + (sci -> scj))
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertRestrictedDataRanges
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: RestrictedDataRange => s })
    ss.foldLeft(state) { case (acc, rdri) =>
      for {
        prev <- acc
        o2ri = prev(e)
        tboxi = rdri.getTbox
        rsi = rdri.getRestrictedRange
        o2rj <-
        (prev.get(tboxi.getExtent).flatMap(_.tboxes.get(tboxi)),
          prev.get(rsi.getTbox.getExtent).flatMap(_.dataRanges.get(rsi))) match {
          case (Some(tboxj), Some(rsj)) =>
            val (rj, rdrj) = rdri match {
              case rri: BinaryScalarRestriction =>
                f.createBinaryScalarRestriction(
                  extent = o2ri.rextent,
                  tbox = tboxj,
                  restrictedRange = rsj,
                  length = Option.apply(rri.getLength),
                  minLength = Option.apply(rri.getMinLength),
                  maxLength = Option.apply(rri.getMaxLength),
                  name = rri.name)
              case rri: IRIScalarRestriction =>
                f.createIRIScalarRestriction(
                  extent = o2ri.rextent,
                  tbox = tboxj,
                  restrictedRange = rsj,
                  length = Option.apply(rri.getLength),
                  minLength = Option.apply(rri.getMinLength),
                  maxLength = Option.apply(rri.getMaxLength),
                  pattern = Option.apply(rri.getPattern),
                  name = rri.name)
              case rri: NumericScalarRestriction =>
                f.createNumericScalarRestriction(
                  extent = o2ri.rextent,
                  tbox = tboxj,
                  restrictedRange = rsj,
                  minExclusive = Option.apply(rri.getMinExclusive),
                  minInclusive = Option.apply(rri.getMinInclusive),
                  maxExclusive = Option.apply(rri.getMaxExclusive),
                  maxInclusive = Option.apply(rri.getMaxInclusive),
                  name = rri.name)
              case rri: PlainLiteralScalarRestriction =>
                f.createPlainLiteralScalarRestriction(
                  extent = o2ri.rextent,
                  tbox = tboxj,
                  restrictedRange = rsj,
                  length = Option.apply(rri.getLength),
                  minLength = Option.apply(rri.getMinLength),
                  maxLength = Option.apply(rri.getMaxLength),
                  pattern = Option.apply(rri.getPattern),
                  langRange = Option.apply(rri.getLangRange),
                  name = rri.name)
              case rri: ScalarOneOfRestriction =>
                f.createScalarOneOfRestriction(
                  extent = o2ri.rextent,
                  tbox = tboxj,
                  restrictedRange = rsj,
                  name = rri.name)
              case rri: StringScalarRestriction =>
                f.createStringScalarRestriction(
                  extent = o2ri.rextent,
                  tbox = tboxj,
                  restrictedRange = rsj,
                  length = Option.apply(rri.getLength),
                  minLength = Option.apply(rri.getMinLength),
                  maxLength = Option.apply(rri.getMaxLength),
                  pattern = Option.apply(rri.getPattern),
                  name = rri.name)
              case rri: SynonymScalarRestriction =>
                f.createSynonymScalarRestriction(
                  extent = o2ri.rextent,
                  tbox = tboxj,
                  restrictedRange = rsj,
                  name = rri.name)
              case rri: TimeScalarRestriction =>
                f.createTimeScalarRestriction(
                  extent = o2ri.rextent,
                  tbox = tboxj,
                  restrictedRange = rsj,
                  minExclusive = Option.apply(rri.getMinExclusive),
                  minInclusive = Option.apply(rri.getMinInclusive),
                  maxExclusive = Option.apply(rri.getMaxExclusive),
                  maxInclusive = Option.apply(rri.getMaxInclusive),
                  name = rri.name)
            }
            o2ri.copy(rextent = rj, dataRanges = o2ri.dataRanges + (rdri -> rdrj)).right
          case (tboxj, rsj) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRestrictedDataRanges: Failed to resolve " +
                tboxj.fold(s" tbox: ${tboxi.getIri}")(_ => "") +
                rsj.fold(s" restricted data range: ${rsi.getName}")(_ => "")
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertScalarOneOfLiteralAxioms
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: ScalarOneOfLiteralAxiom => s })
    ss.foldLeft(state) { case (acc, li) =>
      for {
        prev <- acc
        o2ri = prev(e)
        tboxi = li.getTbox
        dri = li.getAxiom
        o2rj <-
        (prev.get(tboxi.getExtent).flatMap(_.tboxes.get(tboxi)),
          prev.get(dri.getTbox.getExtent).flatMap(_.dataRanges.get(dri))) match {
          case (Some(tboxj), Some(drj: api.ScalarOneOfRestriction)) =>
            val (rj, lj) = f.createScalarOneOfLiteralAxiom(
              extent = o2ri.rextent,
              tbox = tboxj,
              axiom = drj,
              value = li.getValue)
            o2ri.copy(
              rextent = rj,
              scalarOneOfLiterals = o2ri.scalarOneOfLiterals + (li -> lj),
              termAxioms = o2ri.termAxioms + (li -> lj)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertScalarOneLiteralAxioms: Cannot find ScalarOneOfRestriction for ${li.getAxiom.abbrevIRI()}"
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertEntityScalarDataProperties
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: EntityScalarDataProperty => s })
    ss.foldLeft(state) { case (acc, dpi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        tboxi = dpi.getTbox
        dpdi = dpi.getDomain
        dpri = dpi.getRange
        o2rj <-
        (prev.get(tboxi.getExtent).flatMap(_.tboxes.get(tboxi)),
          prev.get(dpdi.getTbox.getExtent).flatMap(_.entityLookup(dpdi)),
          prev.get(dpri.getTbox.getExtent).flatMap(_.dataRanges.get(dpri))) match {
          case (Some(tboxj), Some(dpdj: api.Entity), Some(dprj: api.DataRange)) =>
            val (rj, dpj) = f.createEntityScalarDataProperty(
              o2ri.rextent,
              tboxj,
              dpdj, dprj,
              dpi.isIsIdentityCriteria,
              dpi.name())
            o2ri.copy(rextent = rj, entityScalarDataProperties = o2ri.entityScalarDataProperties + (dpi -> dpj)).right
          case (tboxj, dpdj, dprj) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertEntityScalarDataProperties: Cannot resolve " +
                tboxj.fold(s" tbox: ${tboxi.getIri}")(_ => "") +
                dpdj.fold(s" data property domain: ${dpdi.getName}")(_ => "") +
                dprj.fold(s" data property range: ${dpri.getName}")(_ => "")
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertEntityStructuredDataProperties
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: EntityStructuredDataProperty => s })
    ss.foldLeft(state) { case (acc, dpi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        tboxi = dpi.getTbox
        dpdi = dpi.getDomain
        dpri = dpi.getRange
        o2rj <-
        (prev.get(tboxi.getExtent).flatMap(_.tboxes.get(tboxi)),
          prev.get(dpdi.getTbox.getExtent).flatMap(_.entityLookup(dpdi)),
          prev.get(dpri.getTbox.getExtent).flatMap(_.structures.get(dpri))) match {
          case (Some(tboxj), Some(dpdj: api.Entity), Some(dprj: api.Structure)) =>
            val (rj, dpj) = f.createEntityStructuredDataProperty(
              o2ri.rextent,
              tboxj,
              dpdj, dprj,
              dpi.isIsIdentityCriteria,
              dpi.name())
            o2ri.copy(rextent = rj, entityStructuredDataProperties = o2ri.entityStructuredDataProperties + (dpi -> dpj)).right
          case (tboxj, dpdj, dprj) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertEntityStructuredDataProperties: Cannot resolve " +
                tboxj.fold(s" tbox: ${tboxi.getIri}")(_ => "") +
                dpdj.fold(s" data property domain: ${dpdi.getName}")(_ => "") +
                dprj.fold(s" data property range: ${dpri.getName}")(_ => "")
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertScalarDataProperties
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: ScalarDataProperty => s })
    ss.foldLeft(state) { case (acc, dpi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        tboxi = dpi.getTbox
        dpdi = dpi.getDomain
        dpri = dpi.getRange
        o2rj <-
        (prev.get(tboxi.getExtent).flatMap(_.tboxes.get(tboxi)),
          prev.get(dpdi.getTbox.getExtent).flatMap(_.structures.get(dpdi)),
          prev.get(dpri.getTbox.getExtent).flatMap(_.dataRanges.get(dpri))) match {
          case (Some(tboxj), Some(dpdj: api.Structure), Some(dprj: api.DataRange)) =>
            val (rj, dpj) = f.createScalarDataProperty(
              o2ri.rextent,
              tboxj,
              dpdj, dprj,
              dpi.name())
            o2ri.copy(rextent = rj, scalarDataProperties = o2ri.scalarDataProperties + (dpi -> dpj)).right
          case (tboxj, dpdj, dprj) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertScalarDataProperties: Cannot resolve " +
                tboxj.fold(s" tbox: ${tboxi.getIri}")(_ => "") +
                dpdj.fold(s" data property domain: ${dpdi.getName}")(_ => "") +
                dprj.fold(s" data property range: ${dpri.getName}")(_ => "")
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertStructuredDataProperties
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: StructuredDataProperty => s })
    ss.foldLeft(state) { case (acc, dpi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        tboxi = dpi.getTbox
        dpdi = dpi.getDomain
        dpri = dpi.getRange
        o2rj <-
        (prev.get(tboxi.getExtent).flatMap(_.tboxes.get(tboxi)),
          prev.get(dpdi.getTbox.getExtent).flatMap(_.structures.get(dpdi)),
          prev.get(dpri.getTbox.getExtent).flatMap(_.structures.get(dpri))) match {
          case (Some(tboxj), Some(dpdj: api.Structure), Some(dprj: api.Structure)) =>
            val (rj, dpj) = f.createStructuredDataProperty(
              o2ri.rextent,
              tboxj,
              dpdj, dprj,
              dpi.name())
            o2ri.copy(rextent = rj, structuredDataProperties = o2ri.structuredDataProperties + (dpi -> dpj)).right
          case (tboxj, dpdj, dprj) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertStructuredDataProperties: Cannot resolve " +
                tboxj.fold(s" tbox: ${tboxi.getIri}")(_ => "") +
                dpdj.fold(s" data property domain: ${dpdi.getName}")(_ => "") +
                dprj.fold(s" data property range: ${dpri.getName}")(_ => "")
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertEntityRestrictionAxioms
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: EntityRestrictionAxiom => s })
    ss.foldLeft(state) { case (acc, axi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        tboxi = axi.getTbox
        rdi = axi.getRestrictedDomain
        rri = axi.getRestrictedRange
        rli = axi.getRestrictedRelation
        o2rj <-
        (prev.get(tboxi.getExtent).flatMap(_.tboxes.get(tboxi)),
          prev.get(rdi.getTbox.getExtent).flatMap(_.entityLookup(rdi)),
          prev.get(rri.getTbox.getExtent).flatMap(_.entityLookup(rri)),
          prev.get(rli.getTbox.getExtent).flatMap(_.entityRelationshipLookup(rli))) match {
          case (Some(tboxj), Some(rdj), Some(rrj), Some(rlj)) =>
            val (rj, axj) = axi match {
              case _: EntityExistentialRestrictionAxiom =>
                f.createEntityExistentialRestrictionAxiom(o2ri.rextent, tboxj, rlj, rdj, rrj)
              case _: EntityUniversalRestrictionAxiom =>
                f.createEntityUniversalRestrictionAxiom(o2ri.rextent, tboxj, rlj, rdj, rrj)
            }
            o2ri.copy(rextent = rj, termAxioms = o2ri.termAxioms + (axi -> axj)).right
          case (tboxj, rdj, rrj, rlj) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertEntityRestrictionAxioms: Cannot resolve " +
                tboxj.fold(s" tbox: ${tboxi.getIri}")(_ => "") +
                rdj.fold(s" restriction domain: ${rdi.getName}")(_ => "") +
                rrj.fold(s" restriction range: ${rdi.getName}")(_ => "") +
                rlj.fold(s" restricted relation: ${rli.getName}")(_ => "")
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertEntityScalarDataPropertyRestrictionAxioms
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: EntityScalarDataPropertyRestrictionAxiom => s })
    ss.foldLeft(state) { case (acc, axi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        tboxi = axi.getTbox
        ei = axi.getRestrictedEntity
        dpi = axi.getScalarProperty
        o2rj <-
        (prev.get(tboxi.getExtent).flatMap(_.tboxes.get(tboxi)),
          prev.get(ei.getTbox.getExtent).flatMap(_.entityLookup(ei)),
          prev.get(dpi.getTbox.getExtent).flatMap(_.entityScalarDataProperties.get(dpi))) match {
          case (Some(tboxj), Some(ej), Some(dpj)) =>
            val (rj, axj: \/[EMFProblems, api.EntityScalarDataPropertyRestrictionAxiom]) = axi match {
              case axpi: EntityScalarDataPropertyParticularRestrictionAxiom =>
                f
                  .createEntityScalarDataPropertyParticularRestrictionAxiom(
                    o2ri.rextent, tboxj, ej, dpj, axpi.getLiteralValue) match {
                  case (rk, ak) => rk -> ak.right
                }
              case axui: EntityScalarDataPropertyExistentialRestrictionAxiom =>
                val dri = axui.getScalarRestriction
                prev.get(dri.getTbox.getExtent).flatMap(_.dataRanges.get(dri)) match {
                  case Some(drj) =>
                    f
                      .createEntityScalarDataPropertyExistentialRestrictionAxiom(
                        o2ri.rextent, tboxj, ej, dpj, drj) match {
                      case (rk, ak) => rk -> ak.right
                    }
                  case _ =>
                    o2ri.rextent -> new EMFProblems(new java.lang.IllegalArgumentException(
                      s"convertEntityScalarDataPropertyRestrictionAxioms: " +
                        s"Cannot find DataRange: ${axui.getScalarRestriction.abbrevIRI()}"
                    )).left
                }
              case axui: EntityScalarDataPropertyUniversalRestrictionAxiom =>
                val dri = axui.getScalarRestriction
                prev.get(dri.getTbox.getExtent).flatMap(_.dataRanges.get(dri)) match {
                  case Some(drj) =>
                    f
                      .createEntityScalarDataPropertyUniversalRestrictionAxiom(
                        o2ri.rextent, tboxj, ej, dpj, drj) match {
                      case (rk, ak) => rk -> ak.right
                    }
                  case _ =>
                    o2ri.rextent -> new EMFProblems(new java.lang.IllegalArgumentException(
                      s"convertEntityScalarDataPropertyRestrictionAxioms: " +
                        s"Cannot find DataRange: ${axui.getScalarRestriction.abbrevIRI()}"
                    )).left
                }
            }
            axj match {
              case \/-(axrj) =>
                o2ri.copy(rextent = rj, termAxioms = o2ri.termAxioms + (axi -> axrj)).right
              case -\/(error) =>
                error.left
            }
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertEntityScalarDataPropertyRestrictionAxioms: Cannot find EntityScalarDataPropertyRestrictionAxiom for" +
                s": restricted entity: ${axi.getRestrictedEntity.abbrevIRI()}" +
                s", scalar data property: ${axi.getScalarProperty.abbrevIRI()}"
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertSpecializationAxioms
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: SpecializationAxiom => s })
    ss.foldLeft(state) { case (acc, axi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        tboxi = axi.getTbox
        supi = axi.parent()
        subi = axi.child()
        (rj, axj: \/[EMFProblems, api.SpecializationAxiom]) =
        (axi,
          prev.get(tboxi.getExtent).flatMap(_.tboxes.get(tboxi)),
          prev.get(supi.getTbox.getExtent).flatMap(_.entityLookup(supi)),
          prev.get(subi.getTbox.getExtent).flatMap(_.entityLookup(subi))) match {
          case (_: ConceptSpecializationAxiom, Some(tboxj), Some(supj: api.Concept), Some(subj: api.Concept)) =>
            f.createConceptSpecializationAxiom(o2ri.rextent, tboxj, supj, subj) match {
              case (rk, ak) => rk -> ak.right
            }
          case (_: AspectSpecializationAxiom, Some(tboxj), Some(supj: api.Aspect), Some(subj)) =>
            f.createAspectSpecializationAxiom(o2ri.rextent, tboxj, supj, subj) match {
              case (rk, ak) => rk -> ak.right
            }
          case (_: ReifiedRelationshipSpecializationAxiom, Some(tboxj), Some(supj: api.ReifiedRelationship), Some(subj: api.ReifiedRelationship)) =>
            f.createReifiedRelationshipSpecializationAxiom(o2ri.rextent, tboxj, supj, subj) match {
              case (rk, ak) => rk -> ak.right
            }
          case (_, tboxj, supj, subj) =>
            o2ri.rextent -> new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertSpecializationAxioms: Cannot find: " +
                s"tbox: ${tboxi.abbrevIRI()}" +
                s"general: ${supi.abbrevIRI()}" +
                s"specific: ${subi.abbrevIRI()}"
            )).left
        }
        o2rj <- axj match {
          case \/-(axrj) =>
            o2ri.copy(rextent = rj, termAxioms = o2ri.termAxioms + (axi -> axrj)).right
          case -\/(error) =>
            error.left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertRootConceptTaxonomyAxioms
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val bundles = e.getModules.selectByKindOf { case b: Bundle => b }
    val axs = bundles.flatMap(_.getBundleStatements.selectByKindOf { case ax: RootConceptTaxonomyAxiom => ax })
    axs.foldLeft(state) { case (acc, axi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        o2rj <- convertConceptTreeDisjunction(o2ri, axi)
        next = prev.updated(e, o2rj)
      } yield next
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

  protected def convertConceptInstances
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val dboxes = e.getModules.selectByKindOf { case dbox: DescriptionBox => dbox }
    val ss = dboxes.flatMap(_.getConceptInstances.asScala.to[Set])
    ss.foldLeft(state) { case (acc, di) =>
      for {
        prev <- acc
        o2ri = prev(e)
        dboxi = di.descriptionBox()
        cli = di.getSingletonConceptClassifier
        o2rj <-
        (prev.get(dboxi.getExtent).flatMap(_.dboxes.get(dboxi)),
          prev.get(cli.getTbox.getExtent).flatMap(_.concepts.get(cli))) match {
          case (Some(dboxj), Some(clj)) =>
            val (rj, dj) = f.createConceptInstance(o2ri.rextent, dboxj, clj, di.getName)
            o2ri.copy(
              rextent = rj,
              conceptualEntitySingletonInstances = o2ri.conceptualEntitySingletonInstances + (di -> dj)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertConceptInstances: Cannot find: " +
                s"concept: ${di.getSingletonConceptClassifier.abbrevIRI}"
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertReifiedRelationshipInstances
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val dboxes = e.getModules.selectByKindOf { case dbox: DescriptionBox => dbox }
    val ss = dboxes.flatMap(_.getReifiedRelationshipInstances.asScala.to[Set])
    ss.foldLeft(state) { case (acc, di) =>
      for {
        prev <- acc
        o2ri = prev(e)
        dboxi = di.descriptionBox()
        cli = di.getSingletonReifiedRelationshipClassifier
        o2rj <-
        (prev.get(dboxi.getExtent).flatMap(_.dboxes.get(dboxi)),
          prev.get(cli.getTbox.getExtent).flatMap(_.reifiedRelationships.get(cli))) match {
          case (Some(dboxj), Some(clj)) =>
            val (rj, dj) = f.createReifiedRelationshipInstance(o2ri.rextent, dboxj, clj, di.getName)
            o2ri.copy(
              rextent = rj,
              conceptualEntitySingletonInstances = o2ri.conceptualEntitySingletonInstances + (di -> dj)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertReifiedRelationshipInstances: Cannot find: " +
                s"reified relationship: ${di.getSingletonReifiedRelationshipClassifier.abbrevIRI}"
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertReifiedRelationshipInstanceDomains
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val dboxes = e.getModules.selectByKindOf { case dbox: DescriptionBox => dbox }
    val ss = dboxes.flatMap(_.getReifiedRelationshipInstanceDomains.asScala.to[Set])
    ss.foldLeft(state) { case (acc, di) =>
      for {
        prev <- acc
        o2ri = prev(e)
        dboxi = di.descriptionBox()
        rrii = di.getReifiedRelationshipInstance
        rdi = di.getDomain
        o2rj <-
        (prev.get(dboxi.getExtent).flatMap(_.dboxes.get(dboxi)),
          prev.get(rrii.descriptionBox.getExtent).flatMap(_.conceptualEntitySingletonInstances.get(rrii)),
          prev.get(rdi.descriptionBox.getExtent).flatMap(_.conceptualEntitySingletonInstances.get(rdi))) match {
          case (Some(dboxj), Some(rrij: api.ReifiedRelationshipInstance), Some(rdj)) =>
            val (rj, dj) = f.createReifiedRelationshipInstanceDomain(o2ri.rextent, dboxj, rrij, rdj)
            o2ri.copy(
              rextent = rj,
              reifiedRelationshipInstanceDomains = o2ri.reifiedRelationshipInstanceDomains + (di -> dj)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertReifiedRelationshipInstanceDomains: Cannot find: " +
                s"reified relationship instance: ${rrii.abbrevIRI}" +
                s"reified relationship domain: ${rdi.abbrevIRI}"
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertReifiedRelationshipInstanceRanges
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val dboxes = e.getModules.selectByKindOf { case dbox: DescriptionBox => dbox }
    val ss = dboxes.flatMap(_.getReifiedRelationshipInstanceRanges.asScala.to[Set])
    ss.foldLeft(state) { case (acc, di) =>
      for {
        prev <- acc
        o2ri = prev(e)
        dboxi = di.descriptionBox()
        rrii = di.getReifiedRelationshipInstance
        rri = di.getRange
        o2rj <-
        (prev.get(dboxi.getExtent).flatMap(_.dboxes.get(dboxi)),
          prev.get(rrii.descriptionBox.getExtent).flatMap(_.conceptualEntitySingletonInstances.get(rrii)),
          prev.get(rri.descriptionBox.getExtent).flatMap(_.conceptualEntitySingletonInstances.get(rri))) match {
          case (Some(dboxj), Some(rrij: api.ReifiedRelationshipInstance), Some(rrj)) =>
            val (rj, dj) = f.createReifiedRelationshipInstanceRange(o2ri.rextent, dboxj, rrij, rrj)
            o2ri.copy(
              rextent = rj,
              reifiedRelationshipInstanceRanges = o2ri.reifiedRelationshipInstanceRanges + (di -> dj)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertReifiedRelationshipInstanceRanges: Cannot find: " +
                s"reified relationship instance: ${rrii.abbrevIRI}" +
                s"reified relationship range: ${rri.abbrevIRI}"
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertUnreifiedRelationshipInstances
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val dboxes = e.getModules.selectByKindOf { case dbox: DescriptionBox => dbox }
    val ss = dboxes.flatMap(_.getUnreifiedRelationshipInstanceTuples.asScala.to[Set])
    ss.foldLeft(state) { case (acc, di) =>
      for {
        prev <- acc
        o2ri = prev(e)
        dboxi = di.descriptionBox()
        ui = di.getUnreifiedRelationship
        udi = di.getDomain
        uri = di.getRange
        o2rj <-
        (prev.get(dboxi.getExtent).flatMap(_.dboxes.get(dboxi)),
          prev.get(ui.getTbox.getExtent).flatMap(_.unreifiedRelationships.get(ui)),
          prev.get(udi.descriptionBox.getExtent).flatMap(_.conceptualEntitySingletonInstances.get(udi)),
          prev.get(uri.descriptionBox.getExtent).flatMap(_.conceptualEntitySingletonInstances.get(uri))) match {
          case (Some(dboxj), Some(uj), Some(udj), Some(urj)) =>
            val (rj, dj) = f.createUnreifiedRelationshipInstanceTuple(o2ri.rextent, dboxj, uj, udj, urj)
            o2ri.copy(
              rextent = rj,
              unreifiedRelationshipInstanceTuples = o2ri.unreifiedRelationshipInstanceTuples + (di -> dj)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertUnreifiedRelationshipInstances: Cannot find: " +
                s"domain: ${udi.abbrevIRI}" +
                s"range: ${uri.abbrevIRI}" +
                s"unreified relationship: ${ui.abbrevIRI}"
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertSingletonScalarDataPropertyValues
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val dboxes = e.getModules.selectByKindOf { case dbox: DescriptionBox => dbox }
    val ss = dboxes.flatMap(_.getSingletonScalarDataPropertyValues.asScala.to[Set])
    ss.foldLeft(state) { case (acc, di) =>
      for {
        prev <- acc
        o2ri = prev(e)
        dboxi = di.descriptionBox()
        si = di.getSingletonInstance
        dpi = di.getScalarDataProperty
        o2rj <-
        (prev.get(dboxi.getExtent).flatMap(_.dboxes.get(dboxi)),
          prev.get(si.descriptionBox().getExtent).flatMap(_.conceptualEntitySingletonInstances.get(si)),
          prev.get(dpi.getTbox.getExtent).flatMap(_.entityScalarDataProperties.get(dpi))) match {
          case (Some(dboxj), Some(sj), Some(dpj)) =>
            val (rj, dj) = f.createSingletonInstanceScalarDataPropertyValue(o2ri.rextent, dboxj, sj, dpj, di.getScalarPropertyValue)
            o2ri.copy(
              rextent = rj,
              singletonInstanceScalarDataPropertyValues = o2ri.singletonInstanceScalarDataPropertyValues + (di -> dj)).right
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertSingletonScalarDataPropertyValues: Cannot find: " +
                s"singleton instance: ${si.abbrevIRI}" +
                s"entity scalar data property: ${dpi.abbrevIRI}"
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertSingletonStructuredDataPropertyValues
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val (e, _) = entry
    val dboxes = e.getModules.selectByKindOf { case dbox: DescriptionBox => dbox }
    val ss = dboxes.flatMap(_.getSingletonStructuredDataPropertyValues.asScala.to[Set])
    ss.foldLeft(state) { case (acc, di) =>
      for {
        prev <- acc
        o2ri = prev(e)
        dboxi = di.descriptionBox()
        si = di.getSingletonInstance
        dpi = di.getStructuredDataProperty.asInstanceOf[EntityStructuredDataProperty]
        o2rj <-
        (prev.get(dboxi.getExtent).flatMap(_.dboxes.get(dboxi)),
          prev.get(si.descriptionBox().getExtent).flatMap(_.conceptualEntitySingletonInstances.get(si)),
          prev.get(dpi.getTbox.getExtent).flatMap(_.entityStructuredDataProperties.get(dpi))) match {
          case (Some(dboxj), Some(sj), Some(dpj)) =>
            val (rj, dj) = f.createSingletonInstanceStructuredDataPropertyValue(o2ri.rextent, dboxj, sj, dpj)
            convertSingletonInstanceStructuredDataPropertyContext(
              o2ri.copy(
                rextent = rj,
                singletonInstanceStructuredDataPropertyValues = o2ri.singletonInstanceStructuredDataPropertyValues + (di -> dj)),
              di.getScalarDataPropertyValues.asScala.to[Seq].map(dj -> _),
              di.getStructuredPropertyTuples.asScala.to[Seq].map(dj -> _))
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertSingletonStructuredDataPropertyValues: Cannot find: " +
                s"singleton instance: ${si.abbrevIRI}" +
                s"entity structured data property: ${dpi.abbrevIRI}"
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
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
          o2r.copy(
            rextent = rj,
            structuredDataPropertyTuples = o2r.structuredDataPropertyTuples + (sti -> stj)),
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
        val (rj, scj) = f.createScalarDataPropertyValue(o2r.rextent, sck, sci.getScalarPropertyValue, ctx)
        val next = o2r.copy(
          rextent = rj,
          scalarDataPropertyValues = o2r.scalarDataPropertyValues + (sci -> scj))
        convertSingletonInstanceStructuredDataPropertyContext(next, sct, Nil)
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"convertSingletonInstanceStructuredDataPropertyContext: Cannot find: " +
            s"scalar data property: ${sci.getScalarDataProperty.abbrevIRI}"
        )).left
    }
  } else
    o2r.right

  protected def convertAnnotations
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    val (e, _) = entry
    e.getModules.asScala.to[Set].foldLeft(state) {
      case (acc1, mi) =>
        mi match {
          case tboxi: TerminologyBox =>
            for {
              prev <- acc1
              allO2Rs = prev.values
              annotationProperties = allO2Rs.flatMap(_.aps).toMap
              o2ri = prev(e)
              o2rj <- tboxi.getAnnotations.asScala.foldLeft(o2ri.right[EMFProblems]) { case (acc2, a0) =>
                for {
                  o2rk <- acc2
                  aSubject <- allO2Rs.flatMap(_.elementLookup(a0.getSubject)).headOption match {
                    case Some(aSubject) =>
                      aSubject.right
                    case None =>
                      new EMFProblems(new java.lang.IllegalArgumentException(
                        s"convertAnnotations: Cannot find: ${a0.getSubject} "
                      )).left
                  }
                  aProperty <- annotationProperties.get(a0.getProperty) match {
                    case Some(aProperty) =>
                      aProperty.right
                    case None =>
                      new EMFProblems(new java.lang.IllegalArgumentException(
                        s"convertAnnotations: Cannot find: ${a0.getProperty} "
                      )).left
                  }
                  tboxk = o2rk.tboxes(tboxi)
                  (el, _) = f.createAnnotation(o2rk.rextent, tboxk, aSubject, aProperty, a0.getValue)
                  o2rl = o2rk.copy(rextent = el)
                } yield o2rl
              }
              next = prev.updated(e, o2rj)
            } yield next

          case dboxi: DescriptionBox =>
            for {
              prev <- acc1
              allO2Rs = prev.values
              annotationProperties = allO2Rs.flatMap(_.aps).toMap
              o2ri = prev(e)
              o2rj <- dboxi.getAnnotations.asScala.foldLeft(o2ri.right[EMFProblems]) { case (acc2, a0) =>
                for {
                  o2rk <- acc2
                  aSubject <- allO2Rs.flatMap(_.elementLookup(a0.getSubject)).headOption match {
                    case Some(aSubject) =>
                      aSubject.right
                    case None =>
                      new EMFProblems(new java.lang.IllegalArgumentException(
                        s"convertAnnotations: Cannot find: ${a0.getSubject} "
                      )).left
                  }
                  aProperty <- annotationProperties.get(a0.getProperty) match {
                    case Some(aProperty) =>
                      aProperty.right
                    case None =>
                      new EMFProblems(new java.lang.IllegalArgumentException(
                        s"convertAnnotations: Cannot find: ${a0.getProperty} "
                      )).left
                  }
                  dboxk = o2rk.dboxes(dboxi)
                  (el, _) = f.createAnnotation(o2rk.rextent, dboxk, aSubject, aProperty, a0.getValue)
                  o2rl = o2rk.copy(rextent = el)
                } yield o2rl
              }
              next = prev.updated(e, o2rj)
            } yield next
        }
    }
  }

  def convert
  (fileExtents: Map[Extent, File])
  (implicit factory: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = for {
    c00 <- fileExtents.map { case (e, f) =>
      e -> OMLText2Resolver(omlFile = f, rextent = api.Extent())
    }.right[EMFProblems]

    // Modules
    c01 <- c00.foldLeft(c00.right[EMFProblems])(convertModules)

    // AnnotationProperties
    c02 <- c01.foldLeft(c01.right[EMFProblems])(convertAnnotationProperties)

    // TerminologyExtensions

    c03 <- c02.foldLeft(c02.right[EMFProblems])(convertTerminologyExtensions)

    // Atomic Entities
    c10 = c03

    c11 <- c10.foldLeft(c10.right[EMFProblems])(convertAspects)

    c12 <- c11.foldLeft(c11.right[EMFProblems])(convertConcepts)

    // Other ModuleEdges
    c20 = c12

    c21 <- c20.foldLeft(c20.right[EMFProblems])(convertConceptDesignationTerminologyAxioms)

    c22 <- c21.foldLeft(c21.right[EMFProblems])(convertTerminologyNestingAxioms)

    c23 <- c22.foldLeft(c22.right[EMFProblems])(convertBundledTerminologyAxioms)

    c24 <- c23.foldLeft(c23.right[EMFProblems])(convertDescriptionBoxExtendsClosedWorldDefinitions)

    c25 <- c24.foldLeft(c24.right[EMFProblems])(convertDescriptionBoxRefinements)

    // Relationships
    c30 = c25

    c31 <- c30.foldLeft(c30.right[EMFProblems])(convertReifiedRelationships)

    c32 <- c31.foldLeft(c31.right[EMFProblems])(convertUnreifiedRelationships)

    // DataTypes

    c33 <- c32.foldLeft(c32.right[EMFProblems])(convertStructures)

    c34 <- c33.foldLeft(c33.right[EMFProblems])(convertScalars)

    c35 <- c34.foldLeft(c34.right[EMFProblems])(convertRestrictedDataRanges)

    c36 <- c35.foldLeft(c35.right[EMFProblems])(convertScalarOneOfLiteralAxioms)

    // DataRelationships
    c40 = c36

    c41 <- c40.foldLeft(c40.right[EMFProblems])(convertEntityScalarDataProperties)

    c42 <- c41.foldLeft(c41.right[EMFProblems])(convertEntityStructuredDataProperties)

    c43 <- c42.foldLeft(c42.right[EMFProblems])(convertScalarDataProperties)

    c44 <- c43.foldLeft(c43.right[EMFProblems])(convertStructuredDataProperties)

    // Restrictions
    c50 = c44

    c51 <- c50.foldLeft(c50.right[EMFProblems])(convertEntityRestrictionAxioms)

    c52 <- c51.foldLeft(c51.right[EMFProblems])(convertEntityScalarDataPropertyRestrictionAxioms)

    // Specializations
    c60 = c52

    c61 <- c60.foldLeft(c60.right[EMFProblems])(convertSpecializationAxioms)

    // Disjunctions
    c70 = c61

    c71 <- c70.foldLeft(c70.right[EMFProblems])(convertRootConceptTaxonomyAxioms)

    // ConceptualEntityInstances
    c80 = c71

    c81 <- c80.foldLeft(c80.right[EMFProblems])(convertConceptInstances)

    c82 <- c81.foldLeft(c81.right[EMFProblems])(convertReifiedRelationshipInstances)

    c83 <- c82.foldLeft(c82.right[EMFProblems])(convertReifiedRelationshipInstanceDomains)

    c84 <- c83.foldLeft(c83.right[EMFProblems])(convertReifiedRelationshipInstanceRanges)

    c85 <- c84.foldLeft(c84.right[EMFProblems])(convertUnreifiedRelationshipInstances)

    // Data Property Values
    c90 = c85

    c91 <- c90.foldLeft(c90.right[EMFProblems])(convertSingletonScalarDataPropertyValues)

    c92 <- c91.foldLeft(c91.right[EMFProblems])(convertSingletonStructuredDataPropertyValues)

    cA0 = c92

    // AnnotationProperties
    cA1 <- cA0.foldLeft(cA0.right[EMFProblems])(convertAnnotations)

  } yield cA1

}
