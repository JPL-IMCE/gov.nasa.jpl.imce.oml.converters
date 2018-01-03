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

package gov.nasa.jpl.imce.oml.processor.internal

import ammonite.ops.RelPath

import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.processor.emf2tables
import gov.nasa.jpl.imce.oml.processor.utils.EMFProblems
import gov.nasa.jpl.imce.oml.model.bundles._
import gov.nasa.jpl.imce.oml.model.common._
import gov.nasa.jpl.imce.oml.model.descriptions._
import gov.nasa.jpl.imce.oml.model.graphs._
import gov.nasa.jpl.imce.oml.model.terminologies._
import gov.nasa.jpl.imce.oml.resolver
import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.tables.{ClosedWorldDesignations, OpenWorldDefinitions, TerminologyKind => TTerminologyKind}
import gov.nasa.jpl.imce.oml.tables.{Final, Partial, DescriptionKind => TDescriptionKind}

import scala.collection.JavaConverters._
import scala.collection.immutable._
import scala.Predef.{ArrowAssoc, augmentString, require}
import scala.{Boolean, None, Option, Some, StringContext, Tuple3}
import scalaz._
import Scalaz._

case class OMLText2Resolver
(omlFile: RelPath,
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

 chainRules: Map[ChainRule, api.ChainRule] = Map.empty,
 ruleBodySegments: Map[RuleBodySegment, api.RuleBodySegment] = Map.empty,
 segmentPredicates: Map[SegmentPredicate, api.SegmentPredicate] = Map.empty,

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

  def toOMLTablesFile
  : RelPath
  = omlFile.copy(segments =
    omlFile.segments.dropRight(1) :+
      omlFile.segments.last.stripSuffix(".oml")+".omlzip")

  def includesAPIModule(m: api.Module): Boolean = tboxes.values.find(_ == m).orElse(dboxes.values.find(_ == m)).isDefined

  def allAccessibleModules: Map[Module, api.Module] =
    tboxes ++ dboxes

  def moduleLookup(m: Module): Option[api.Module] = m match {
    case t: TerminologyBox =>
      tboxes.get(t)
    case d: DescriptionBox =>
      dboxes.get(d)
  }

  def elementLookup(e: LogicalElement): Option[api.LogicalElement] = e match {
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
            val (rj, gj) = f.createTerminologyGraph(
              o2ri.rextent,
              convertTerminologyGraphKind(gi.getKind),
              tables.taggedTypes.iri(gi.iri()))
            o2ri.copy(rextent = rj, tboxes = o2ri.tboxes + (gi -> gj))
          case bi: Bundle =>
            val (rj, bj) = f.createBundle(
              o2ri.rextent,
              convertTerminologyGraphKind(bi.getKind),
              tables.taggedTypes.iri(bi.iri()))
            o2ri.copy(rextent = rj, tboxes = o2ri.tboxes + (bi -> bj))
          case di: DescriptionBox =>
            val (rj, dj) = f.createDescriptionBox(
              o2ri.rextent,
              convertDescriptionKind(di.getKind),
              tables.taggedTypes.iri(di.iri()))
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
    e.getModules.asScala.to[Set].foldLeft(state) { case (acci, mi) =>
      for {
        previ <- acci
        o2ri = previ(e)
        o2rl <- mi.getAnnotationProperties.asScala.to[Set].foldLeft(o2ri.right[EMFProblems]) { case (accj, apj) =>
          for {
            o2rj <- accj
            mj <- o2rj.moduleLookup(mi) match {
              case Some(m) =>
                m.right[EMFProblems]
              case None =>
                new EMFProblems(new java.lang.IllegalArgumentException(
                  s"convertAnnotationProperties: " +
                    s" Failed to resolve module=${mi.getIri}" +
                    s" For AnnotationProperty: ${apj.getIri}"
                )).left
            }
            (rj, apk) = f.createAnnotationProperty(
              o2rj.rextent,
              mj,
              tables.taggedTypes.iri(apj.getIri),
              tables.taggedTypes.abbrevIRI(apj.getAbbrevIRI))
            o2rk = o2rj.copy(rextent = rj, aps = o2rj.aps + (apj -> apk))
          } yield o2rk
        }
        nexti = previ.updated(e, o2rl)
      } yield nexti
    }
  }

  protected def convertTerminologyExtensions
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
            val (rj, axj) = f.createTerminologyExtensionAxiom(o2ri.rextent, extendingj, extendedj.iri)
            o2ri.copy(rextent = rj, moduleEdges = o2ri.moduleEdges + (axi -> axj)).right
          case (extendingj, extendedj) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertTerminologyExtensions: " +
                extendingj.fold(s"Failed to resolve extending=${extendingi.getIri}")(_ => s"From extending=${extendingi.getIri}") +
                "; " +
                extendedj.fold(s"Failed to resolve extended=${extendedi.getIri}")(_ => s"To extended=${extendedi.getIri}")
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
            val (rj, axj) = f.createConceptDesignationTerminologyAxiom(o2ri.rextent, designationG, designatedC, designatedT.iri)
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
            val (rj, axj) = f.createTerminologyNestingAxiom(o2ri.rextent, nestedG, nestingC, nestingT.iri)
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
            val (rj, axj) = f.createBundledTerminologyAxiom(o2ri.rextent, bundle, bundled.iri)
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
            val (rj, axj) = f.createDescriptionBoxExtendsClosedWorldDefinitions(o2ri.rextent, dbox, definitions.iri)
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
            val (rj, axj) = f.createDescriptionBoxRefinement(o2ri.rextent, refining, refined.iri)
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: Aspect => s })
    ss.foldLeft(state) { case (acc, ai) =>
      for {
        prev <- acc
        o2ri = prev(e)
        (rj, aj) = f.createAspect(
          o2ri.rextent,
          o2ri.tboxes(ai.getTbox),
          tables.taggedTypes.localName(ai.name()))
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: Concept => s })
    ss.foldLeft(state) { case (acc, ci) =>
      for {
        prev <- acc
        o2ri = prev(e)
        (rj, cj) = f.createConcept(
          o2ri.rextent,
          o2ri.tboxes(ci.getTbox),
          tables.taggedTypes.localName(ci.name()))
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
              name = tables.taggedTypes.localName(rri.name()),
              unreifiedPropertyName = tables.taggedTypes.localName(rri.getUnreifiedPropertyName),
              unreifiedInversePropertyName = Option.apply(
                tables.taggedTypes.localName(rri.getUnreifiedInversePropertyName)))
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
              name = tables.taggedTypes.localName(uri.name()))
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

  @scala.annotation.tailrec
  protected final def getRuleBodySegmentPredicates
  (allAs: Map[Aspect, api.Aspect],
   allCs: Map[Concept, api.Concept],
   allRRs: Map[ReifiedRelationship, api.ReifiedRelationship],
   allURs: Map[UnreifiedRelationship, api.UnreifiedRelationship],
   cr: ChainRule,
   segment: RuleBodySegment,
   segmentPredicates: Seq[(RuleBodySegment, SegmentPredicate, api.Term)])
  : EMFProblems \/ Seq[(RuleBodySegment, SegmentPredicate, api.Term)]
  = Option(segment.getPredicate) match {
    case Some(pred) =>
      Option(pred.termPredicate()) match {
        case Some(term: Aspect) =>
          allAs.get(term) match {
            case Some(a) =>
              val nextSegmentPredicates = segmentPredicates :+ Tuple3(segment, pred, a)
              Option(segment.getNextSegment) match {
                case Some(next) =>
                  getRuleBodySegmentPredicates(allAs, allCs, allRRs, allURs, cr, next, nextSegmentPredicates)
                case None =>
                  nextSegmentPredicates.right
              }
            case None =>
              new EMFProblems(new java.lang.IllegalArgumentException(
                s"getRuleBodySegmentPredicates: Unmapped Aspect $term in $pred in $segment in $cr"
              )).left
          }
        case Some(term: Concept) =>
          allCs.get(term) match {
            case Some(c) =>
              val nextSegmentPredicates = segmentPredicates :+ Tuple3(segment, pred, c)
              Option(segment.getNextSegment) match {
                case Some(next) =>
                  getRuleBodySegmentPredicates(allAs, allCs, allRRs, allURs, cr, next, nextSegmentPredicates)
                case None =>
                  nextSegmentPredicates.right
              }
            case None =>
              new EMFProblems(new java.lang.IllegalArgumentException(
                s"getRuleBodySegmentPredicates: Unmapped Concept $term in $pred in $segment in $cr"
              )).left
          }
        case Some(term: ReifiedRelationship) =>
          allRRs.get(term) match {
            case Some(rr) =>
              val nextSegmentPredicates = segmentPredicates :+ Tuple3(segment, pred, rr)
              Option(segment.getNextSegment) match {
                case Some(next) =>
                  getRuleBodySegmentPredicates(allAs, allCs, allRRs, allURs, cr, next, nextSegmentPredicates)
                case None =>
                  nextSegmentPredicates.right
              }
            case None =>
              new EMFProblems(new java.lang.IllegalArgumentException(
                s"getRuleBodySegmentPredicates: Unmapped ReifiedRelationship $term in $pred in $segment in $cr"
              )).left
          }
        case Some(term: UnreifiedRelationship) =>
          allURs.get(term) match {
            case Some(ur) =>
              val nextSegmentPredicates = segmentPredicates :+ Tuple3(segment, pred, ur)
              Option(segment.getNextSegment) match {
                case Some(next) =>
                  getRuleBodySegmentPredicates(allAs, allCs, allRRs, allURs, cr, next, nextSegmentPredicates)
                case None =>
                  nextSegmentPredicates.right
              }
            case None =>
              new EMFProblems(new java.lang.IllegalArgumentException(
                s"getRuleBodySegmentPredicates: Unmapped UneifiedRelationship $term in $pred in $segment in $cr"
              )).left
          }
        case _ =>
          new EMFProblems(new java.lang.IllegalArgumentException(
            s"getRuleBodySegmentPredicates: Ill-formed SegmentPredicate without a valid term: $pred in $segment in $cr"
          )).left
      }
    case None =>
      new EMFProblems(new java.lang.IllegalArgumentException(
        s"getRuleBodySegmentPredicates: Ill-formed RuleBodySegment without a predicate: $segment in $cr"
      )).left
  }

  protected def convertChainRules
  (allAs: Map[Aspect, api.Aspect],
   allCs: Map[Concept, api.Concept],
   allRRs: Map[ReifiedRelationship, api.ReifiedRelationship],
   allURs: Map[UnreifiedRelationship, api.UnreifiedRelationship])
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val crs = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case cr: ChainRule => cr }).to[Set]
    crs.foldLeft(state) { case (acc, cri) =>
      for {
        prev <- acc
        o2ri = prev(e)
        uri = cri.getHead
        segmentsi <- getRuleBodySegmentPredicates(allAs, allCs, allRRs, allURs, cri, cri.getFirstSegment, Seq.empty)
        tboxi = cri.getTbox
        tuple <-
        ( prev.get(tboxi.getExtent).flatMap(_.tboxes.get(tboxi)),
          prev.get(uri.getTbox.getExtent).flatMap(_.unreifiedRelationships.get(uri))) match {
          case (Some(tboxj), Some(urj)) =>
            val (rj, crj) = f.createChainRule(
              extent = o2ri.rextent,
              tbox = tboxj,
              name = tables.taggedTypes.localName(cri.getName),
              head = urj)
            val o2rj1 = o2ri.copy(rextent = rj, chainRules = o2ri.chainRules + (cri -> crj))
            val o2rj2 = segmentsi.foldLeft((o2rj1, Option(crj), Option.empty[api.RuleBodySegment])) {
              case ((o2rj3, ocrj, orbsj), (rbsi, predi, tj)) =>
                val rbsi_uuid = rbsi.uuid()
                val (rk1, rbsj) = f.createRuleBodySegment(o2rj3.rextent, orbsj, ocrj)
                val rbsj_uuid = rbsj.uuid.toString
                if (rbsi_uuid != rbsj_uuid)
                  require(rbsi_uuid == rbsj_uuid)
                val rk2 = if (ocrj.isDefined)
                  rk1.copy(
                    chainRuleOfRuleBodySegment = rk1.chainRuleOfRuleBodySegment + (rbsj -> crj),
                    firstSegment = rk1.firstSegment + (crj -> rbsj))
                else
                  // createRuleBodySegment factory has updated: nextSegment, ruleBodySegmentOfRuleBodySegment
                  rk1.copy(
                    chainRuleOfRuleBodySegment = rk1.chainRuleOfRuleBodySegment + (rbsj -> crj))
                val o2rj4 = o2rj3.copy(rextent = rk2, ruleBodySegments = o2rj3.ruleBodySegments + (rbsi -> rbsj))
                val (rl, predj) = (predi, tj) match {
                  case (p: AspectPredicate, at: api.Aspect) =>
                    f.createAspectPredicate(o2rj4.rextent, at, rbsj)
                  case (p: ConceptPredicate, ct: api.Concept) =>
                    f.createConceptPredicate(o2rj4.rextent, rbsj, ct)
                  case (p: ReifiedRelationshipPredicate, rrt: api.ReifiedRelationship) =>
                    f.createReifiedRelationshipPredicate(o2rj4.rextent, rbsj, rrt)
                  case (p: ReifiedRelationshipPropertyPredicate, rrt: api.ReifiedRelationship) =>
                    f.createReifiedRelationshipPropertyPredicate(o2rj4.rextent, rbsj, rrt)
                  case (p: ReifiedRelationshipInversePropertyPredicate, rrt: api.ReifiedRelationship) =>
                    f.createReifiedRelationshipInversePropertyPredicate(o2rj4.rextent, rbsj, rrt)
                  case (p: ReifiedRelationshipSourcePropertyPredicate, rrt: api.ReifiedRelationship) =>
                    f.createReifiedRelationshipSourcePropertyPredicate(o2rj4.rextent, rbsj, rrt)
                  case (p: ReifiedRelationshipSourceInversePropertyPredicate, rrt: api.ReifiedRelationship) =>
                    f.createReifiedRelationshipSourceInversePropertyPredicate(o2rj4.rextent, rbsj, rrt)
                  case (p: ReifiedRelationshipTargetPropertyPredicate, rrt: api.ReifiedRelationship) =>
                    f.createReifiedRelationshipTargetPropertyPredicate(o2rj4.rextent, rbsj, rrt)
                  case (p: ReifiedRelationshipTargetInversePropertyPredicate, rrt: api.ReifiedRelationship) =>
                    f.createReifiedRelationshipTargetInversePropertyPredicate(o2rj4.rextent, rbsj, rrt)
                  case (p: UnreifiedRelationshipPropertyPredicate, urt: api.UnreifiedRelationship) =>
                    f.createUnreifiedRelationshipPropertyPredicate(o2rj4.rextent, urt, rbsj)
                  case (p: UnreifiedRelationshipInversePropertyPredicate, urt: api.UnreifiedRelationship) =>
                    f.createUnreifiedRelationshipInversePropertyPredicate(o2rj4.rextent, urt, rbsj)
                  case (p, t) =>
                    throw new java.lang.IllegalArgumentException(s"This should not happen:\np=$p\nt=$t")
                }
                val si_uuid = predi.uuid()
                val sj_uuid = predj.uuid.toString
                if (si_uuid != sj_uuid)
                  require(si_uuid == sj_uuid)
                val o2rj5 = o2rj4.copy(rextent = rl, segmentPredicates = o2rj4.segmentPredicates + (predi -> predj))
                ( o2rj5,
                  Option.empty[api.ChainRule],
                  Option(rbsj) )
            }
            o2rj2.right
          case (tboxj, urj) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertChainRules: Failed to resolve " +
                tboxj.fold(s" tbox: ${tboxi.getIri}")(_ => "") +
                urj.fold(s" head: ${uri.getName}")(_ => "")
            )).left
        }
        (o2rk, _, _) = tuple
        next = prev.updated(e, o2rk)
      } yield next
    }
  }

  protected def convertStructures
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: Structure => s })
    ss.foldLeft(state) { case (acc, sci) =>
      for {
        prev <- acc
        o2ri = prev(e)
        (rj, scj) = f.createStructure(
          o2ri.rextent,
          o2ri.tboxes(sci.getTbox),
          tables.taggedTypes.localName(sci.name()))
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: Scalar => s })
    ss.foldLeft(state) { case (acc, sci) =>
      for {
        prev <- acc
        o2ri = prev(e)
        (rj, scj) = f.createScalar(
          o2ri.rextent,
          o2ri.tboxes(sci.getTbox),
          tables.taggedTypes.localName(sci.name()))
        o2rj = o2ri.copy(rextent = rj, dataRanges = o2ri.dataRanges + (sci -> scj))
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  protected def convertRestrictedDataRanges
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (api.Module, api.Extent))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

    for {
      s <- state
      (mj, _) = entry
      e_o2r <- s.find(_._2.rextent.lookupModule(mj.uuid).isDefined) match {
        case Some(pair) =>
          pair.right
        case _ =>
          new EMFProblems(new java.lang.IllegalArgumentException(
            s"convertRestrictedDataRanges(${mj.iri}): Failed to lookup extent!"
          )).left
      }
      (ei, _) = e_o2r
      tboxes = ei.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
      ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: RestrictedDataRange => s })
      result <- ss.foldLeft(state) { case (acc, rdri) =>
        for {
          prev <- acc
          o2ri = prev(ei)
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
                    length = Option.apply(rri.getLength).map(emf2tables),
                    minLength = Option.apply(rri.getMinLength).map(emf2tables),
                    maxLength = Option.apply(rri.getMaxLength).map(emf2tables),
                    name = tables.taggedTypes.localName(rri.name))
                case rri: IRIScalarRestriction =>
                  f.createIRIScalarRestriction(
                    extent = o2ri.rextent,
                    tbox = tboxj,
                    restrictedRange = rsj,
                    length = Option.apply(rri.getLength).map(emf2tables),
                    minLength = Option.apply(rri.getMinLength).map(emf2tables),
                    maxLength = Option.apply(rri.getMaxLength).map(emf2tables),
                    pattern = Option.apply(rri.getPattern).map(p => tables.taggedTypes.literalPattern(p.value)),
                    name = tables.taggedTypes.localName(rri.name))
                case rri: NumericScalarRestriction =>
                  f.createNumericScalarRestriction(
                    extent = o2ri.rextent,
                    tbox = tboxj,
                    restrictedRange = rsj,
                    minExclusive = Option.apply(rri.getMinExclusive).map(emf2tables),
                    minInclusive = Option.apply(rri.getMinInclusive).map(emf2tables),
                    maxExclusive = Option.apply(rri.getMaxExclusive).map(emf2tables),
                    maxInclusive = Option.apply(rri.getMaxInclusive).map(emf2tables),
                    name = tables.taggedTypes.localName(rri.name))
                case rri: PlainLiteralScalarRestriction =>
                  f.createPlainLiteralScalarRestriction(
                    extent = o2ri.rextent,
                    tbox = tboxj,
                    restrictedRange = rsj,
                    length = Option.apply(rri.getLength).map(emf2tables),
                    minLength = Option.apply(rri.getMinLength).map(emf2tables),
                    maxLength = Option.apply(rri.getMaxLength).map(emf2tables),
                    pattern = Option.apply(rri.getPattern).map(emf2tables),
                    langRange = Option.apply(rri.getLangRange).map(emf2tables),
                    name = tables.taggedTypes.localName(rri.name))
                case rri: ScalarOneOfRestriction =>
                  f.createScalarOneOfRestriction(
                    extent = o2ri.rextent,
                    tbox = tboxj,
                    restrictedRange = rsj,
                    name = tables.taggedTypes.localName(rri.name))
                case rri: StringScalarRestriction =>
                  f.createStringScalarRestriction(
                    extent = o2ri.rextent,
                    tbox = tboxj,
                    restrictedRange = rsj,
                    length = Option.apply(rri.getLength).map(emf2tables),
                    minLength = Option.apply(rri.getMinLength).map(emf2tables),
                    maxLength = Option.apply(rri.getMaxLength).map(emf2tables),
                    pattern = Option.apply(rri.getPattern).map(p => tables.taggedTypes.literalPattern(p.value)),
                    name = tables.taggedTypes.localName(rri.name))
                case rri: SynonymScalarRestriction =>
                  f.createSynonymScalarRestriction(
                    extent = o2ri.rextent,
                    tbox = tboxj,
                    restrictedRange = rsj,
                    name = tables.taggedTypes.localName(rri.name))
                case rri: TimeScalarRestriction =>
                  f.createTimeScalarRestriction(
                    extent = o2ri.rextent,
                    tbox = tboxj,
                    restrictedRange = rsj,
                    minExclusive = Option.apply(rri.getMinExclusive).map(emf2tables),
                    minInclusive = Option.apply(rri.getMinInclusive).map(emf2tables),
                    maxExclusive = Option.apply(rri.getMaxExclusive).map(emf2tables),
                    maxInclusive = Option.apply(rri.getMaxInclusive).map(emf2tables),
                    name = tables.taggedTypes.localName(rri.name))
              }
              o2ri.copy(rextent = rj, dataRanges = o2ri.dataRanges + (rdri -> rdrj)).right
            case (tboxj, rsj) =>
              new EMFProblems(new java.lang.IllegalArgumentException(
                s"convertRestrictedDataRanges(${rdri.iri()}): Failed to resolve " +
                  tboxj.fold(s" tbox: ${tboxi.getIri}")(_ => "") +
                  rsj.fold(s" restricted data range: ${rsi.iri}")(_ => "")
              )).left
          }
          next = prev.updated(ei, o2rj)
        } yield next
      }
    } yield result
  }

  protected def convertScalarOneOfLiteralAxioms
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
        ( prev.get(tboxi.getExtent).flatMap(_.tboxes.get(tboxi)),
          prev.get(dri.getTbox.getExtent).flatMap(_.dataRanges.get(dri))) match {
          case (Some(tboxj), Some(drj: api.ScalarOneOfRestriction)) =>
            Option(li.getValueType) match {
              case Some(vti) =>
                prev.get(vti.getTbox.getExtent).flatMap(_.dataRanges.get(vti)) match {
                  case Some(vtj: api.DataRange) =>
                    val (rj, lj) = f.createScalarOneOfLiteralAxiom(
                      extent = o2ri.rextent,
                      tbox = tboxj,
                      axiom = drj,
                      value = emf2tables(li.getValue),
                      valueType = Some(vtj))
                    o2ri.copy(
                      rextent = rj,
                      scalarOneOfLiterals = o2ri.scalarOneOfLiterals + (li -> lj),
                      termAxioms = o2ri.termAxioms + (li -> lj)).right
                  case _ =>
                    new EMFProblems(new java.lang.IllegalArgumentException(
                      s"convertScalarOneLiteralAxioms: Cannot find value type $vti for ${li.getAxiom.abbrevIRI()}"
                    )).left
                }
              case None =>
                val (rj, lj) = f.createScalarOneOfLiteralAxiom(
                  extent = o2ri.rextent,
                  tbox = tboxj,
                  axiom = drj,
                  value = emf2tables(li.getValue),
                  valueType = None)
                o2ri.copy(
                  rextent = rj,
                  scalarOneOfLiterals = o2ri.scalarOneOfLiterals + (li -> lj),
                  termAxioms = o2ri.termAxioms + (li -> lj)).right
            }
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
              tables.taggedTypes.localName(dpi.name()))
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
              tables.taggedTypes.localName(dpi.name()))
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
              tables.taggedTypes.localName(dpi.name()))
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
              tables.taggedTypes.localName(dpi.name()))
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
                Option(axpi.getValueType) match {
                  case Some(vti) =>
                    prev.get(vti.getTbox.getExtent).flatMap(_.dataRanges.get(vti)) match {
                      case Some(vtj) =>
                        f
                          .createEntityScalarDataPropertyParticularRestrictionAxiom(
                            o2ri.rextent, tboxj, ej, dpj,
                            emf2tables(axpi.getLiteralValue), Some(vtj)) match {
                          case (rk, ak) => rk -> ak.right
                        }
                      case _ =>
                        o2ri.rextent -> new EMFProblems(new java.lang.IllegalArgumentException(
                          s"convertEntityScalarDataPropertyRestrictionAxioms: " +
                            s"Cannot find ValueType: ${vti.abbrevIRI()}"
                        )).left
                    }
                  case None =>
                    f
                      .createEntityScalarDataPropertyParticularRestrictionAxiom(
                        o2ri.rextent, tboxj, ej, dpj,
                        emf2tables(axpi.getLiteralValue), None) match {
                      case (rk, ak) => rk -> ak.right
                    }
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

  protected def convertEntityStructuredDataPropertyRestrictionAxioms
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: EntityStructuredDataPropertyParticularRestrictionAxiom => s })
    ss.foldLeft(state) { case (acc, axi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        tboxi = axi.getTbox
        ei = axi.getRestrictedEntity
        dpi = axi.getStructuredDataProperty
        dptboxi = dpi match {
          case edp: EntityStructuredDataProperty =>
            edp.getTbox()
          case sdp: StructuredDataProperty =>
            sdp.getTbox()
        }
        o2rj <-
        (prev.get(tboxi.getExtent).flatMap(_.tboxes.get(tboxi)),
          prev.get(ei.getTbox.getExtent).flatMap(_.entityLookup(ei)),
          prev.get(dptboxi.getExtent).flatMap(_.dataRelationshipToStructureLookup(dpi))) match {
          case (Some(tboxj), Some(ej), Some(dpj)) =>
            val (rj, axj) =
              f.createEntityStructuredDataPropertyParticularRestrictionAxiom(o2ri.rextent, tboxj, dpj, ej)
            convertRestrictionStructuredDataPropertyContext(o2ri.copy(rextent = rj, termAxioms = o2ri.termAxioms + (axi -> axj)), Seq(axi -> axj))
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertEntityStructuredDataPropertyRestrictionAxioms: Cannot find EntityScalarDataPropertyRestrictionAxiom for" +
                s": restricted entity: ${axi.getRestrictedEntity.abbrevIRI()}" +
                s", structured data property: ${axi.getStructuredDataProperty.abbrevIRI()}"
            )).left
        }
        next = prev.updated(e, o2rj)
      } yield next
    }
  }

  @scala.annotation.tailrec
  protected final def convertRestrictionStructuredDataPropertyContext
  (o2r: OMLText2Resolver,
   cs: Seq[(RestrictionStructuredDataPropertyContext, api.RestrictionStructuredDataPropertyContext)])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ OMLText2Resolver
  = if (cs.isEmpty)
    o2r.right
  else {
    val (ci, cj) = cs.head
    val values
    : EMFProblems \/ OMLText2Resolver
    = ci.getScalarDataPropertyRestrictions.asScala.foldLeft(o2r.right[EMFProblems]) { case (acc, vi) =>
      for {
        o2r1 <- acc
        lv = emf2tables(vi.getScalarPropertyValue)
        o2r2 <- (
          o2r1.dataRelationshipToScalarLookup(vi.getScalarDataProperty),
          Option(vi.getValueType),
          Option(vi.getValueType).flatMap(o2r1.dataRanges.get)
        ) match {
          case (Some(dpj), Some(_), Some(vtj)) =>
            val (rj, _) =
              f.createRestrictionScalarDataPropertyValue(o2r1.rextent, dpj, lv, cj, Some(vtj))
            o2r1.copy(rextent = rj).right

          case (Some(dpj), None, _) =>
            val (rj, _) =
              f.createRestrictionScalarDataPropertyValue(o2r1.rextent, dpj, lv, cj, None)
            o2r1.copy(rextent = rj).right

          case (dpj, vti, vtj) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRestrictionStructuredDataPropertyContext: failed to resolve: " +
                dpj.fold{ s"\n scalar data property: ${vi.getScalarDataProperty}"}{_ => ""} +
                vti.fold{""}{ ti => vtj.fold{ s"\n value type: $ti" }{_ => " "} }
            )).left
        }
      } yield o2r2
    }

    val tuples
    : EMFProblems \/
      (OMLText2Resolver, Seq[(RestrictionStructuredDataPropertyContext, api.RestrictionStructuredDataPropertyContext)])
    = ci.getStructuredDataPropertyRestrictions.asScala.foldLeft {
      values.map(_ -> Seq.empty[(RestrictionStructuredDataPropertyContext, api.RestrictionStructuredDataPropertyContext)])
    } { case (acc, si) =>
      for {
        tuple1 <- acc
        (o2r1, prev) = tuple1
        tuple2 <- o2r1.dataRelationshipToStructureLookup(si.getStructuredDataProperty) match {
          case Some(dpj) =>
            val (rj, sj) =
              f.createRestrictionStructuredDataPropertyTuple(o2r1.rextent, dpj, cj)
            (o2r1.copy(rextent = rj) -> (prev :+ (si -> sj))).right
          case None =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRestrictionStructuredDataPropertyContext: failed to resolve: " +
                s"\n structured data property: ${si.getStructuredDataProperty}"
            )).left
        }
        (o2r2, next) = tuple2
      } yield o2r2 -> next
    }

    tuples match {
      case \/-((o2r_updated, inc)) =>
        convertRestrictionStructuredDataPropertyContext(o2r_updated, cs.tail ++ inc)
      case -\/(errors) =>
        -\/(errors)
    }
  }

  protected def convertSpecializationAxioms
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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

  protected def convertSubDataPropertyOfAxioms
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: SubDataPropertyOfAxiom => s })
    ss.foldLeft(state) { case (acc, axi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        tboxi = axi.getTbox
        subi = axi.getSubProperty()
        supi = axi.getSuperProperty()
        (rj, axj: \/[EMFProblems, api.SubDataPropertyOfAxiom]) =
        (prev.get(tboxi.getExtent).flatMap(_.tboxes.get(tboxi)),
          prev.get(subi.getTbox.getExtent).flatMap(_.entityScalarDataProperties.get(subi)),
          prev.get(supi.getTbox.getExtent).flatMap(_.entityScalarDataProperties.get(supi))) match {
          case (Some(tboxj), Some(subj: api.EntityScalarDataProperty), Some(supj: api.EntityScalarDataProperty)) =>
            f.createSubDataPropertyOfAxiom(o2ri.rextent, tboxj, subj, supj) match {
              case (rk, ak) => rk -> ak.right
            }
          case (tboxj, subj, supj) =>
            o2ri.rextent -> new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertSubDataPropertyOfAxioms: Cannot find: " +
                s"tbox: ${tboxi.abbrevIRI()}" +
                s"subProperty: ${subi.abbrevIRI()}" +
                s"superProperty: ${supi.abbrevIRI()}"
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

  protected def convertSubObjectPropertyOfAxioms
  (state: EMFProblems \/ Map[Extent, OMLText2Resolver],
   entry: (Extent, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Extent, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

    val (e, _) = entry
    val tboxes = e.getModules.selectByKindOf { case tbox: TerminologyBox => tbox }
    val ss = tboxes.flatMap(_.getBoxStatements.selectByKindOf { case s: SubObjectPropertyOfAxiom => s })
    ss.foldLeft(state) { case (acc, axi) =>
      for {
        prev <- acc
        o2ri = prev(e)
        tboxi = axi.getTbox
        subi = axi.getSubProperty()
        supi = axi.getSuperProperty()
        (rj, axj: \/[EMFProblems, api.SubObjectPropertyOfAxiom]) =
        (prev.get(tboxi.getExtent).flatMap(_.tboxes.get(tboxi)),
          prev.get(subi.getTbox.getExtent).flatMap(_.unreifiedRelationships.get(subi)),
          prev.get(supi.getTbox.getExtent).flatMap(_.unreifiedRelationships.get(supi))) match {
          case (Some(tboxj), Some(subj: api.UnreifiedRelationship), Some(supj: api.UnreifiedRelationship)) =>
            f.createSubObjectPropertyOfAxiom(o2ri.rextent, tboxj, subj, supj) match {
              case (rk, ak) => rk -> ak.right
            }
          case (tboxj, subj, supj) =>
            o2ri.rextent -> new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertSubObjectPropertyOfAxioms: Cannot find: " +
                s"tbox: ${tboxi.abbrevIRI()}" +
                s"subProperty: ${subi.abbrevIRI()}" +
                s"superProperty: ${supi.abbrevIRI()}"
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
          val (rj, axj) = f.createAnonymousConceptUnionAxiom(
            o2r.rextent,
            p,
            tables.taggedTypes.localName(axi.getName))
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
        val (rj, axj) = f.createAnonymousConceptUnionAxiom(
          o2r.rextent,
          cxj,
          tables.taggedTypes.localName(axi.getName))
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
            val (rj, dj) = f.createConceptInstance(
              o2ri.rextent,
              dboxj,
              clj,
              tables.taggedTypes.localName(di.getName))
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
            val (rj, dj) = f.createReifiedRelationshipInstance(
              o2ri.rextent,
              dboxj,
              clj,
              tables.taggedTypes.localName(di.getName))
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
            Option(di.getValueType) match {
              case Some(vti) =>
                prev.get(vti.getTbox.getExtent).flatMap(_.dataRanges.get(vti)) match {
                  case Some(vtj) =>
                    val (rj, dj) = f.createSingletonInstanceScalarDataPropertyValue(
                      o2ri.rextent, dboxj, sj, dpj,
                      emf2tables(di.getScalarPropertyValue), Some(vtj))
                    o2ri.copy(
                      rextent = rj,
                      singletonInstanceScalarDataPropertyValues = o2ri.singletonInstanceScalarDataPropertyValues + (di -> dj)).right
                  case None =>
                    new EMFProblems(new java.lang.IllegalArgumentException(
                      s"convertSingletonScalarDataPropertyValues: Cannot find:" +
                        s" singleton instance: ${si.abbrevIRI}" +
                        s" entity scalar data property: ${dpi.abbrevIRI}" +
                        s" value type: ${vti.abbrevIRI}"
                    )).left
                }
              case None =>
                val (rj, dj) = f.createSingletonInstanceScalarDataPropertyValue(
                  o2ri.rextent, dboxj, sj, dpj,
                  emf2tables(di.getScalarPropertyValue), None)
                o2ri.copy(
                  rextent = rj,
                  singletonInstanceScalarDataPropertyValues = o2ri.singletonInstanceScalarDataPropertyValues + (di -> dj)).right
            }
          case _ =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertSingletonScalarDataPropertyValues: Cannot find:" +
                s" singleton instance: ${si.abbrevIRI}" +
                s" entity scalar data property: ${dpi.abbrevIRI}" +
                Option(di.getValueType).fold{""}{ vti => s" value type: ${vti.abbrevIRI}" }
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
    import gov.nasa.jpl.imce.oml.processor.utils.EMFFilterable._

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
        val evtj = Option.apply(sci.getValueType) match {
          case Some(vti) =>
            o2r.dataRanges.get(vti) match {
              case Some(vtj) =>
                Some(vtj).right
              case _ =>
                new EMFProblems(new java.lang.IllegalArgumentException(
                  s"convertSingletonInstanceStructuredDataPropertyContext: Cannot find: " +
                    s"value type: ${vti.abbrevIRI}"
                )).left
            }
          case None =>
            None.right
        }
        evtj match {
          case \/-(vtj) =>
            val (rj, scj) = f.createScalarDataPropertyValue(
              o2r.rextent, sck,
              emf2tables(sci.getScalarPropertyValue), ctx, vtj)
            val next = o2r.copy(
              rextent = rj,
              scalarDataPropertyValues = o2r.scalarDataPropertyValues + (sci -> scj))
            convertSingletonInstanceStructuredDataPropertyContext(next, sct, Nil)
          case -\/(errors) =>
            -\/(errors)
        }
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
        for {
          prev <- acc1
          allO2Rs = prev.values
          annotationProperties = allO2Rs.flatMap(_.aps).toMap
          o2ri = prev(e)
          as = mi.getAnnotations.asScala ++
            mi.moduleElements().asScala.flatMap(_.getAnnotations.asScala) ++
            mi.moduleEdges().asScala.flatMap(_.getAnnotations.asScala)
          o2rj <- as.foldLeft(o2ri.right[EMFProblems]) { case (acc2, a0) =>
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
              (el, _) = f.createAnnotationPropertyValue(
                o2rk.rextent,
                aSubject,
                aProperty,
                tables.taggedTypes.stringDataType(a0.getValue.value))
              o2rl = o2rk.copy(rextent = el)
            } yield o2rl
          }
          next = prev.updated(e, o2rj)
        } yield next
    }
  }

  def convert
  (fileExtents: Map[Extent, RelPath])
  (implicit factory: api.OMLResolvedFactory)
  : EMFProblems \/ (Map[Extent, OMLText2Resolver], Seq[(api.Module, api.Extent)])
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

    c2N = c25

    // Topo sort of the modules

    modules = c2N.foldLeft(Map.empty[api.Module, api.Extent]) { case (acc, (_, t2r)) =>
      val modules
      : Seq[api.Module]
      = Seq.empty[api.Module] ++
        t2r.rextent.terminologyGraphs.values ++
        t2r.rextent.bundles.values ++
        t2r.rextent.descriptionBoxes.values

      scala.Predef.require(modules.size == 1)
      acc + (modules(0) -> t2r.rextent)
    }

    moduleEdges = c2N.values.foldLeft(Map.empty[api.ModuleEdge, api.Extent]) { case (acc, t2r) =>
      val ext = t2r.rextent
      acc ++
        ext.terminologyBoxAxiomByUUID.values.map(_ -> ext) ++
        ext.terminologyBundleAxiomByUUID.values.map(_ -> ext) ++
        ext.descriptionBoxExtendsClosedWorldDefinitionsByUUID.values.map(_ -> ext) ++
        ext.descriptionBoxRefinementByUUID.values.map(_ -> ext)
    }

    sortedModuleExtents <- resolver.sortExtents(modules, moduleEdges).leftMap(ts => EMFProblems(exceptions = ts.to[List]))

    // Relationships
    c30 = c2N

    c31 <- c30.foldLeft(c30.right[EMFProblems])(convertReifiedRelationships)

    c32 <- c31.foldLeft(c31.right[EMFProblems])(convertUnreifiedRelationships)

    // ChainRules
    c40 = c32

    t2rs = c40.values.to[Set]
    allAs = t2rs.flatMap(_.aspects).toMap
    allCs = t2rs.flatMap(_.concepts).toMap
    allRRs = t2rs.flatMap(_.reifiedRelationships).toMap
    allURs = t2rs.flatMap(_.unreifiedRelationships).toMap

    c41 <- c40.foldLeft(c40.right[EMFProblems])(convertChainRules(allAs, allCs, allRRs, allURs))

    // DataTypes

    c50 = c41

    c51 <- c50.foldLeft(c50.right[EMFProblems])(convertStructures)

    c52 <- c51.foldLeft(c51.right[EMFProblems])(convertScalars)

    c53 <- sortedModuleExtents.foldLeft(c52.right[EMFProblems])(convertRestrictedDataRanges)

    c54 <- c53.foldLeft(c53.right[EMFProblems])(convertScalarOneOfLiteralAxioms)

    // DataRelationships
    c60 = c54

    c61 <- c60.foldLeft(c60.right[EMFProblems])(convertEntityScalarDataProperties)

    c62 <- c61.foldLeft(c61.right[EMFProblems])(convertEntityStructuredDataProperties)

    c63 <- c62.foldLeft(c62.right[EMFProblems])(convertScalarDataProperties)

    c64 <- c63.foldLeft(c63.right[EMFProblems])(convertStructuredDataProperties)

    // Restrictions
    c70 = c64

    c71 <- c70.foldLeft(c70.right[EMFProblems])(convertEntityRestrictionAxioms)

    c72 <- c71.foldLeft(c71.right[EMFProblems])(convertEntityScalarDataPropertyRestrictionAxioms)

    c73 <- c72.foldLeft(c72.right[EMFProblems])(convertEntityStructuredDataPropertyRestrictionAxioms)

    // Specializations
    c80 = c73

    c81 <- c80.foldLeft(c80.right[EMFProblems])(convertSpecializationAxioms)
    c82 <- c81.foldLeft(c81.right[EMFProblems])(convertSubDataPropertyOfAxioms)
    c83 <- c82.foldLeft(c82.right[EMFProblems])(convertSubObjectPropertyOfAxioms)

    // Disjunctions
    c90 = c83

    c91 <- c90.foldLeft(c90.right[EMFProblems])(convertRootConceptTaxonomyAxioms)

    // ConceptualEntityInstances
    cA0 = c81

    cA1 <- cA0.foldLeft(cA0.right[EMFProblems])(convertConceptInstances)

    cA2 <- cA1.foldLeft(cA1.right[EMFProblems])(convertReifiedRelationshipInstances)

    cA3 <- cA2.foldLeft(cA2.right[EMFProblems])(convertReifiedRelationshipInstanceDomains)

    cA4 <- cA3.foldLeft(cA3.right[EMFProblems])(convertReifiedRelationshipInstanceRanges)

    cA5 <- cA4.foldLeft(cA4.right[EMFProblems])(convertUnreifiedRelationshipInstances)

    // Data Property Values
    cB0 = cA5

    cB1 <- cB0.foldLeft(cB0.right[EMFProblems])(convertSingletonScalarDataPropertyValues)

    cB2 <- cB1.foldLeft(cB1.right[EMFProblems])(convertSingletonStructuredDataPropertyValues)

    cC0 = cB2

    // AnnotationProperties
    cC1 <- cC0.foldLeft(cC0.right[EMFProblems])(convertAnnotations)
    cCN = cC1

    finalSortedModuleExtents <- sortedModuleExtents.foldLeft(Seq.empty[(api.Module, api.Extent)].right[EMFProblems]) {
      case (acc, (mi, _)) =>
        cCN.find(_._2.includesAPIModule(mi)).map(_._2.rextent) match {
          case Some(ei) =>
            acc.map(_ :+ (mi -> ei))
          case None =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convert() failed to find final extent for module ${mi.iri}"
            )).left
        }
    }

  } yield cCN -> finalSortedModuleExtents

}
