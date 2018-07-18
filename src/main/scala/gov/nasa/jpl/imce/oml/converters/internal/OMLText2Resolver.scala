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

import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.converters.{ConversionCommand, emf2tables}
import gov.nasa.jpl.imce.oml.converters.utils.EMFProblems
import gov.nasa.jpl.imce.oml.model.extensions.OMLExtensions
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
import scala.Predef.{ArrowAssoc, String}
import scala.{Boolean, None, Option, Some, StringContext}
import scalaz._
import Scalaz._

import scala.collection.mutable

case class OMLText2Resolver
(iri: tables.taggedTypes.IRI,
 rextent: api.Extent,
 queue_edges: Set[ModuleEdge] = Set.empty,
 queue_elements: Set[ModuleElement] = Set.empty,

 tboxes: Map[TerminologyBox, api.TerminologyBox] = Map.empty,
 dboxes: Map[DescriptionBox, api.DescriptionBox] = Map.empty,

 allImportedModules: Set[Module] = Set.empty,
 moduleEdges: Map[ModuleEdge, api.ModuleEdge] = Map.empty,

 aps: Map[AnnotationProperty, api.AnnotationProperty] = Map.empty,

// TerminologyBoxStatements

 aspects: Map[AspectKind, api.AspectKind] = Map.empty,
 concepts: Map[ConceptKind, api.ConceptKind] = Map.empty,
 cardinalityRestrictedReifiedRelationships: Map[CardinalityRestrictedReifiedRelationship, api.CardinalityRestrictedReifiedRelationship] = Map.empty,
 reifiedRelationshipRestrictions: Map[ReifiedRelationshipRestriction, api.ReifiedRelationshipRestriction] = Map.empty,
 reifiedRelationships: Map[ReifiedRelationship, api.ReifiedRelationship] = Map.empty,
 forwardProperties: Map[ForwardProperty, api.ForwardProperty] = Map.empty,
 inverseProperties: Map[InverseProperty, api.InverseProperty] = Map.empty,
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

  def isResolved: Boolean
  = queue_edges.isEmpty &&
    queue_elements.isEmpty

  def includesAPIModule(m: api.Module): Boolean = tboxes.values.find(_ == m).orElse(dboxes.values.find(_ == m)).isDefined

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
    case x: AspectKind =>
      aspects.get(x)
    case x: ConceptKind =>
      concepts.get(x)
    case x: CardinalityRestrictedReifiedRelationship =>
      cardinalityRestrictedReifiedRelationships.get(x)
    case x: ReifiedRelationshipRestriction =>
      reifiedRelationshipRestrictions.get(x)
    case x: ReifiedRelationship =>
      reifiedRelationships.get(x)
    case x: ForwardProperty =>
      forwardProperties.get(x)
    case x: InverseProperty =>
      inverseProperties.get(x)
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
    case x: ChainRule =>
      chainRules.get(x)
    case x: RuleBodySegment =>
      ruleBodySegments.get(x)
    case x: SegmentPredicate =>
      segmentPredicates.get(x)
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
    case a: AspectKind => aspects.get(a)
    case c: ConceptKind => concepts.get(c)
    case crr: CardinalityRestrictedReifiedRelationship => cardinalityRestrictedReifiedRelationships.get(crr)
    case rs: ReifiedRelationshipRestriction => reifiedRelationshipRestrictions.get(rs)
    case rr: ReifiedRelationship => reifiedRelationships.get(rr)
    case _ => None
  }

  def conceptualRelationshipLookup(e: ConceptualRelationship): Option[api.ConceptualRelationship] = e match {
    case crr: CardinalityRestrictedReifiedRelationship => cardinalityRestrictedReifiedRelationships.get(crr)
    case rs: ReifiedRelationshipRestriction => reifiedRelationshipRestrictions.get(rs)
    case rr: ReifiedRelationship => reifiedRelationships.get(rr)
  }

  def restrictableRelationshipLookup
  (rr: RestrictableRelationship)
  : Option[api.RestrictableRelationship]
  = rr match {
    case f: ForwardProperty =>
      forwardProperties.get(f)
    case i: InverseProperty =>
      inverseProperties.get(i)
    case ur: UnreifiedRelationship =>
      unreifiedRelationships.get(ur)
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

  override def toString: String = {
    val buff = new scala.collection.mutable.StringBuilder
    buff ++= s"OMLText2Resolver(iri=$iri) {\n"
    buff ++= "// queues (must be empty for successful resolution)\n"
    buff ++= s"queue_edges: ${queue_edges.size}\n"
    buff ++= s"queue_elements: ${queue_elements.size}\n"
    buff ++= "// resolved state\n"
    buff ++= s"tboxes: ${tboxes.size}\n"
    buff ++= s"dboxes: ${dboxes.size}\n"
    buff ++= s"moduleEdges: ${moduleEdges.size}\n"
    buff ++= s"aps: ${aps.size}\n"
    buff ++= s"aspects: ${aspects.size}\n"
    buff ++= s"concepts: ${concepts.size}\n"
    buff ++= s"cardinalityRestrictedReifiedRelationships: ${cardinalityRestrictedReifiedRelationships.size}\n"
    buff ++= s"reifiedRelationshipRestrictions: ${reifiedRelationshipRestrictions.size}\n"
    buff ++= s"reifiedRelationships: ${reifiedRelationships.size}\n"
    buff ++= s"forwardProperties: ${forwardProperties.size}\n"
    buff ++= s"inverseProperties: ${inverseProperties.size}\n"
    buff ++= s"unreifiedRelationships: ${unreifiedRelationships.size}\n"
    buff ++= s"chainRules: ${chainRules.size}\n"
    buff ++= s"ruleBodySegments: ${ruleBodySegments.size}\n"
    buff ++= s"segmentPredicates: ${segmentPredicates.size}\n"
    buff ++= s"dataRanges: ${dataRanges.size}\n"
    buff ++= s"structures: ${structures.size}\n"
    buff ++= s"scalarOneOfLiterals: ${scalarOneOfLiterals.size}\n"
    buff ++= s"entityScalarDataProperties: ${entityScalarDataProperties.size}\n"
    buff ++= s"entityStructuredDataProperties: ${entityStructuredDataProperties.size}\n"
    buff ++= s"scalarDataProperties: ${scalarDataProperties.size}\n"
    buff ++= s"structuredDataProperties: ${structuredDataProperties.size}\n"
    buff ++= s"termAxioms: ${termAxioms.size}\n"
    buff ++= s"conceptTreeDisjunctions: ${conceptTreeDisjunctions.size}\n"
    buff ++= s"disjointUnionOfConceptAxioms: ${disjointUnionOfConceptAxioms.size}\n"
    buff ++= s"conceptualEntitySingletonInstances: ${conceptualEntitySingletonInstances.size}\n"
    buff ++= s"reifiedRelationshipInstanceDomains: ${reifiedRelationshipInstanceDomains.size}\n"
    buff ++= s"reifiedRelationshipInstanceRanges: ${reifiedRelationshipInstanceRanges.size}\n"
    buff ++= s"unreifiedRelationshipInstanceTuples: ${unreifiedRelationshipInstanceTuples.size}\n"
    buff ++= s"singletonInstanceStructuredDataPropertyValues: ${singletonInstanceStructuredDataPropertyValues.size}\n"
    buff ++= s"singletonInstanceScalarDataPropertyValues: ${singletonInstanceScalarDataPropertyValues.size}\n"
    buff ++= s"scalarDataPropertyValues: ${scalarDataPropertyValues.size}\n"
    buff ++= s"structuredDataPropertyTuples: ${structuredDataPropertyTuples.size}\n"
    buff ++= "}\n"
    buff.toString
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

  implicit def toCardinalityRestrictionKind
  (k: CardinalityRestrictionKind)
  : tables.CardinalityRestrictionKind
  = k match {
    case CardinalityRestrictionKind.MIN =>
      tables.MinCardinalityRestriction
    case CardinalityRestrictionKind.MAX =>
      tables.MaxCardinalityRestriction
    case CardinalityRestrictionKind.EXACT =>
      tables.ExactCardinalityRestriction
  }

  implicit class ModuleLookup(val s: Map[Module, OMLText2Resolver]) {

    def isResolved
    : Boolean
    = s.values.forall(_.isResolved)

    def lookupTerminologyBox(tbox: TerminologyBox)
    : Option[api.TerminologyBox]
    = s
      .get(tbox)
      .flatMap(o2r => o2r.tboxes.get(tbox))

    def lookupTerminologyGraph(tbox: TerminologyBox)
    : Option[api.TerminologyGraph]
    = lookupTerminologyBox(tbox) match {
      case Some(tg: api.TerminologyGraph) =>
        Some(tg)
      case _ =>
        None
    }

    def lookupBundle(b0: Bundle)
    : Option[api.Bundle]
    = lookupTerminologyBox(b0) match {
      case Some(b1: api.Bundle) =>
        Some(b1)
      case _ =>
        None
    }

    def lookupDescriptionBox(dbox: DescriptionBox)
    : Option[api.DescriptionBox]
    = s
      .get(dbox)
      .flatMap(o2r => o2r.dboxes.get(dbox))

    def lookupConceptKind(o: Option[ConceptKind])
    : Option[api.ConceptKind]
    = o.flatMap { ck =>
      Option
        .apply(ck.getTbox)
        .flatMap(s.get)
        .flatMap { o2r =>
          o2r.concepts.get(ck)
        }
    }

    def accessibleLookup[T]
    (o2r: OMLText2Resolver,
     f: OMLText2Resolver => Option[T])
    : Option[T]
    = {

      def importedLookup
      (ms: Set[Module])
      : Option[T]
      = if (ms.nonEmpty)
        s.get(ms.head).flatMap(f) match {
          case Some(x) =>
            Some(x)
          case _ =>
            importedLookup(ms.tail)
        }
      else
        None

      f(o2r) orElse
        importedLookup(o2r.allImportedModules)
    }
  }

  protected def convertModules
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    val ext = new OMLExtensions
    val (mi, _) = entry
    for {
      prev <- state
      o2ri = prev(mi)
      o2rj = mi match {
        case gi: TerminologyGraph =>
          val (rj, gj) = f.createTerminologyGraph(
            o2ri.rextent,
            convertTerminologyGraphKind(gi.getKind),
            tables.taggedTypes.iri(gi.iri()))
          o2ri.copy(
            rextent = rj,
            tboxes = o2ri.tboxes + (gi -> gj),
            allImportedModules = ext.allImportedModules(mi).asScala.to[Set],
            queue_edges = o2ri.queue_edges ++ gi.moduleEdges.asScala.to[Set],
            queue_elements = o2ri.queue_elements ++ gi.moduleElements.asScala.to[Set])
        case bi: Bundle =>
          val (rj, bj) = f.createBundle(
            o2ri.rextent,
            convertTerminologyGraphKind(bi.getKind),
            tables.taggedTypes.iri(bi.iri()))
          o2ri.copy(
            rextent = rj,
            allImportedModules = ext.allImportedModules(mi).asScala.to[Set],
            tboxes = o2ri.tboxes + (bi -> bj),
            queue_edges = o2ri.queue_edges ++ bi.moduleEdges.asScala.to[Set],
            queue_elements = o2ri.queue_elements ++ bi.moduleElements.asScala.to[Set])
        case di: DescriptionBox =>
          val (rj, dj) = f.createDescriptionBox(
            o2ri.rextent,
            convertDescriptionKind(di.getKind),
            tables.taggedTypes.iri(di.iri()))
          o2ri.copy(
            rextent = rj,
            allImportedModules = ext.allImportedModules(mi).asScala.to[Set],
            dboxes = o2ri.dboxes + (di -> dj),
            queue_edges = o2ri.queue_edges ++ di.moduleEdges.asScala.to[Set],
            queue_elements = o2ri.queue_elements ++ di.moduleElements.asScala.to[Set])
      }
      next = prev.updated(mi, o2rj)
    } yield next
  }

  protected def convertAnnotationProperties
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    val (mi, _) = entry
    for {
      previ <- state
      o2ri = previ(mi)
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
      nexti = previ.updated(mi, o2rl)
    } yield nexti
  }

  protected def convertTerminologyExtensions
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
    entry match {
      case (tbox: TerminologyBox, _) =>
        val axs = tbox.getBoxAxioms.selectByKindOf { case ax: TerminologyExtensionAxiom => ax }
        axs.foldLeft(state) { case (acc, axi) =>
          for {
            prev <- acc
            o2ri = prev(tbox)
            extendingi = axi.extendingTerminology()
            extendedi = axi.getExtendedTerminology
            o2rj <-
              ( prev.get(extendingi).flatMap(_.tboxes.get(extendingi)),
                prev.get(extendedi).flatMap(_.tboxes.get(extendedi)) ) match {
                case (Some(extendingj), Some(extendedj)) =>
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
            next = prev.updated(tbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }





  protected def convertAspects
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
    entry match {
      case (tbox: TerminologyBox, _) =>
        val ss = tbox.getBoxStatements.selectByKindOf { case s: Aspect => s }
        ss.foldLeft(state) { case (acc, ai) =>
          for {
            prev <- acc
            o2ri = prev(tbox)
            (rj, aj) = f.createAspect(
              o2ri.rextent,
              o2ri.tboxes(ai.getTbox),
              tables.taggedTypes.localName(ai.name()))
            o2rj = o2ri.copy(rextent = rj, aspects = o2ri.aspects + (ai -> aj))
            next = prev.updated(tbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertConcepts
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
    entry match {
      case (tbox: TerminologyBox, _) =>
        val ss = tbox.getBoxStatements.selectByKindOf { case s: Concept => s }
        ss.foldLeft(state) { case (acc, ci) =>
          for {
            prev <- acc
            o2ri = prev(tbox)
            (rj, cj) = f.createConcept(
              o2ri.rextent,
              o2ri.tboxes(ci.getTbox),
              tables.taggedTypes.localName(ci.name()))
            o2rj = o2ri.copy(rextent = rj, concepts = o2ri.concepts + (ci -> cj))
            next = prev.updated(tbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertStructures
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
    entry match {
      case (tbox: TerminologyBox, _) =>
        val ss = tbox.getBoxStatements.selectByKindOf { case s: Structure => s }
        ss.foldLeft(state) { case (acc, sci) =>
          for {
            prev <- acc
            o2ri = prev(tbox)
            (rj, scj) = f.createStructure(
              o2ri.rextent,
              o2ri.tboxes(sci.getTbox),
              tables.taggedTypes.localName(sci.name()))
            o2rj = o2ri.copy(rextent = rj, structures = o2ri.structures + (sci -> scj))
            next = prev.updated(tbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertScalars
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
    entry match {
      case (tbox: TerminologyBox, _) =>
        val ss = tbox.getBoxStatements.selectByKindOf { case s: Scalar => s }
        ss.foldLeft(state) { case (acc, sci) =>
          for {
            prev <- acc
            o2ri = prev(tbox)
            (rj, scj) = f.createScalar(
              o2ri.rextent,
              o2ri.tboxes(sci.getTbox),
              tables.taggedTypes.localName(sci.name()))
            o2rj = o2ri.copy(rextent = rj, dataRanges = o2ri.dataRanges + (sci -> scj))
            next = prev.updated(tbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertRestrictedDataRanges
  (state: Map[Module, OMLText2Resolver])
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

    val rrs: Iterable[RestrictedDataRange]
    = state.foldLeft(Seq.empty[RestrictedDataRange]) {
      case (prev, (tbox: TerminologyBox, _)) =>
        val ss = tbox.getBoxStatements.selectByKindOf { case s: RestrictedDataRange => s }
        prev ++ ss
      case (prev, _) =>
        prev
    }

    val result = convertRestrictedDataRanges(state.right, rrs, List.empty)
    result
  }

  @scala.annotation.tailrec
  protected def convertRestrictedDataRanges
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   rdrs: Iterable[RestrictedDataRange],
   queue: List[RestrictedDataRange],
   progress: Boolean = false)
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = if (rdrs.isEmpty) {
    if (queue.isEmpty)
      state
    else if (progress)
      convertRestrictedDataRanges(state, queue, List.empty)
    else
      new EMFProblems(new java.lang.IllegalArgumentException(
        ConversionCommand.explainProblems(
          s"convertRestrictedDataRanges(...): Failed to resolve ${queue.size} data ranges",
          queue.map(_.getName))
      )).left
  } else
    state match {
      case \/-(prev) =>
        val rdri: RestrictedDataRange = rdrs.head
        val tboxi: TerminologyBox = rdri.getTbox
        val o2ri: OMLText2Resolver = prev(tboxi)
        val rsi: DataRange = rdri.getRestrictedRange
        ( prev.get(tboxi).flatMap(_.tboxes.get(tboxi)),
          prev.get(rsi.getTbox).flatMap(_.dataRanges.get(rsi)) ) match {
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
            val o2rj = o2ri.copy(rextent = rj, dataRanges = o2ri.dataRanges + (rdri -> rdrj))
            convertRestrictedDataRanges(prev.updated(tboxi, o2rj).right, rdrs.tail, queue, progress = true)
          case (Some(_), None) =>
            val rest = rdrs.tail
            if (rest.isEmpty)
              convertRestrictedDataRanges(state, rdri :: queue, List.empty, progress)
            else
              convertRestrictedDataRanges(state, rest, rdri :: queue)
          case (tboxj, rsj) =>
            new EMFProblems(new java.lang.IllegalArgumentException(
              s"convertRestrictedDataRanges(${rdri.iri()}): Failed to resolve " +
                tboxj.fold(s" tbox: ${tboxi.getIri}")(_ => "") +
                rsj.fold(s" restricted data range: ${rsi.iri}")(_ => "")
            )).left
        }
      case _ =>
        state
    }

  protected def convertScalarOneOfLiteralAxioms
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
    entry match {
      case (tbox: TerminologyBox, _) =>
        val ss = tbox.getBoxStatements.selectByKindOf { case s: ScalarOneOfLiteralAxiom => s }
        ss.foldLeft(state) { case (acc, li) =>
          for {
            prev <- acc
            o2ri = prev(tbox)
            tboxi = li.getTbox
            dri = li.getAxiom
            o2rj <-
              (prev.get(tboxi).flatMap(_.tboxes.get(tboxi)),
                prev.get(dri.getTbox).flatMap(_.dataRanges.get(dri))) match {
                case (Some(tboxj), Some(drj: api.ScalarOneOfRestriction)) =>
                  Option(li.getValueType) match {
                    case Some(vti) =>
                      prev.get(vti.getTbox).flatMap(_.dataRanges.get(vti)) match {
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
            next = prev.updated(tbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertEntityScalarDataProperties
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
    entry match {
      case (tbox: TerminologyBox, _) =>
        val ss = tbox.getBoxStatements.selectByKindOf { case s: EntityScalarDataProperty => s }
        ss.foldLeft(state) { case (acc, dpi) =>
          for {
            prev <- acc
            o2ri = prev(tbox)
            tboxi = dpi.getTbox
            dpdi = dpi.getDomain
            dpri = dpi.getRange
            o2rj <-
              (prev.get(tboxi).flatMap(_.tboxes.get(tboxi)),
                prev.get(dpdi.getTbox).flatMap(_.entityLookup(dpdi)),
                prev.get(dpri.getTbox).flatMap(_.dataRanges.get(dpri))) match {
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
            next = prev.updated(tbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertEntityStructuredDataProperties
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
    entry match {
      case (tbox: TerminologyBox, _) =>
        val ss = tbox.getBoxStatements.selectByKindOf { case s: EntityStructuredDataProperty => s }
        ss.foldLeft(state) { case (acc, dpi) =>
          for {
            prev <- acc
            o2ri = prev(tbox)
            tboxi = dpi.getTbox
            dpdi = dpi.getDomain
            dpri = dpi.getRange
            o2rj <-
              (prev.get(tboxi).flatMap(_.tboxes.get(tboxi)),
                prev.get(dpdi.getTbox).flatMap(_.entityLookup(dpdi)),
                prev.get(dpri.getTbox).flatMap(_.structures.get(dpri))) match {
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
            next = prev.updated(tbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertScalarDataProperties
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
    entry match {
      case (tbox: TerminologyBox, _) =>
        val ss = tbox.getBoxStatements.selectByKindOf { case s: ScalarDataProperty => s }
        ss.foldLeft(state) { case (acc, dpi) =>
          for {
            prev <- acc
            o2ri = prev(tbox)
            tboxi = dpi.getTbox
            dpdi = dpi.getDomain
            dpri = dpi.getRange
            o2rj <-
              (prev.get(tboxi).flatMap(_.tboxes.get(tboxi)),
                prev.get(dpdi.getTbox).flatMap(_.structures.get(dpdi)),
                prev.get(dpri.getTbox).flatMap(_.dataRanges.get(dpri))) match {
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
            next = prev.updated(tbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertStructuredDataProperties
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
    entry match {
      case (tbox: TerminologyBox, _) =>
        val ss = tbox.getBoxStatements.selectByKindOf { case s: StructuredDataProperty => s }
        ss.foldLeft(state) { case (acc, dpi) =>
          for {
            prev <- acc
            o2ri = prev(tbox)
            tboxi = dpi.getTbox
            dpdi = dpi.getDomain
            dpri = dpi.getRange
            o2rj <-
              (prev.get(tboxi).flatMap(_.tboxes.get(tboxi)),
                prev.get(dpdi.getTbox).flatMap(_.structures.get(dpdi)),
                prev.get(dpri.getTbox).flatMap(_.structures.get(dpri))) match {
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
            next = prev.updated(tbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertEntityRestrictionAxioms
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
    import EMFProblems.{ResourceAccessor,TermAxiomAccessor}
    entry match {
      case (tbox: TerminologyBox, _) =>
        val ss = tbox.getBoxStatements.selectByKindOf { case s: EntityRestrictionAxiom => s }
        ss.foldLeft(state) { case (acc, axi) =>
          for {
            prev <- acc
            o2ri = prev(tbox)
            tboxi = axi.getTbox
            rdi <- axi.accessFeature(_.getRestrictedDomain, "restrictedDomain")
            rri <- axi.accessFeature( _.getRestrictedRange, "restrictedRange")
            rli <- axi.accessFeature( _.getRestrictedRelationship, "restrictedRelationship")
            rel <- rli.accessFeature( _.relation, "relation")
            o2rj <-
              ( prev.get(tboxi).flatMap(_.tboxes.get(tboxi)),
                prev.get(rdi.getTbox).flatMap(_.entityLookup(rdi)),
                prev.get(rri.getTbox).flatMap(_.entityLookup(rri)),
                prev.get(rel.getTbox).flatMap { x =>
                  x.restrictableRelationshipLookup(rli)
                }) match {
                case (Some(tboxj), Some(rdj), Some(rrj), Some(rlj)) =>
                  val (rj, axj) = axi match {
                    case _: EntityExistentialRestrictionAxiom =>
                      f.createEntityExistentialRestrictionAxiom(o2ri.rextent, tboxj, rdj, rrj, rlj)
                    case _: EntityUniversalRestrictionAxiom =>
                      f.createEntityUniversalRestrictionAxiom(o2ri.rextent, tboxj, rdj, rrj, rlj)
                  }
                  o2ri.copy(rextent = rj, termAxioms = o2ri.termAxioms + (axi -> axj)).right
                case (tboxj, rdj, rrj, rlj) =>
                  new EMFProblems(new java.lang.IllegalArgumentException(
                    s"convertEntityRestrictionAxioms: Cannot resolve " +
                      tboxj.fold(s" tbox: ${tboxi.getIri}")(_ => "") +
                      rdj.fold(s" restriction domain: ${rdi.getName}")(_ => "") +
                      rrj.fold(s" restriction range: ${rdi.getName}")(_ => "") +
                      rlj.fold(s" restricted relation: ${rli.getClass.getName}: ${rli.relation().getName}")(_ => "")
                  )).left
              }
            next = prev.updated(tbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertEntityScalarDataPropertyRestrictionAxioms
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
    entry match {
      case (tbox: TerminologyBox, _) =>
        val ss = tbox.getBoxStatements.selectByKindOf { case s: EntityScalarDataPropertyRestrictionAxiom => s }
        ss.foldLeft(state) { case (acc, axi) =>
          for {
            prev <- acc
            o2ri = prev(tbox)
            tboxi = axi.getTbox
            ei = axi.getRestrictedEntity
            dpi = axi.getScalarProperty
            o2rj <-
              (prev.get(tboxi).flatMap(_.tboxes.get(tboxi)),
                prev.get(ei.getTbox).flatMap(_.entityLookup(ei)),
                prev.get(dpi.getTbox).flatMap(_.entityScalarDataProperties.get(dpi))) match {
                case (Some(tboxj), Some(ej), Some(dpj)) =>
                  val (rj, axj: \/[EMFProblems, api.EntityScalarDataPropertyRestrictionAxiom]) = axi match {
                    case axpi: EntityScalarDataPropertyParticularRestrictionAxiom =>
                      Option(axpi.getValueType) match {
                        case Some(vti) =>
                          prev.get(vti.getTbox).flatMap(_.dataRanges.get(vti)) match {
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
                      prev.get(dri.getTbox).flatMap(_.dataRanges.get(dri)) match {
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
                      prev.get(dri.getTbox).flatMap(_.dataRanges.get(dri)) match {
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
            next = prev.updated(tbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertEntityStructuredDataPropertyRestrictionAxioms
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
    entry match {
      case (tbox: TerminologyBox, _) =>
        val ss = tbox.getBoxStatements.selectByKindOf { case s: EntityStructuredDataPropertyParticularRestrictionAxiom => s }
        ss.foldLeft(state) { case (acc, axi) =>
          for {
            prev <- acc
            o2ri = prev(tbox)
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
              (prev.get(tboxi).flatMap(_.tboxes.get(tboxi)),
                prev.get(ei.getTbox).flatMap(_.entityLookup(ei)),
                prev.get(dptboxi).flatMap(_.dataRelationshipToStructureLookup(dpi))) match {
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
            next = prev.updated(tbox, o2rj)
          } yield next
        }
      case _ =>
        state
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
              f.createRestrictionScalarDataPropertyValue(o2r1.rextent, cj, dpj, lv, Some(vtj))
            o2r1.copy(rextent = rj).right

          case (Some(dpj), None, _) =>
            val (rj, _) =
              f.createRestrictionScalarDataPropertyValue(o2r1.rextent, cj, dpj, lv, None)
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
              f.createRestrictionStructuredDataPropertyTuple(o2r1.rextent, cj, dpj)
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
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
    entry match {
      case (tbox: TerminologyBox, _) =>
        val ss = tbox.getBoxStatements.selectByKindOf { case s: SpecializationAxiom => s }
        ss.foldLeft(state) { case (acc, axi) =>
          for {
            prev <- acc
            o2ri = prev(tbox)
            tboxi = axi.getTbox
            supi = axi.parent()
            subi = axi.child()
            (rj, axj: \/[EMFProblems, api.SpecializationAxiom]) =
            (axi,
              prev.get(tboxi).flatMap(_.tboxes.get(tboxi)),
              prev.get(supi.getTbox).flatMap(_.entityLookup(supi)),
              prev.get(subi.getTbox).flatMap(_.entityLookup(subi))) match {
              case (_: ConceptSpecializationAxiom, Some(tboxj), Some(supj: api.ConceptKind), Some(subj: api.ConceptKind)) =>
                f.createConceptSpecializationAxiom(o2ri.rextent, tboxj, supj, subj) match {
                  case (rk, ak) => rk -> ak.right
                }
              case (_: AspectSpecializationAxiom, Some(tboxj), Some(supj: api.AspectKind), Some(subj)) =>
                f.createAspectSpecializationAxiom(o2ri.rextent, tboxj, supj, subj) match {
                  case (rk, ak) => rk -> ak.right
                }
              case (_: ReifiedRelationshipSpecializationAxiom, Some(tboxj), Some(supj: api.ConceptualRelationship), Some(subj: api.ConceptualRelationship)) =>
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
            next = prev.updated(tbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertSubDataPropertyOfAxioms
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
    entry match {
      case (tbox: TerminologyBox, _) =>
        val ss = tbox.getBoxStatements.selectByKindOf { case s: SubDataPropertyOfAxiom => s }
        ss.foldLeft(state) { case (acc, axi) =>
          for {
            prev <- acc
            o2ri = prev(tbox)
            tboxi = axi.getTbox
            subi = axi.getSubProperty()
            supi = axi.getSuperProperty()
            (rj, axj: \/[EMFProblems, api.SubDataPropertyOfAxiom]) =
            (prev.get(tboxi).flatMap(_.tboxes.get(tboxi)),
              prev.get(subi.getTbox).flatMap(_.entityScalarDataProperties.get(subi)),
              prev.get(supi.getTbox).flatMap(_.entityScalarDataProperties.get(supi))) match {
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
            next = prev.updated(tbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertSubObjectPropertyOfAxioms
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
    entry match {
      case (tbox: TerminologyBox, _) =>
        val ss = tbox.getBoxStatements.selectByKindOf { case s: SubObjectPropertyOfAxiom => s }
        ss.foldLeft(state) { case (acc, axi) =>
          for {
            prev <- acc
            o2ri = prev(tbox)
            tboxi = axi.getTbox
            subi = axi.getSubProperty()
            supi = axi.getSuperProperty()
            (rj, axj: \/[EMFProblems, api.SubObjectPropertyOfAxiom]) =
            (prev.get(tboxi).flatMap(_.tboxes.get(tboxi)),
              prev.get(subi.getTbox).flatMap(_.unreifiedRelationships.get(subi)),
              prev.get(supi.getTbox).flatMap(_.unreifiedRelationships.get(supi))) match {
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
            next = prev.updated(tbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertRootConceptTaxonomyAxioms
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
    entry match {
      case (b: Bundle, _) =>
        val axs = b.getBundleStatements.selectByKindOf { case ax: RootConceptTaxonomyAxiom => ax }
        axs.foldLeft(state) { case (acc, axi) =>
          for {
            prev <- acc
            o2ri = prev(b)
            o2rj <- convertConceptTreeDisjunction(o2ri, axi)
            next = prev.updated(b, o2rj)
          } yield next
        }
      case _ =>
        state
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
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    entry match {
      case (dbox: DescriptionBox, _) =>
        val ss = dbox.getConceptInstances.asScala.to[Set]
        ss.foldLeft(state) { case (acc, di) =>
          for {
            prev <- acc
            o2ri = prev(dbox)
            dboxi = di.descriptionBox()
            cli = di.getSingletonConceptClassifier
            o2rj <-
              (prev.get(dboxi).flatMap(_.dboxes.get(dboxi)),
                prev.get(cli.getTbox).flatMap(_.concepts.get(cli))) match {
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
            next = prev.updated(dbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertReifiedRelationshipInstances
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    entry match {
      case (dbox: DescriptionBox, _) =>
        val ss = dbox.getReifiedRelationshipInstances.asScala.to[Set]
        ss.foldLeft(state) { case (acc, di) =>
          for {
            prev <- acc
            o2ri = prev(dbox)
            dboxi = di.descriptionBox()
            cli = di.getSingletonConceptualRelationshipClassifier
            o2rj <-
              (prev.get(dboxi).flatMap(_.dboxes.get(dboxi)),
                prev.get(cli.getTbox).flatMap(_.conceptualRelationshipLookup(cli))) match {
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
                      s"reified relationship: ${di.getSingletonConceptualRelationshipClassifier.abbrevIRI}"
                  )).left
              }
            next = prev.updated(dbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertReifiedRelationshipInstanceDomains
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    entry match {
      case (dbox: DescriptionBox, _) =>
        val ss = dbox.getReifiedRelationshipInstanceDomains.asScala.to[Set]
        ss.foldLeft(state) { case (acc, di) =>
          for {
            prev <- acc
            o2ri = prev(dbox)
            dboxi = di.descriptionBox()
            rrii = di.getReifiedRelationshipInstance
            rdi = di.getDomain
            o2rj <-
              (prev.get(dboxi).flatMap(_.dboxes.get(dboxi)),
                prev.get(rrii.descriptionBox).flatMap(_.conceptualEntitySingletonInstances.get(rrii)),
                prev.get(rdi.descriptionBox).flatMap(_.conceptualEntitySingletonInstances.get(rdi))) match {
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
            next = prev.updated(dbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertReifiedRelationshipInstanceRanges
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    entry match {
      case (dbox: DescriptionBox, _) =>
        val ss = dbox.getReifiedRelationshipInstanceRanges.asScala.to[Set]
        ss.foldLeft(state) { case (acc, di) =>
          for {
            prev <- acc
            o2ri = prev(dbox)
            dboxi = di.descriptionBox()
            rrii = di.getReifiedRelationshipInstance
            rri = di.getRange
            o2rj <-
              (prev.get(dboxi).flatMap(_.dboxes.get(dboxi)),
                prev.get(rrii.descriptionBox).flatMap(_.conceptualEntitySingletonInstances.get(rrii)),
                prev.get(rri.descriptionBox).flatMap(_.conceptualEntitySingletonInstances.get(rri))) match {
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
            next = prev.updated(dbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertUnreifiedRelationshipInstances
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    entry match {
      case (dbox: DescriptionBox, _) =>
        val ss = dbox.getUnreifiedRelationshipInstanceTuples.asScala.to[Set]
        ss.foldLeft(state) { case (acc, di) =>
          for {
            prev <- acc
            o2ri = prev(dbox)
            dboxi = di.descriptionBox()
            ui = di.getUnreifiedRelationship
            udi = di.getDomain
            uri = di.getRange
            o2rj <-
              (prev.get(dboxi).flatMap(_.dboxes.get(dboxi)),
                prev.get(ui.getTbox).flatMap(_.unreifiedRelationships.get(ui)),
                prev.get(udi.descriptionBox).flatMap(_.conceptualEntitySingletonInstances.get(udi)),
                prev.get(uri.descriptionBox).flatMap(_.conceptualEntitySingletonInstances.get(uri))) match {
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
            next = prev.updated(dbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertSingletonScalarDataPropertyValues
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    entry match {
      case (dbox: DescriptionBox, _) =>
        val ss = dbox.getSingletonScalarDataPropertyValues.asScala.to[Set]
        ss.foldLeft(state) { case (acc, di) =>
          for {
            prev <- acc
            o2ri = prev(dbox)
            dboxi = di.descriptionBox()
            si = di.getSingletonInstance
            dpi = di.getScalarDataProperty
            o2rj <-
              (prev.get(dboxi).flatMap(_.dboxes.get(dboxi)),
                prev.get(si.descriptionBox()).flatMap(_.conceptualEntitySingletonInstances.get(si)),
                prev.get(dpi.getTbox).flatMap(_.entityScalarDataProperties.get(dpi))) match {
                case (Some(dboxj), Some(sj), Some(dpj)) =>
                  Option(di.getValueType) match {
                    case Some(vti) =>
                      prev.get(vti.getTbox).flatMap(_.dataRanges.get(vti)) match {
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
                      Option(di.getValueType).fold {
                        ""
                      } { vti => s" value type: ${vti.abbrevIRI}" }
                  )).left
              }
            next = prev.updated(dbox, o2rj)
          } yield next
        }
      case _ =>
        state
    }
  }

  protected def convertSingletonStructuredDataPropertyValues
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    entry match {
      case (dbox: DescriptionBox, _) =>
        val ss = dbox.getSingletonStructuredDataPropertyValues.asScala.to[Set]
        ss.foldLeft(state) { case (acc, di) =>
          for {
            prev <- acc
            o2ri = prev(dbox)
            dboxi = di.descriptionBox()
            si = di.getSingletonInstance
            dpi = di.getStructuredDataProperty.asInstanceOf[EntityStructuredDataProperty]
            o2rj <-
              (prev.get(dboxi).flatMap(_.dboxes.get(dboxi)),
                prev.get(si.descriptionBox()).flatMap(_.conceptualEntitySingletonInstances.get(si)),
                prev.get(dpi.getTbox).flatMap(_.entityStructuredDataProperties.get(dpi))) match {
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
            next = prev.updated(dbox, o2rj)
          } yield next
        }
      case _ =>
        state
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
        val (rj, stj) = f.createStructuredDataPropertyTuple(o2r.rextent, ctx, stk)
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
              o2r.rextent, ctx, sck,
              emf2tables(sci.getScalarPropertyValue), vtj)
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
  (state: EMFProblems \/ Map[Module, OMLText2Resolver],
   entry: (Module, OMLText2Resolver))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    val (mi, _) = entry
    for {
      prev <- state
      allO2Rs = prev.values
      annotationProperties = allO2Rs.flatMap(_.aps).toMap
      o2ri = prev(mi)
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
          aValue = a0.getValue
          (el, _) = f.createAnnotationPropertyValue(
            o2rk.rextent,
            aSubject,
            aProperty,
            tables.taggedTypes.stringDataType(StringArray.decode2StringIfNeeded(a0.getValue.value)))
          o2rl = o2rk.copy(rextent = el)
        } yield o2rl
      }
      next = prev.updated(mi, o2rj)
    } yield next
  }

  protected def resolvableCardinalityAspectRestrictions
  (s: Map[Module, OMLText2Resolver])
  : Iterable[(TerminologyBox, Iterable[(CardinalityRestrictedAspect, api.RestrictableRelationship, Option[api.Entity])])]
  = s.flatMap {
    case (tbox: TerminologyBox, o2r) =>
      import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
      val tuples
      : Iterable[(CardinalityRestrictedAspect, api.RestrictableRelationship, Option[api.Entity])]
      = o2r
        .queue_elements
        .selectByKindOf { case x: CardinalityRestrictedAspect => x }
        .flatMap { x =>
          ( s.accessibleLookup(o2r, _.restrictableRelationshipLookup(x.getRestrictedRelationship)),
            Option.apply(x.getRestrictedRange),
            s.accessibleLookup(o2r, _.entityLookup(x.getRestrictedRange)) ) match {
            case (Some(rel), Some(_), Some(range)) =>
              Some((x, rel, Some(range)))
            case (Some(rel), None, _) =>
              Some((x, rel, None))
            case _ =>
              None
          }
        }
      if (tuples.nonEmpty)
        Some(tbox -> tuples)
      else
        None
    case _ =>
      None
  }

  protected def updateCardinalityAspectRestrictions
  (current: EMFProblems \/ Map[Module, OMLText2Resolver],
   x: (TerminologyBox, Iterable[(CardinalityRestrictedAspect, api.RestrictableRelationship, Option[api.Entity])]))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {

    def updateCardinalityAspectRestrictions1
    (t0: TerminologyBox)
    (current: EMFProblems \/ OMLText2Resolver,
     x: (CardinalityRestrictedAspect, api.RestrictableRelationship, Option[api.Entity]))
    : EMFProblems \/ OMLText2Resolver
    = for {
      o2r <- current
      t1 = o2r.tboxes(t0)
      (rj, y) = f.createCardinalityRestrictedAspect(
        extent = o2r.rextent,
        tbox = t1,
        restrictedRange = x._3,
        name = tables.taggedTypes.localName(x._1.getName),
        restrictedCardinality = tables.taggedTypes.positiveIntegerLiteral(x._1.getRestrictedCardinality.toString),
        restrictedRelationship = x._2,
        restrictionKind = x._1.getRestrictionKind)
      next = o2r.copy(
        rextent = rj,
        queue_elements = o2r.queue_elements - x._1,
        aspects = o2r.aspects + (x._1 -> y))
    } yield next

    for {
      s0 <- current
      prev = s0(x._1)
      next <- x._2.foldLeft[EMFProblems \/ OMLText2Resolver](\/-(prev))(updateCardinalityAspectRestrictions1(x._1))
      s1 = s0.updated(x._1, next)
    } yield s1
  }

  protected def resolvableCardinalityConceptRestrictions
  (s: Map[Module, OMLText2Resolver])
  : Iterable[(TerminologyBox, Iterable[(CardinalityRestrictedConcept, api.RestrictableRelationship, Option[api.Entity])])]
  = s.flatMap {
    case (tbox: TerminologyBox, o2r) =>
      import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
      val tuples
      : Iterable[(CardinalityRestrictedConcept, api.RestrictableRelationship, Option[api.Entity])]
      = o2r
        .queue_elements
        .selectByKindOf { case x: CardinalityRestrictedConcept => x }
        .flatMap { x =>
          ( s.accessibleLookup(o2r, _.restrictableRelationshipLookup(x.getRestrictedRelationship)),
            Option.apply(x.getRestrictedRange),
            s.accessibleLookup(o2r, _.entityLookup(x.getRestrictedRange)) ) match {
            case (Some(rel), Some(_), Some(range)) =>
              Some((x, rel, Some(range)))
            case (Some(rel), None, _) =>
              Some((x, rel, None))
            case _ =>
              None
          }
        }
      if (tuples.nonEmpty)
        Some(tbox -> tuples)
      else
        None
    case _ =>
      None
  }

  protected def updateCardinalityConceptRestrictions
  (current: EMFProblems \/ Map[Module, OMLText2Resolver],
   x: (TerminologyBox, Iterable[(CardinalityRestrictedConcept, api.RestrictableRelationship, Option[api.Entity])]))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {

    def updateCardinalityConceptRestrictions1
    (t0: TerminologyBox)
    (current: EMFProblems \/ OMLText2Resolver,
     x: (CardinalityRestrictedConcept, api.RestrictableRelationship, Option[api.Entity]))
    : EMFProblems \/ OMLText2Resolver
    = for {
      o2r <- current
      t1 = o2r.tboxes(t0)
      (rj, y) = f.createCardinalityRestrictedConcept(
        extent = o2r.rextent,
        tbox = t1,
        restrictedRange = x._3,
        name = tables.taggedTypes.localName(x._1.getName),
        restrictedCardinality = tables.taggedTypes.positiveIntegerLiteral(x._1.getRestrictedCardinality.toString),
        restrictedRelationship = x._2,
        restrictionKind = x._1.getRestrictionKind)
      next = o2r.copy(
        rextent = rj,
        queue_elements = o2r.queue_elements - x._1,
        concepts = o2r.concepts + (x._1 -> y))
    } yield next

    for {
      s0 <- current
      prev = s0(x._1)
      next <- x._2.foldLeft[EMFProblems \/ OMLText2Resolver](\/-(prev))(updateCardinalityConceptRestrictions1(x._1))
      s1 = s0.updated(x._1, next)
    } yield s1
  }

  protected def resolvableCardinalityReifiedRelationshipRestrictions
  (s: Map[Module, OMLText2Resolver])
  : Iterable[(TerminologyBox, Iterable[(CardinalityRestrictedReifiedRelationship, api.RestrictableRelationship, Option[api.Entity])])]
  = s.flatMap {
    case (tbox: TerminologyBox, o2r) =>
      import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
      val tuples
      : Iterable[(CardinalityRestrictedReifiedRelationship, api.RestrictableRelationship, Option[api.Entity])]
      = o2r
        .queue_elements
        .selectByKindOf { case x: CardinalityRestrictedReifiedRelationship => x }
        .flatMap { x =>
          ( s.accessibleLookup(o2r, _.restrictableRelationshipLookup(x.getRestrictedRelationship)),
            Option.apply(x.getRestrictedRange),
            s.accessibleLookup(o2r, _.entityLookup(x.getRestrictedRange)) ) match {
            case (Some(rel), Some(_), Some(range)) =>
              Some((x, rel, Some(range)))
            case (Some(rel), None, _) =>
              Some((x, rel, None))
            case _ =>
              None
          }
        }
      if (tuples.nonEmpty)
        Some(tbox -> tuples)
      else
        None
    case _ =>
      None
  }

  protected def updateCardinalityReifiedRelationshipRestrictions
  (current: EMFProblems \/ Map[Module, OMLText2Resolver],
   x: (TerminologyBox, Iterable[(CardinalityRestrictedReifiedRelationship, api.RestrictableRelationship, Option[api.Entity])]))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    def updateCardinalityReifiedRelationshipRestrictions1
    (t0: TerminologyBox)
    (current: EMFProblems \/ OMLText2Resolver,
     x: (CardinalityRestrictedReifiedRelationship, api.RestrictableRelationship, Option[api.Entity]))
    : EMFProblems \/ OMLText2Resolver
    = for {
      o2r <- current
      t1 = o2r.tboxes(t0)
      (rj, y) = f.createCardinalityRestrictedReifiedRelationship(
        extent = o2r.rextent,
        tbox = t1,
        restrictedRange = x._3,
        name = tables.taggedTypes.localName(x._1.getName),
        restrictedCardinality = tables.taggedTypes.positiveIntegerLiteral(x._1.getRestrictedCardinality.toString),
        restrictedRelationship = x._2,
        restrictionKind = x._1.getRestrictionKind)
      next = o2r.copy(
        rextent = rj,
        queue_elements = o2r.queue_elements - x._1,
        cardinalityRestrictedReifiedRelationships = o2r.cardinalityRestrictedReifiedRelationships + (x._1 -> y))
    } yield next

    for {
      s0 <- current
      prev = s0(x._1)
      next <- x._2.foldLeft[EMFProblems \/ OMLText2Resolver](\/-(prev))(updateCardinalityReifiedRelationshipRestrictions1(x._1))
      s1 = s0.updated(x._1, next)
    } yield s1
  }

  protected def resolvableConceptDesignationTerminologyAxioms
  (s: Map[Module, OMLText2Resolver])
  : Iterable[(TerminologyBox, Iterable[(ConceptDesignationTerminologyAxiom, api.TerminologyBox, api.TerminologyBox, api.ConceptKind)])]
  = s.flatMap {
    case (tbox: TerminologyBox, o2r) =>
      import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
      val tuples
      : Iterable[(ConceptDesignationTerminologyAxiom, api.TerminologyBox, api.TerminologyBox, api.ConceptKind)]
      = o2r
          .queue_elements
          .selectByKindOf { case x: ConceptDesignationTerminologyAxiom => x }
          .flatMap { x =>
            ( s.lookupTerminologyBox(x.designationTerminologyGraph),
              s.lookupTerminologyBox(x.getDesignatedTerminology),
              s.lookupConceptKind(Option.apply(x.getDesignatedConcept)) ) match {
              case (Some(designationG), Some(designatedT), Some(designatedC)) =>
                Some((x, designationG, designatedT, designatedC))
              case _ =>
                None
            }
          }

      if (tuples.nonEmpty)
        Some(tbox -> tuples)
      else
        None
    case _ =>
      None
  }

  protected def updateConceptDesignationTerminologyAxioms
  (current: EMFProblems \/ Map[Module, OMLText2Resolver],
   x: (TerminologyBox, Iterable[(ConceptDesignationTerminologyAxiom, api.TerminologyBox, api.TerminologyBox, api.ConceptKind)]))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    def updateConceptDesignationTerminologyAxioms1
    (t0: TerminologyBox)
    (current: EMFProblems \/ OMLText2Resolver,
     x: (ConceptDesignationTerminologyAxiom, api.TerminologyBox, api.TerminologyBox, api.ConceptKind))
    : EMFProblems \/ OMLText2Resolver
    = for {
      o2r <- current
      t1 = o2r.tboxes(t0)
      (rj, y) = f.createConceptDesignationTerminologyAxiom(
        extent = o2r.rextent,
        tbox = x._2,
        designatedConcept = x._4,
        designatedTerminology = x._3.iri)
      next = o2r.copy(
        rextent = rj,
        queue_edges = o2r.queue_edges - x._1,
        moduleEdges = o2r.moduleEdges + (x._1 -> y))
    } yield next

    for {
      s0 <- current
      prev = s0(x._1)
      next <- x._2.foldLeft[EMFProblems \/ OMLText2Resolver](\/-(prev))(updateConceptDesignationTerminologyAxioms1(x._1))
      s1 = s0.updated(x._1, next)
    } yield s1
  }

  protected def resolvableTerminologyNestingAxioms
  (s: Map[Module, OMLText2Resolver])
  : Iterable[(TerminologyBox, Iterable[(TerminologyNestingAxiom, api.TerminologyGraph, api.TerminologyBox, api.ConceptKind)])]
  = s.flatMap {
    case (tbox: TerminologyBox, o2r) =>
      import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
      val tuples
      : Iterable[(TerminologyNestingAxiom, api.TerminologyGraph, api.TerminologyBox, api.ConceptKind)]
      = o2r
        .queue_elements
        .selectByKindOf { case x: TerminologyNestingAxiom => x }
        .flatMap { x =>
          ( s.lookupTerminologyGraph(x.nestedTerminology),
            s.lookupTerminologyBox(x.getNestingTerminology),
            s.lookupConceptKind(Option.apply(x.getNestingContext)) ) match {
            case (Some(nestedG), Some(nestingT), Some(nestingC)) =>
              Some((x, nestedG, nestingT, nestingC))
            case _ =>
              None
          }
        }

      if (tuples.nonEmpty)
        Some(tbox -> tuples)
      else
        None
    case _ =>
      None
  }

  protected def updateTerminologyNestingAxioms
  (current: EMFProblems \/ Map[Module, OMLText2Resolver],
   x: (TerminologyBox, Iterable[(TerminologyNestingAxiom, api.TerminologyGraph, api.TerminologyBox, api.ConceptKind)]))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    def updateTerminologyNestingAxioms1
    (t0: TerminologyBox)
    (current: EMFProblems \/ OMLText2Resolver,
     x: (TerminologyNestingAxiom, api.TerminologyGraph, api.TerminologyBox, api.ConceptKind))
    : EMFProblems \/ OMLText2Resolver
    = for {
      o2r <- current
      t1 = o2r.tboxes(t0)
      _ <- if (t1 == x._2)
      ().right[EMFProblems]
      else
        new EMFProblems(new java.lang.IllegalArgumentException(
        s"updateTerminologyNestingAxioms1: t1=$t1 != x._2=${x._2}"
      )).left
      (rj, y) = f.createTerminologyNestingAxiom(
        extent = o2r.rextent,
        tbox = x._2,
        nestingContext = x._4,
        nestingTerminology = x._3.iri)
      next = o2r.copy(
        rextent = rj,
        queue_edges = o2r.queue_edges - x._1,
        moduleEdges = o2r.moduleEdges + (x._1 -> y))
    } yield next

    for {
      s0 <- current
      prev = s0(x._1)
      next <- x._2.foldLeft[EMFProblems \/ OMLText2Resolver](\/-(prev))(updateTerminologyNestingAxioms1(x._1))
      s1 = s0.updated(x._1, next)
    } yield s1
  }

  protected def resolvableBundledTerminologyAxioms
  (s: Map[Module, OMLText2Resolver])
  : Iterable[(TerminologyBox, Iterable[(BundledTerminologyAxiom, api.Bundle, api.TerminologyBox)])]
  = s.flatMap {
    case (tbox: TerminologyBox, o2r) =>
      import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
      val tuples
      : Iterable[(BundledTerminologyAxiom, api.Bundle, api.TerminologyBox)]
      = o2r
        .queue_elements
        .selectByKindOf { case x: BundledTerminologyAxiom => x }
        .flatMap { x =>
          ( s.lookupBundle(x.getBundle),
            s.lookupTerminologyBox(x.getBundledTerminology) ) match {
            case (Some(bundle), Some(bundled)) =>
              Some((x, bundle, bundled))
            case _ =>
              None
          }
        }

      if (tuples.nonEmpty)
        Some(tbox -> tuples)
      else
        None
    case _ =>
      None
  }

  protected def updateBundledTerminologyAxioms
  (current: EMFProblems \/ Map[Module, OMLText2Resolver],
   x: (TerminologyBox, Iterable[(BundledTerminologyAxiom, api.Bundle, api.TerminologyBox)]))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    def updateBundledTerminologyAxioms1
    (t0: TerminologyBox)
    (current: EMFProblems \/ OMLText2Resolver,
     x: (BundledTerminologyAxiom, api.Bundle, api.TerminologyBox))
    : EMFProblems \/ OMLText2Resolver
    = for {
      o2r <- current
      t1 = o2r.tboxes(t0)
      (rj, y) = f.createBundledTerminologyAxiom(
        extent = o2r.rextent,
        bundle = x._2,
        bundledTerminology = x._3.iri)
      next = o2r.copy(
        rextent = rj,
        queue_edges = o2r.queue_edges - x._1,
        moduleEdges = o2r.moduleEdges + (x._1 -> y))
    } yield next

    for {
      s0 <- current
      prev = s0(x._1)
      next <- x._2.foldLeft[EMFProblems \/ OMLText2Resolver](\/-(prev))(updateBundledTerminologyAxioms1(x._1))
      s1 = s0.updated(x._1, next)
    } yield s1
  }

  protected def resolvableDescriptionBoxExtendsClosedWorldDefinitions
  (s: Map[Module, OMLText2Resolver])
  : Iterable[(TerminologyBox, Iterable[(DescriptionBoxExtendsClosedWorldDefinitions, api.DescriptionBox, api.TerminologyBox)])]
  = s.flatMap {
    case (tbox: TerminologyBox, o2r) =>
      import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
      val tuples
      : Iterable[(DescriptionBoxExtendsClosedWorldDefinitions, api.DescriptionBox, api.TerminologyBox)]
      = o2r
        .queue_elements
        .selectByKindOf { case x: DescriptionBoxExtendsClosedWorldDefinitions => x }
        .flatMap { x =>
          ( s.lookupDescriptionBox(x.getDescriptionBox),
            s.lookupTerminologyBox(x.getClosedWorldDefinitions) ) match {
            case (Some(dbox), Some(definitions)) =>
              Some((x, dbox, definitions))
            case _ =>
              None
          }
        }

      if (tuples.nonEmpty)
        Some(tbox -> tuples)
      else
        None
    case _ =>
      None
  }

  protected def updateDescriptionBoxExtendsClosedWorldDefinitions
  (current: EMFProblems \/ Map[Module, OMLText2Resolver],
   x: (TerminologyBox, Iterable[(DescriptionBoxExtendsClosedWorldDefinitions, api.DescriptionBox, api.TerminologyBox)]))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    def updateDescriptionBoxExtendsClosedWorldDefinitions1
    (t0: TerminologyBox)
    (current: EMFProblems \/ OMLText2Resolver,
     x: (DescriptionBoxExtendsClosedWorldDefinitions, api.DescriptionBox, api.TerminologyBox))
    : EMFProblems \/ OMLText2Resolver
    = for {
      o2r <- current
      t1 = o2r.tboxes(t0)
      (rj, y) = f.createDescriptionBoxExtendsClosedWorldDefinitions(
        extent = o2r.rextent,
        descriptionBox = x._2,
        closedWorldDefinitions = x._3.iri)
      next = o2r.copy(
        rextent = rj,
        queue_edges = o2r.queue_edges - x._1,
        moduleEdges = o2r.moduleEdges + (x._1 -> y))
    } yield next

    for {
      s0 <- current
      prev = s0(x._1)
      next <- x._2.foldLeft[EMFProblems \/ OMLText2Resolver](\/-(prev))(updateDescriptionBoxExtendsClosedWorldDefinitions1(x._1))
      s1 = s0.updated(x._1, next)
    } yield s1
  }

  protected def resolvableDescriptionBoxRefinements
  (s: Map[Module, OMLText2Resolver])
  : Iterable[(TerminologyBox, Iterable[(DescriptionBoxRefinement, api.DescriptionBox, api.DescriptionBox)])]
  = s.flatMap {
    case (tbox: TerminologyBox, o2r) =>
      import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
      val tuples
      : Iterable[(DescriptionBoxRefinement, api.DescriptionBox, api.DescriptionBox)]
      = o2r
        .queue_elements
        .selectByKindOf { case x: DescriptionBoxRefinement => x }
        .flatMap { x =>
          ( s.lookupDescriptionBox(x.getRefiningDescriptionBox),
            s.lookupDescriptionBox(x.getRefinedDescriptionBox) ) match {
            case (Some(refining), Some(refined)) =>
              Some((x, refining, refined))
            case _ =>
              None
          }
        }

      if (tuples.nonEmpty)
        Some(tbox -> tuples)
      else
        None
    case _ =>
      None
  }

  protected def updateDescriptionBoxRefinements
  (current: EMFProblems \/ Map[Module, OMLText2Resolver],
   x: (TerminologyBox, Iterable[(DescriptionBoxRefinement, api.DescriptionBox, api.DescriptionBox)]))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    def updateDescriptionBoxRefinements1
    (t0: TerminologyBox)
    (current: EMFProblems \/ OMLText2Resolver,
     x: (DescriptionBoxRefinement, api.DescriptionBox, api.DescriptionBox))
    : EMFProblems \/ OMLText2Resolver
    = for {
      o2r <- current
      t1 = o2r.tboxes(t0)
      (rj, y) = f.createDescriptionBoxRefinement(
        extent = o2r.rextent,
        refiningDescriptionBox = x._2,
        refinedDescriptionBox = x._3.iri)
      next = o2r.copy(
        rextent = rj,
        queue_edges = o2r.queue_edges - x._1,
        moduleEdges = o2r.moduleEdges + (x._1 -> y))
    } yield next

    for {
      s0 <- current
      prev = s0(x._1)
      next <- x._2.foldLeft[EMFProblems \/ OMLText2Resolver](\/-(prev))(updateDescriptionBoxRefinements1(x._1))
      s1 = s0.updated(x._1, next)
    } yield s1
  }

  protected def resolvableReifiedRelationships
  (s: Map[Module, OMLText2Resolver])
  : Iterable[(TerminologyBox, Iterable[(ReifiedRelationship, api.Entity, ForwardProperty, Option[InverseProperty], api.Entity)])]
  = s.flatMap {
    case (tbox: TerminologyBox, o2r) =>
      import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
      val tuples
      : Iterable[(ReifiedRelationship, api.Entity, ForwardProperty, Option[InverseProperty], api.Entity)]
      = o2r
        .queue_elements
        .selectByKindOf { case x: ReifiedRelationship => x }
        .flatMap { x =>
          ( s.accessibleLookup(o2r, _.entityLookup(x.getSource)),
            Option.apply(x.getForwardProperty),
            Option.apply(x.getInverseProperty),
            s.accessibleLookup(o2r, _.entityLookup(x.getTarget)) ) match {
            case (Some(domain), Some(forward), Some(inverse), Some(range)) =>
              Some((x, domain, forward, Some(inverse), range))
            case (Some(domain), Some(forward), None, Some(range)) =>
              Some((x, domain, forward, None, range))
            case _ =>
              None
          }
        }
      if (tuples.nonEmpty)
        Some(tbox -> tuples)
      else
        None
    case _ =>
      None
  }

  protected def updateReifiedRelationships
  (current: EMFProblems \/ Map[Module, OMLText2Resolver],
   x: (TerminologyBox, Iterable[(ReifiedRelationship, api.Entity, ForwardProperty, Option[InverseProperty], api.Entity)]))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {

    def updateReifiedRelationships1
    (t0: TerminologyBox)
    (current: EMFProblems \/ OMLText2Resolver,
     x: (ReifiedRelationship, api.Entity, ForwardProperty, Option[InverseProperty], api.Entity))
    : EMFProblems \/ OMLText2Resolver
    = for {
      o2r <- current
      t1 = o2r.tboxes(t0)
      (rj1, y) = f.createReifiedRelationship(
        extent = o2r.rextent,
        tbox = t1,
        source = x._2,
        target = x._5,
        isAsymmetric = x._1.isIsAsymmetric,
        isEssential = x._1.isIsEssential,
        isFunctional = x._1.isIsFunctional,
        isInverseEssential = x._1.isIsInverseEssential,
        isInverseFunctional = x._1.isIsInverseFunctional,
        isIrreflexive = x._1.isIsIrreflexive,
        isReflexive = x._1.isIsReflexive,
        isSymmetric = x._1.isIsSymmetric,
        isTransitive = x._1.isIsTransitive,
        name = tables.taggedTypes.localName(x._1.getName))
      (rj2, fwd) = f.createForwardProperty(
        extent = rj1,
        name = tables.taggedTypes.localName(x._3.getName),
        reifiedRelationship = y)
      (rj3, inv) = x._4 match {
        case Some(_inv) =>
          val (rj4, i4) = f.createInverseProperty(
            extent = rj2,
            name = tables.taggedTypes.localName(_inv.getName),
            reifiedRelationship = y)
          rj4 -> Some(_inv -> i4)
        case None =>
          rj2 -> None
      }
      next = o2r.copy(
        rextent = rj3,
        queue_elements = o2r.queue_elements - x._1,
        reifiedRelationships = o2r.reifiedRelationships + (x._1 -> y),
        forwardProperties = o2r.forwardProperties + (x._3 -> fwd),
        inverseProperties = o2r.inverseProperties ++ inv)
    } yield next

    for {
      s0 <- current
      prev = s0(x._1)
      next <- x._2.foldLeft[EMFProblems \/ OMLText2Resolver](\/-(prev))(updateReifiedRelationships1(x._1))
      s1 = s0.updated(x._1, next)
    } yield s1
  }

  protected def resolvableUnreifiedRelationships
  (s: Map[Module, OMLText2Resolver])
  : Iterable[(TerminologyBox, Iterable[(UnreifiedRelationship, api.Entity, api.Entity)])]
  = s.flatMap {
    case (tbox: TerminologyBox, o2r) =>
      import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
      val tuples
      : Iterable[(UnreifiedRelationship, api.Entity, api.Entity)]
      = o2r
        .queue_elements
        .selectByKindOf { case x: UnreifiedRelationship => x }
        .flatMap { x =>
          ( s.accessibleLookup(o2r, _.entityLookup(x.getSource)),
            s.accessibleLookup(o2r, _.entityLookup(x.getTarget)) ) match {
            case (Some(domain), Some(range)) =>
              Some((x, domain, range))
            case _ =>
              None
          }
        }
      if (tuples.nonEmpty)
        Some(tbox -> tuples)
      else
        None
    case _ =>
      None
  }

  protected def updateUnreifiedRelationships
  (current: EMFProblems \/ Map[Module, OMLText2Resolver],
   x: (TerminologyBox, Iterable[(UnreifiedRelationship, api.Entity, api.Entity)]))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {

    def updateUnreifiedRelationships1
    (t0: TerminologyBox)
    (current: EMFProblems \/ OMLText2Resolver,
     x: (UnreifiedRelationship, api.Entity, api.Entity))
    : EMFProblems \/ OMLText2Resolver
    = for {
      o2r <- current
      t1 = o2r.tboxes(t0)
      (rj, y) = f.createUnreifiedRelationship(
        extent = o2r.rextent,
        tbox = t1,
        source = x._2,
        target = x._3,
        isAsymmetric = x._1.isIsAsymmetric,
        isEssential = x._1.isIsEssential,
        isFunctional = x._1.isIsFunctional,
        isInverseEssential = x._1.isIsInverseEssential,
        isInverseFunctional = x._1.isIsInverseFunctional,
        isIrreflexive = x._1.isIsIrreflexive,
        isReflexive = x._1.isIsReflexive,
        isSymmetric = x._1.isIsSymmetric,
        isTransitive = x._1.isIsTransitive,
        name = tables.taggedTypes.localName(x._1.getName))
      next = o2r.copy(
        rextent = rj,
        queue_elements = o2r.queue_elements - x._1,
        unreifiedRelationships = o2r.unreifiedRelationships + (x._1 -> y))
    } yield next

    for {
      s0 <- current
      prev = s0(x._1)
      next <- x._2.foldLeft[EMFProblems \/ OMLText2Resolver](\/-(prev))(updateUnreifiedRelationships1(x._1))
      s1 = s0.updated(x._1, next)
    } yield s1
  }

  protected def resolvableReifiedRelationshipRestrictions
  (s: Map[Module, OMLText2Resolver])
  : Iterable[(TerminologyBox, Iterable[(ReifiedRelationshipRestriction, api.Entity, api.Entity)])]
  = s.flatMap {
    case (tbox: TerminologyBox, o2r) =>
      import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._
      val tuples
      : Iterable[(ReifiedRelationshipRestriction, api.Entity, api.Entity)]
      = o2r
        .queue_elements
        .selectByKindOf { case x: ReifiedRelationshipRestriction => x }
        .flatMap { x =>
          ( s.accessibleLookup(o2r, _.entityLookup(x.getSource)),
            s.accessibleLookup(o2r, _.entityLookup(x.getTarget)) ) match {
            case (Some(domain), Some(range)) =>
              Some((x, domain, range))
            case _ =>
              None
          }
        }
      if (tuples.nonEmpty)
        Some(tbox -> tuples)
      else
        None
    case _ =>
      None
  }

  protected def updateReifiedRelationshipRestrictions
  (current: EMFProblems \/ Map[Module, OMLText2Resolver],
   x: (TerminologyBox, Iterable[(ReifiedRelationshipRestriction, api.Entity, api.Entity)]))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {

    def updateReifiedRelationshipRestrictions1
    (t0: TerminologyBox)
    (current: EMFProblems \/ OMLText2Resolver,
     x: (ReifiedRelationshipRestriction, api.Entity, api.Entity))
    : EMFProblems \/ OMLText2Resolver
    = for {
      o2r <- current
      t1 = o2r.tboxes(t0)
      (rj, y) = f.createReifiedRelationshipRestriction(
        extent = o2r.rextent,
        tbox = t1,
        source = x._2,
        target = x._3,
        name = tables.taggedTypes.localName(x._1.getName))
      next = o2r.copy(
        rextent = rj,
        queue_elements = o2r.queue_elements - x._1,
        reifiedRelationshipRestrictions = o2r.reifiedRelationshipRestrictions + (x._1 -> y))
    } yield next

    for {
      s0 <- current
      prev = s0(x._1)
      next <- x._2.foldLeft[EMFProblems \/ OMLText2Resolver](\/-(prev))(updateReifiedRelationshipRestrictions1(x._1))
      s1 = s0.updated(x._1, next)
    } yield s1
  }

  case class SegmentPredicateInfo
  (ruleBodySegment: RuleBodySegment,
   segmentPredicate: SegmentPredicate,
   predicate: Option[api.Predicate] = None,
   reifiedRelationshipSource: Option[api.ReifiedRelationship] = None,
   reifiedRelationshipInverseSource: Option[api.ReifiedRelationship ]= None,
   reifiedRelationshipTarget: Option[api.ReifiedRelationship] = None,
   reifiedRelationshipInverseTarget: Option[api.ReifiedRelationship] = None,
   unreifiedRelationshipInverse: Option[api.UnreifiedRelationship] = None)

  protected def resolvableChainRules
  (s: Map[Module, OMLText2Resolver])
  : Iterable[(TerminologyBox, Iterable[(ChainRule, api.UnreifiedRelationship, Seq[SegmentPredicateInfo])])]
  = s.flatMap {
    case (tbox: TerminologyBox, o2r) =>
      import gov.nasa.jpl.imce.oml.converters.utils.EMFFilterable._

      @scala.annotation.tailrec
      def resolveSegmentPredicates
      (seg1: RuleBodySegment,
       pred1: SegmentPredicate,
       acc: Seq[SegmentPredicateInfo] = Seq.empty[SegmentPredicateInfo])
      : Seq[SegmentPredicateInfo]
      = {
        val p
        = ( Option.apply(pred1.getPredicate),
            Option.apply(pred1.getReifiedRelationshipSource),
            Option.apply(pred1.getReifiedRelationshipInverseSource),
            Option.apply(pred1.getReifiedRelationshipTarget),
            Option.apply(pred1.getReifiedRelationshipInverseTarget),
            Option.apply(pred1.getUnreifiedRelationshipInverse) ) match {
          case (Some(x), None, None, None, None, None) =>
            x match {
              case a0: AspectKind =>
                s.accessibleLookup(o2r, _.aspects.get(a0)).map { a1 =>
                  SegmentPredicateInfo(seg1, pred1, predicate = Some(a1))
                }
              case c0: ConceptKind =>
                s.accessibleLookup(o2r, _.concepts.get(c0)).map { c1 =>
                  SegmentPredicateInfo(seg1, pred1, predicate = Some(c1))
                }
              case rr0: ReifiedRelationshipRestriction =>
                s.accessibleLookup(o2r, _.reifiedRelationshipRestrictions.get(rr0)).map { rr1 =>
                  SegmentPredicateInfo(seg1, pred1, predicate = Some(rr1))
                }
              case rr0: ReifiedRelationship =>
                s.accessibleLookup(o2r, _.reifiedRelationships.get(rr0)).map { rr1 =>
                  SegmentPredicateInfo(seg1, pred1, predicate = Some(rr1))
                }
              case ur0: UnreifiedRelationship =>
                s.accessibleLookup(o2r, _.unreifiedRelationships.get(ur0)).map { ur1 =>
                  SegmentPredicateInfo(seg1, pred1, predicate = Some(ur1))
                }
              case f0: ForwardProperty =>
                s.accessibleLookup(o2r, _.forwardProperties.get(f0)).map { f1 =>
                  SegmentPredicateInfo(seg1, pred1, predicate = Some(f1))
                }
              case i0: InverseProperty =>
                s.accessibleLookup(o2r, _.inverseProperties.get(i0)).map { i1 =>
                  SegmentPredicateInfo(seg1, pred1, predicate = Some(i1))
                }
            }

          case (None, Some(rr0), None, None, None, None) =>
            s.accessibleLookup(o2r, _.reifiedRelationships.get(rr0)).map { rr1 =>
              SegmentPredicateInfo(seg1, pred1, reifiedRelationshipSource = Some(rr1))
            }

          case (None, None, Some(rr0), None, None, None) =>
            s.accessibleLookup(o2r, _.reifiedRelationships.get(rr0)).map { rr1 =>
              SegmentPredicateInfo(seg1, pred1, reifiedRelationshipInverseSource = Some(rr1))
            }

          case (None, None, None, Some(rr0), None, None) =>
            s.accessibleLookup(o2r, _.reifiedRelationships.get(rr0)).map { rr1 =>
              SegmentPredicateInfo(seg1, pred1, reifiedRelationshipTarget = Some(rr1))
            }

          case (None, None, None, None, Some(rr0), None) =>
            s.accessibleLookup(o2r, _.reifiedRelationships.get(rr0)).map { rr1 =>
              SegmentPredicateInfo(seg1, pred1, reifiedRelationshipInverseTarget = Some(rr1))
            }

          case (None, None, None, None, None, Some(ur0)) =>
            s.accessibleLookup(o2r, _.unreifiedRelationships.get(ur0)).map { ur1 =>
              SegmentPredicateInfo(seg1, pred1, predicate = Some(ur1))
            }

          case (_, _, _, _, _, _) =>
            None
        }

        (p, Option.apply(seg1.getNextSegment)) match {
          case (Some(p1), Some(seg2)) =>
            Option.apply(seg2.getPredicate) match {
              case Some(pred2) =>
                resolveSegmentPredicates(seg2, pred2, acc :+ p1)
              case _ =>
                Seq.empty[SegmentPredicateInfo]
            }
          case _ =>
            Seq.empty[SegmentPredicateInfo]

        }
      }

      val tuples
      : Iterable[(ChainRule, api.UnreifiedRelationship, Seq[SegmentPredicateInfo])]
      = o2r
        .queue_elements
        .selectByKindOf { case x: ChainRule => x }
        .flatMap { x =>
          o2r.unreifiedRelationships.get(x.getHead).flatMap { head =>
            val segments = Option.apply(x.getFirstSegment) match {
              case Some(seg) =>
                Option.apply(seg.getPredicate) match {
                  case Some(pred) =>
                    resolveSegmentPredicates(seg, pred)
                  case None =>
                    Seq.empty[SegmentPredicateInfo]
                }
              case None =>
                Seq.empty[SegmentPredicateInfo]
            }
            if (segments.nonEmpty)
              Some((x, head, segments))
            else
              None
          }
        }
      if (tuples.nonEmpty)
        Some(tbox -> tuples)
      else
        None
    case _ =>
      None
  }

  def updateChainRules
  (current: EMFProblems \/ Map[Module, OMLText2Resolver],
   x: (TerminologyBox, Iterable[(ChainRule, api.UnreifiedRelationship, Seq[SegmentPredicateInfo])]))
  (implicit f: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = {
    @scala.annotation.tailrec
    def updateSegmentPredicates
    (o2r: OMLText2Resolver,
     chainRule: Option[api.ChainRule],
     previous: Option[api.RuleBodySegment],
     preds: Seq[SegmentPredicateInfo])
    : EMFProblems \/ OMLText2Resolver
    = if (preds.nonEmpty) {
      val (e1, bs) = f.createRuleBodySegment(extent = o2r.rextent, rule = chainRule, previousSegment = previous)
      val i = preds.head
        i.predicate match {
          case Some(p) =>
            val (e2, sp) = f.createSegmentPredicate(extent = e1, bodySegment = bs,
              predicate = Some(p),
              reifiedRelationshipSource = None,
              reifiedRelationshipInverseSource = None,
              reifiedRelationshipTarget = None,
              reifiedRelationshipInverseTarget = None,
              unreifiedRelationshipInverse = None)
            val next = o2r.copy(
              rextent = e2,
              ruleBodySegments = o2r.ruleBodySegments + (i.ruleBodySegment -> bs),
              segmentPredicates = o2r.segmentPredicates + (i.segmentPredicate -> sp))
            updateSegmentPredicates(next, chainRule = None, previous = Some(bs), preds.tail)
          case _ =>
            i.reifiedRelationshipSource match {
              case Some(rr) =>
                val (e2, sp) = f.createSegmentPredicate(extent = e1, bodySegment = bs,
                  predicate = None,
                  reifiedRelationshipSource = Some(rr),
                  reifiedRelationshipInverseSource = None,
                  reifiedRelationshipTarget = None,
                  reifiedRelationshipInverseTarget = None,
                  unreifiedRelationshipInverse = None)
                val next = o2r.copy(
                  rextent = e2,
                  ruleBodySegments = o2r.ruleBodySegments + (i.ruleBodySegment -> bs),
                  segmentPredicates = o2r.segmentPredicates + (i.segmentPredicate -> sp))
                updateSegmentPredicates(next, chainRule = None, previous = Some(bs), preds.tail)
              case _ =>
                i.reifiedRelationshipInverseSource match {
                  case Some(rr) =>
                    val (e2, sp) = f.createSegmentPredicate(extent = e1, bodySegment = bs,
                      predicate = None,
                      reifiedRelationshipSource = None,
                      reifiedRelationshipInverseSource = Some(rr),
                      reifiedRelationshipTarget = None,
                      reifiedRelationshipInverseTarget = None,
                      unreifiedRelationshipInverse = None)
                    val next = o2r.copy(
                      rextent = e2,
                      ruleBodySegments = o2r.ruleBodySegments + (i.ruleBodySegment -> bs),
                      segmentPredicates = o2r.segmentPredicates + (i.segmentPredicate -> sp))
                    updateSegmentPredicates(next, chainRule = None, previous = Some(bs), preds.tail)
                  case _ =>
                    i.reifiedRelationshipTarget match {
                      case Some(rr) =>
                        val (e2, sp) = f.createSegmentPredicate(extent = e1, bodySegment = bs,
                          predicate = None,
                          reifiedRelationshipSource = None,
                          reifiedRelationshipInverseSource = None,
                          reifiedRelationshipTarget = Some(rr),
                          reifiedRelationshipInverseTarget = None,
                          unreifiedRelationshipInverse = None)
                        val next = o2r.copy(
                          rextent = e2,
                          ruleBodySegments = o2r.ruleBodySegments + (i.ruleBodySegment -> bs),
                          segmentPredicates = o2r.segmentPredicates + (i.segmentPredicate -> sp))
                        updateSegmentPredicates(next, chainRule = None, previous = Some(bs), preds.tail)
                      case _ =>
                        i.reifiedRelationshipInverseTarget match {
                          case Some(rr) =>
                            val (e2, sp) = f.createSegmentPredicate(extent = e1, bodySegment = bs,
                              predicate = None,
                              reifiedRelationshipSource = None,
                              reifiedRelationshipInverseSource = None,
                              reifiedRelationshipTarget = None,
                              reifiedRelationshipInverseTarget = Some(rr),
                              unreifiedRelationshipInverse = None)
                            val next = o2r.copy(
                              rextent = e2,
                              ruleBodySegments = o2r.ruleBodySegments + (i.ruleBodySegment -> bs),
                              segmentPredicates = o2r.segmentPredicates + (i.segmentPredicate -> sp))
                            updateSegmentPredicates(next, chainRule = None, previous = Some(bs), preds.tail)
                          case _ =>
                            i.unreifiedRelationshipInverse match {
                              case Some(ur) =>
                                val (e2, sp) = f.createSegmentPredicate(extent = e1, bodySegment = bs,
                                  predicate = None,
                                  reifiedRelationshipSource = None,
                                  reifiedRelationshipInverseSource = None,
                                  reifiedRelationshipTarget = None,
                                  reifiedRelationshipInverseTarget = None,
                                  unreifiedRelationshipInverse = Some(ur))
                                val next = o2r.copy(
                                  rextent = e2,
                                  ruleBodySegments = o2r.ruleBodySegments + (i.ruleBodySegment -> bs),
                                  segmentPredicates = o2r.segmentPredicates + (i.segmentPredicate -> sp))
                                updateSegmentPredicates(next, chainRule = None, previous = Some(bs), preds.tail)
                              case _ =>
                                new EMFProblems(new java.lang.IllegalArgumentException(
                                  s"updateSegmentPredicates: invalid SegmentPredicateInfo: $i"
                                )).left
                            }
                        }
                    }
                }
            }
        }
    } else
      o2r.right[EMFProblems]

    def updateChainRules1
    (t0: TerminologyBox)
    (current: EMFProblems \/ OMLText2Resolver,
     x: (ChainRule, api.UnreifiedRelationship, Seq[SegmentPredicateInfo]))
    : EMFProblems \/ OMLText2Resolver
    = for {
      o2r0 <- current
      t1 = o2r0.tboxes(t0)
      (ext, y) = f.createChainRule(
        extent = o2r0.rextent,
        tbox = t1,
        name = tables.taggedTypes.localName(x._1.getName),
        head = x._2)
      o2r1 <- updateSegmentPredicates(o2r0.copy(rextent = ext), Some(y), None, x._3)
      next = o2r1.copy(
        queue_elements = o2r1.queue_elements - x._1,
        chainRules = o2r1.chainRules + (x._1 -> y))
    } yield next

    for {
      s0 <- current
      prev = s0(x._1)
      next <- x._2.foldLeft[EMFProblems \/ OMLText2Resolver](\/-(prev))(updateChainRules1(x._1))
      s1 = s0.updated(x._1, next)
    } yield s1
  }

  @scala.annotation.tailrec
  final def eliminationConverter1
  (c0: EMFProblems \/ Map[Module, OMLText2Resolver])
  (implicit factory: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = c0 match {
    case \/-(c) =>
      if (c.isResolved)
        \/-(c)
      else {
        val r1 = resolvableCardinalityAspectRestrictions(c)
        val r2 = resolvableCardinalityConceptRestrictions(c)
        val r3 = resolvableCardinalityReifiedRelationshipRestrictions(c)
        val r4 = resolvableConceptDesignationTerminologyAxioms(c)
        val r5 = resolvableTerminologyNestingAxioms(c)
        val r6 = resolvableBundledTerminologyAxioms(c)
        val r7 = resolvableDescriptionBoxExtendsClosedWorldDefinitions(c)
        val r8 = resolvableDescriptionBoxRefinements(c)
        val r9 = resolvableReifiedRelationships(c)
        val rA = resolvableUnreifiedRelationships(c)
        val rB = resolvableReifiedRelationshipRestrictions(c)

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

  @scala.annotation.tailrec
  final def eliminationConverter2
  (c0: EMFProblems \/ Map[Module, OMLText2Resolver])
  (implicit factory: api.OMLResolvedFactory)
  : EMFProblems \/ Map[Module, OMLText2Resolver]
  = c0 match {
    case \/-(c) =>
      if (c.isResolved)
        \/-(c)
      else {
        val r1 = resolvableChainRules(c)

        val more
        = r1.nonEmpty

        if (more) {
          val c1 = r1.foldLeft(c0)(updateChainRules)
          eliminationConverter2(c1)
        } else
          c0
      }
    case -\/(errors) =>
      -\/(errors)
  }

  // what if multiple iri => same extent?
  def convert
  (fileModules: Map[tables.taggedTypes.IRI, Module])
  (implicit factory: api.OMLResolvedFactory)
  : EMFProblems \/ (Map[Module, OMLText2Resolver], Seq[(api.Module, api.Extent)])
  = for {
    c00 <- fileModules.map { case (iri, module) =>
      module -> OMLText2Resolver(iri = iri, rextent = api.Extent())
    }.right[EMFProblems]

    // Modules, AnnotationProperties & TerminologyExtensions

    c01 <- c00.foldLeft(c00.right[EMFProblems])(convertModules)

    c02 <- c01.foldLeft(c01.right[EMFProblems])(convertAnnotationProperties)

    c03 <- c02.foldLeft(c02.right[EMFProblems])(convertTerminologyExtensions)

    c0N = c03

    // Atomic Entities & DataTypes

    c10 = c0N

    c11 <- c10.foldLeft(c10.right[EMFProblems])(convertAspects)

    c12 <- c11.foldLeft(c11.right[EMFProblems])(convertConcepts)

    c13 <- c12.foldLeft(c12.right[EMFProblems])(convertStructures)

    c14 <- c13.foldLeft(c13.right[EMFProblems])(convertScalars)

    c1N = c14

    // Data Ranges

    c20 = c1N

    c21 <- convertRestrictedDataRanges(c20)

    c22 <- c21.foldLeft(c21.right[EMFProblems])(convertScalarOneOfLiteralAxioms)

    c2N = c22

    // Relational constructs

    c30 = c2N

    c31 <- eliminationConverter1(\/-(c30))

    c3N = c31

    // DataRelationships

    c40 = c3N

    c41 <- c40.foldLeft(c40.right[EMFProblems])(convertEntityScalarDataProperties)

    c42 <- c41.foldLeft(c41.right[EMFProblems])(convertEntityStructuredDataProperties)

    c43 <- c42.foldLeft(c42.right[EMFProblems])(convertScalarDataProperties)

    c44 <- c43.foldLeft(c43.right[EMFProblems])(convertStructuredDataProperties)

    c4N = c44

    // Restrictions
    
    c50 = c4N

    c51 <- c50.foldLeft(c50.right[EMFProblems])(convertEntityRestrictionAxioms)

    c52 <- c51.foldLeft(c51.right[EMFProblems])(convertEntityScalarDataPropertyRestrictionAxioms)

    c53 <- c52.foldLeft(c52.right[EMFProblems])(convertEntityStructuredDataPropertyRestrictionAxioms)

    c5N = c53
    
    // Specializations
    
    c60 = c5N

    c61 <- eliminationConverter2(\/-(c60))
    c62 <- c61.foldLeft(c61.right[EMFProblems])(convertSpecializationAxioms)
    c63 <- c62.foldLeft(c62.right[EMFProblems])(convertSubDataPropertyOfAxioms)
    c64 <- c63.foldLeft(c63.right[EMFProblems])(convertSubObjectPropertyOfAxioms)
    
    c6N = c64
    
    // Disjunctions
    
    c70 = c6N

    c71 <- c70.foldLeft(c70.right[EMFProblems])(convertRootConceptTaxonomyAxioms)
    
    c7N = c71
    
    // ConceptualEntityInstances
    
    c80 = c7N

    c81 <- c80.foldLeft(c80.right[EMFProblems])(convertConceptInstances)

    c82 <- c81.foldLeft(c81.right[EMFProblems])(convertReifiedRelationshipInstances)

    c83 <- c82.foldLeft(c82.right[EMFProblems])(convertReifiedRelationshipInstanceDomains)

    c84 <- c83.foldLeft(c83.right[EMFProblems])(convertReifiedRelationshipInstanceRanges)

    c85 <- c84.foldLeft(c84.right[EMFProblems])(convertUnreifiedRelationshipInstances)

    c8N = c85
    
    // Data Property Values

    c90 = c8N

    c91 <- c90.foldLeft(c90.right[EMFProblems])(convertSingletonScalarDataPropertyValues)

    c92 <- c91.foldLeft(c91.right[EMFProblems])(convertSingletonStructuredDataPropertyValues)

    c9N = c92

    // AnnotationProperties

    cA0 = c9N

    cA1 <- cA0.foldLeft(cA0.right[EMFProblems])(convertAnnotations)

    cAN = cA1

    _ <- if (cAN.isResolved)
      ().right[EMFProblems]
    else {
      val buff = new mutable.StringBuilder
      buff ++= s"Incomplete OML Text 2 Resolver conversion\n"
      cAN.foreach { case (m, o2r) =>
        val kind = m match {
          case _: TerminologyGraph =>
            "TerminologyGraph"
          case _: Bundle =>
            "Bundle"
          case _: DescriptionBox =>
            "DescriptionBox"
        }
        buff ++= s"$kind <${o2r.iri}> : "
        if (o2r.isResolved)
          buff ++= "resolved!\n"
        else {
          buff ++= "incompletely resolved!\n"
          buff ++= o2r.toString
          buff ++= "\n"
        }
      }
      new EMFProblems(new java.lang.IllegalArgumentException(buff.toString)).left
    }

    m2e <- cAN
      .foldLeft[EMFProblems \/ Seq[(api.Module, api.Extent)]](Seq.empty[(api.Module, api.Extent)].right) {
      case (acc, (mi,o2r)) =>
        for {
          prev <- acc
          mj <- o2r.moduleLookup(mi) match {
            case Some(mj) =>
              mj.right[EMFProblems]
            case _ =>
              new EMFProblems(new java.lang.IllegalArgumentException(
                s"Failed to lookup module: $mi"
              )).left
          }
        } yield prev :+ (mj -> o2r.rextent )
    }

  } yield cAN -> m2e

}
