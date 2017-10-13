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

import java.lang.{IllegalArgumentException, System}
import java.util.UUID

import gov.nasa.jpl.imce.oml.converters.OMLResolver2Ontology.MutableTboxOrDbox
import gov.nasa.jpl.imce.oml.resolver.Filterable._
import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.omf.scala.binding.owlapi
import gov.nasa.jpl.omf.scala.binding.owlapi.types.{terminologies => owlapiterminologies}
import gov.nasa.jpl.omf.scala.binding.owlapi.{descriptions => owlapidescriptions}
import gov.nasa.jpl.omf.scala.core
import org.semanticweb.owlapi.model.IRI

import scala.collection.immutable.{::, Iterable, List, Map, Nil, Seq, Set}
import scala.{Boolean, Function, None, Option, Some, StringContext}
import scala.Predef.ArrowAssoc
import scalaz._
import Scalaz._

object OMLResolver2Ontology {

  type MutableTboxOrDbox = owlapiterminologies.MutableTerminologyBox \/ owlapidescriptions.MutableDescriptionBox

  type Throwables = core.OMFError.Throwables

  type ResolverResult = Throwables \/ OMLResolver2Ontology

  def convert(extent: api.Extent, prev: OMLResolver2Ontology)
  : Throwables \/ OMLResolver2Ontology
  = for {
    tuple <-
    (extent.terminologyGraphs.toList,
      extent.bundles.toList,
      extent.descriptionBoxes.toList) match {
      case (g :: Nil, Nil, Nil) =>
        Function.tupled(prev.convertTerminologyBox _)(g)
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


    updated = next.copy(extents = next.extents :+ extent)

    result <- if (updated.drc.isBuiltInModule(i)(updated.ops))
      updated.withBuiltIn(m0, m1)(extent)
    else {
      System.out.println(s"==> OMLResolver2Ontology converting: $iri")
      convert(updated.right[Throwables], extent, iri, m0, m1, i)
    }
  } yield result

  def convert(c00: Throwables \/ OMLResolver2Ontology, extent: api.Extent, iri: tables.IRI, m0: api.Module, m1: MutableTboxOrDbox, i: IRI)
  : Throwables \/ OMLResolver2Ontology
  = for {
    // AnnotationProperties
    c10 <- c00
    c11 <- extent.annotationProperties.foldLeft(c10.right[Throwables])(convertAnnotationProperty(m1))
    c1N = c11

    // TerminologyExtensions
    c20 = c1N
    c21 <- extent.terminologyBoxOfTerminologyBoxAxiom.foldLeft(c20.right[Throwables])(convertTerminologyExtension(extent, iri))
    c2N = c21

    // Atomic Entities
    c30 = c2N
    c31 <- extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c30.right[Throwables])(convertAspectOrConcept)
    c3N = c31

    // Other ModuleEdges
    c40 = c3N
    c41 <- extent.terminologyBoxOfTerminologyBoxAxiom.foldLeft(c40.right[Throwables])(convertConceptDesignationTerminologyAxiom(extent))
    c42 <- extent.terminologyBoxOfTerminologyBoxAxiom.foldLeft(c41.right[Throwables])(convertTerminologyNestingAxiom(extent))
    c43 <- extent.bundleOfTerminologyBundleAxiom.foldLeft(c42.right[Throwables])(convertBundledTerminologyAxiom(extent))
    c44 <- extent.descriptionBoxOfDescriptionBoxExtendsClosedWorldDefinitions.foldLeft(c43.right[Throwables])(convertDescriptionBoxExtendsClosedWorldDefinition(extent))
    c45 <- extent.descriptionBoxOfDescriptionBoxRefinement.foldLeft(c44.right[Throwables])(convertDescriptionBoxRefinement(extent))
    c4N = c41

    // Relationships

    c50 = c4N
    c51 <- extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c50.right[Throwables])(convertReifiedRelationship(extent))
    c52 <- extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c51.right[Throwables])(convertUnreifiedRelationship(extent))
    c5N = c52

    // DataTypes

    c60 = c5N
    c61 <- extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c60.right[Throwables])(convertStructure(extent))
    c62 <- extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c61.right[Throwables])(convertScalar(extent))
    c63 <- {
      val drs =
        extent
          .terminologyBoxOfTerminologyBoxStatement
          .selectByKindOf { case (dr: api.RestrictedDataRange, t: api.TerminologyBox) => dr -> t }
      System.out.println(s"- IRI: $iri\n- Request: drs size ${drs.size}")
      convertRestrictedDataRanges(c62.right[Throwables], drs, List.empty)(c62.omfStore)
    }
    c64 <- extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c63.right[Throwables])(convertScalarOneOfLiteralAxiom(extent))
    c6N = c64

    // DataRelationships

    c70 = c6N
    c71 <- extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c70.right[Throwables])(convertEntityScalarDataProperty(extent))
    c72 <- extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c71.right[Throwables])(convertEntityStructuredDataProperty(extent))
    c73 <- extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c72.right[Throwables])(convertScalarDataProperty(extent))
    c74 <- extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c73.right[Throwables])(convertStructuredDataProperty(extent))
    c7N = c74

    // Restrictions

    c80 = c7N
    c81 <- extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c80.right[Throwables])(convertEntityRestrictionAxiom(extent))
    c82 <- extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c81.right[Throwables])(convertEntityScalarDataPropertyRestrictionAxiom(extent))
    c8N = c82

    // Specializations

    c90 = c8N
    c90 <- extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(c90.right[Throwables])(convertSpecializationAxiom(extent))
    c9N = c90

    // Disjunctions

    cA0 = c9N
    cA1 <- extent.terminologyBoxOfTerminologyBoxStatement.foldLeft(cA0.right[Throwables])(convertRootConceptTaxonomyAxiom(extent))
    cAN = cA1

    // ConceptualEntityInstances & UnreifiedRelationshipInstanceTuples
    cB0 = cAN
    cB1 <- extent.descriptionBoxOfConceptInstance.foldLeft(cB0.right[Throwables])(convertConceptInstances(extent))
    cB2 <- extent.descriptionBoxOfReifiedRelationshipInstance.foldLeft(cB1.right[Throwables])(convertReifiedRelationshipInstances(extent))
    cB3 <- extent.descriptionBoxOfReifiedRelationshipInstanceDomain.foldLeft(cB2.right[Throwables])(convertReifiedRelationshipInstanceDomains(extent))
    cB4 <- extent.descriptionBoxOfReifiedRelationshipInstanceRange.foldLeft(cB3.right[Throwables])(convertReifiedRelationshipInstanceRanges(extent))
    cB5 <- extent.descriptionBoxOfUnreifiedRelationshipInstanceTuple.foldLeft(cB4.right[Throwables])(convertUnreifiedReifiedRelationshipInstanceTuples(extent))
    cBN = cB5

    // Data Property Values
    cC0 = cBN
    cC1 <- extent.descriptionBoxOfSingletonInstanceScalarDataPropertyValue.foldLeft(cC0.right[Throwables])(convertSingletonInstanceScalarDataPropertyValues(extent))
    cC2 <- extent.descriptionBoxOfSingletonInstanceStructuredDataPropertyValue.foldLeft(cC1.right[Throwables])(convertSingletonInstanceStructuredDataPropertyValues(extent))
    cC3 <- extent.structuredPropertyTuples.foldLeft(cC2.right[Throwables])(convertStructuredPropertyTuples(extent))
    cC4 <- extent.descriptionBoxOfSingletonInstanceScalarDataPropertyValue.foldLeft(cC3.right[Throwables])(convertSingletonInstanceScalarDataPropertyValues(extent))
    cCN = cC4
    // Annotations

    // Finished!
//    result = c3N
//    result = c4N
//    result = c5N
//    result = c6N
//    result = c7N
//    result = c8N
//    result = c9N
    result = cBN

    _ = java.lang.System.out.println(s"==> OMLResolver2Ontology  converted: $iri")
  } yield result


  protected def convertTerminologyKind(k: tables.TerminologyKind): core.TerminologyKind = k match {
    case tables.OpenWorldDefinitions =>
      core.TerminologyKind.isOpenWorld
    case tables.ClosedWorldDesignations =>
      core.TerminologyKind.isClosedWorld
  }

  protected def convertDescriptionKind(k: tables.DescriptionKind): core.DescriptionKind = k match {
    case tables.Final =>
      core.DescriptionKind.isFinal
    case tables.Partial =>
      core.DescriptionKind.isPartial
  }

  protected def convertAnnotationProperty(m1: MutableTboxOrDbox)
  : (ResolverResult, (UUID, api.AnnotationProperty)) => ResolverResult
  = {
    case (acc, (uuid, ap0)) =>
      for {
        r2o <- acc
        ap1 <- m1 match {
          case -\/(mtbox) =>
            r2o.ops.addTerminologyAnnotationProperty(
              mtbox,
              tables.AnnotationProperty(uuid.toString, ap0.iri, ap0.abbrevIRI))(r2o.omfStore)
          case \/-(mdbox) =>
            r2o.ops.addDescriptionAnnotationProperty(
              mdbox,
              tables.AnnotationProperty(uuid.toString, ap0.iri, ap0.abbrevIRI))(r2o.omfStore)
        }
      } yield r2o.copy(aps = r2o.aps + (ap0 -> ap1))
  }

  // Atomic Entities

  protected val convertAspectOrConcept
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (a0: api.Aspect, t0: api.TerminologyBox)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        a1 <- r2o.ops.addAspect(t1, core.OMLString.LocalName(a0.name))(r2o.omfStore)
      } yield r2o.copy(aspects = r2o.aspects + (a0 -> a1))
    case (acc, (c0: api.Concept, t0: api.TerminologyBox)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        c1 <- r2o.ops.addConcept(t1, core.OMLString.LocalName(c0.name))(r2o.omfStore)
      } yield r2o.copy(concepts = r2o.concepts + (c0 -> c1))
    case (acc, _) =>
      acc
  }

  // ModuleEdges

  protected def convertTerminologyExtension(ext: api.Extent, iri: tables.IRI)
  : (ResolverResult, (api.TerminologyBoxAxiom, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (ax0: api.TerminologyExtensionAxiom, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        e1 <- r2o.getTboxByIRI(ax0.extendedTerminology)
        _ = System.out.println(s"convertTerminologyExtension($iri)\n\textendingG=${t1.iri}\n\t  extendedG=${e1.iri}")
        ax1 <- r2o.ops.addTerminologyExtension(extendingG = t1, extendedG = e1)(r2o.omfStore)
      } yield r2o.copy(edges = r2o.edges + (ax0 -> ax1))
    case (acc, _) =>
      acc
  }

  protected def convertConceptDesignationTerminologyAxiom(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxAxiom, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (ax0: api.ConceptDesignationTerminologyAxiom, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        dt1 <- r2o.getTboxByIRI(ax0.designatedTerminology)
        dc1 <- r2o.lookupConcept(ax0.designatedConcept)
        ax1 <- r2o.ops.addEntityConceptDesignationTerminologyAxiom(t1, dc1, dt1)(r2o.omfStore)
      } yield r2o.copy(edges = r2o.edges + (ax0 -> ax1))
    case (acc, _) =>
      acc
  }

  protected def convertTerminologyNestingAxiom(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxAxiom, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (ax0: api.TerminologyNestingAxiom, g0)) =>
      for {
        r2o <- acc
        g1 <- r2o.getGbox(g0)
        nt1 <- r2o.getTboxByIRI(ax0.nestingTerminology)
        nc1 <- r2o.lookupConcept(ax0.nestingContext)
        ax1 <- r2o.ops.addNestedTerminology(nt1, nc1, g1)(r2o.omfStore)
      } yield r2o.copy(edges = r2o.edges + (ax0 -> ax1))
    case (acc, _) =>
      acc
  }

  protected def convertBundledTerminologyAxiom(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBundleAxiom, api.Bundle)) => ResolverResult
  = {
    case (acc, (ax0: api.BundledTerminologyAxiom, b0)) =>
      for {
        r2o <- acc
        b1 <- r2o.getBundle(b0)
        t1 <- r2o.getTboxByIRI(ax0.bundledTerminology)
        ax1 <- r2o.ops.addBundledTerminologyAxiom(b1, t1)(r2o.omfStore)
      } yield r2o.copy(edges = r2o.edges + (ax0 -> ax1))
    case (acc, _) =>
      acc
  }

  protected def convertDescriptionBoxExtendsClosedWorldDefinition(implicit ext: api.Extent)
  : (ResolverResult, (api.DescriptionBoxExtendsClosedWorldDefinitions, api.DescriptionBox)) => ResolverResult
  = {
    case (acc, (ax0, d0)) =>
      for {
        r2o <- acc
        d1 <- r2o.getDbox(d0)
        t1 <- r2o.getTboxByIRI(ax0.closedWorldDefinitions)
        ax1 <- r2o.ops.addDescriptionBoxExtendsClosedWorldDefinitions(d1, t1)(r2o.omfStore)
      } yield r2o.copy(edges = r2o.edges + (ax0 -> ax1))
    case (acc, _) =>
      acc
  }

  protected def convertDescriptionBoxRefinement(implicit ext: api.Extent)
  : (ResolverResult, (api.DescriptionBoxRefinement, api.DescriptionBox)) => ResolverResult
  = {
    case (acc, (ax0, d0)) =>
      for {
        r2o <- acc
        d1 <- r2o.getDbox(d0)
        rd1 <- r2o.lookupDbox(ax0.refinedDescriptionBox)
        ax1 <- r2o.ops.addDescriptionBoxRefinement(d1, rd1)(r2o.omfStore)
      } yield r2o.copy(edges = r2o.edges + (ax0 -> ax1))
    case (acc, _) =>
      acc
  }

  // Relationships

  protected def convertReifiedRelationship(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (rr0: api.ReifiedRelationship, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        rs1 <- r2o.entityLookup(rr0.source)
        rt1 <- r2o.entityLookup(rr0.target)
        rr1 <- r2o.ops.addReifiedRelationship(
          t1, rs1, rt1,
          Iterable() ++
            (if (rr0.isAsymmetric)
              Iterable(core.RelationshipCharacteristics.isAsymmetric)
            else Iterable()) ++
            (if (rr0.isEssential)
              Iterable(core.RelationshipCharacteristics.isEssential)
            else Iterable()) ++
            (if (rr0.isFunctional)
              Iterable(core.RelationshipCharacteristics.isFunctional)
            else Iterable()) ++
            (if (rr0.isInverseEssential)
              Iterable(core.RelationshipCharacteristics.isInverseEssential)
            else Iterable()) ++
            (if (rr0.isInverseFunctional)
              Iterable(core.RelationshipCharacteristics.isInverseFunctional)
            else Iterable()) ++
            (if (rr0.isIrreflexive)
              Iterable(core.RelationshipCharacteristics.isIrreflexive)
            else Iterable()) ++
            (if (rr0.isReflexive)
              Iterable(core.RelationshipCharacteristics.isReflexive)
            else Iterable()) ++
            (if (rr0.isSymmetric)
              Iterable(core.RelationshipCharacteristics.isSymmetric)
            else Iterable()) ++
            (if (rr0.isTransitive)
              Iterable(core.RelationshipCharacteristics.isTransitive)
            else Iterable()),
          core.OMLString.LocalName(rr0.name),
          core.OMLString.LocalName(rr0.unreifiedPropertyName),
          rr0.unreifiedInversePropertyName.map(core.OMLString.LocalName(_))
        )(r2o.omfStore)
      } yield r2o.copy(reifiedRelationships = r2o.reifiedRelationships + (rr0 -> rr1))
    case (acc, _) =>
      acc
  }

  protected def convertUnreifiedRelationship(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (ur0: api.UnreifiedRelationship, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        us1 <- r2o.entityLookup(ur0.source)
        ut1 <- r2o.entityLookup(ur0.target)
        ur1 <- r2o.ops.addUnreifiedRelationship(
          t1, us1, ut1,
          Iterable() ++
            (if (ur0.isAsymmetric)
              Iterable(core.RelationshipCharacteristics.isAsymmetric)
            else Iterable()) ++
            (if (ur0.isEssential)
              Iterable(core.RelationshipCharacteristics.isEssential)
            else Iterable()) ++
            (if (ur0.isFunctional)
              Iterable(core.RelationshipCharacteristics.isFunctional)
            else Iterable()) ++
            (if (ur0.isInverseEssential)
              Iterable(core.RelationshipCharacteristics.isInverseEssential)
            else Iterable()) ++
            (if (ur0.isInverseFunctional)
              Iterable(core.RelationshipCharacteristics.isInverseFunctional)
            else Iterable()) ++
            (if (ur0.isIrreflexive)
              Iterable(core.RelationshipCharacteristics.isIrreflexive)
            else Iterable()) ++
            (if (ur0.isReflexive)
              Iterable(core.RelationshipCharacteristics.isReflexive)
            else Iterable()) ++
            (if (ur0.isTransitive)
              Iterable(core.RelationshipCharacteristics.isTransitive)
            else Iterable()),
          core.OMLString.LocalName(ur0.name)
        )(r2o.omfStore)
      } yield r2o.copy(unreifiedRelationships = r2o.unreifiedRelationships + (ur0 -> ur1))
    case (acc, _) =>
      acc
  }

  // DataTypes

  protected def convertStructure(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (s0: api.Structure, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        s1 <- r2o.ops.addStructuredDataType(t1, core.OMLString.LocalName(s0.name))(r2o.omfStore)
      } yield r2o.copy(structures = r2o.structures + (s0 -> s1))
    case (acc, _) =>
      acc
  }

  protected def convertScalar(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (s0: api.Scalar, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        s1 <- r2o.ops.addScalarDataType(t1, core.OMLString.LocalName(s0.name))(r2o.omfStore)
      } yield r2o.copy(dataRanges = r2o.dataRanges + (s0 -> s1))
    case (acc, _) =>
      acc
  }

  @scala.annotation.tailrec
  protected def convertRestrictedDataRanges
  (acc: ResolverResult,
   drs: Iterable[(api.RestrictedDataRange, api.TerminologyBox)],
   queue: List[(api.RestrictedDataRange, api.TerminologyBox)],
   progress: Boolean = false)
  (implicit omfStore: owlapi.OWLAPIOMF#Store)
  : ResolverResult
  = if (drs.isEmpty) {
    if (queue.isEmpty)
      acc
    else if (progress) {
      System.out.println(s"- Restart with drs size ${queue.size}")
      convertRestrictedDataRanges(acc, queue, List.empty)
    } else {
      Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
        s"convertRestrictedDataRanges: no progress with ${queue.size} data ranges in the queue: " +
          queue.map(_._1.name).mkString(", "))).left
    }
  }
  else acc match {
    case \/-(r2o) =>
      val (dr0, t0) = drs.head
      (r2o.lookupTbox(t0), r2o.dataRanges.get(dr0.restrictedRange)) match {
        case (Some(t1), Some(rr1)) =>
          val dr1 = dr0 match {
            case rdr0: api.BinaryScalarRestriction =>
              r2o.ops
                .addBinaryScalarRestriction(
                  t1, core.OMLString.LocalName(rdr0.name), rr1,
                  rdr0.length, rdr0.minLength, rdr0.maxLength)
                .map { rdr1 => r2o.copy(dataRanges = r2o.dataRanges + (rdr0 -> rdr1)) }
            case rdr0: api.IRIScalarRestriction =>
              r2o.ops
                .addIRIScalarRestriction(
                  t1, core.OMLString.LocalName(rdr0.name), rr1,
                  rdr0.length, rdr0.minLength, rdr0.maxLength, rdr0.pattern)
                .map { rdr1 => r2o.copy(dataRanges = r2o.dataRanges + (rdr0 -> rdr1)) }
            case rdr0: api.NumericScalarRestriction =>
              r2o.ops
                .addNumericScalarRestriction(
                  t1, core.OMLString.LocalName(rdr0.name), rr1,
                  rdr0.minInclusive, rdr0.maxInclusive,
                  rdr0.minExclusive, rdr0.maxExclusive)
                .map { rdr1 => r2o.copy(dataRanges = r2o.dataRanges + (rdr0 -> rdr1)) }
            case rdr0: api.PlainLiteralScalarRestriction =>
              r2o.ops
                .addPlainLiteralScalarRestriction(
                  t1, core.OMLString.LocalName(rdr0.name), rr1,
                  rdr0.length, rdr0.minLength, rdr0.maxLength, rdr0.pattern,
                  rdr0.langRange.map(core.OMLString.LangRange(_)))
                .map { rdr1 => r2o.copy(dataRanges = r2o.dataRanges + (rdr0 -> rdr1)) }
            case rdr0: api.ScalarOneOfRestriction =>
              r2o.ops
                .addScalarOneOfRestriction(
                  t1, core.OMLString.LocalName(rdr0.name), rr1)
                .map { rdr1 => r2o.copy(dataRanges = r2o.dataRanges + (rdr0 -> rdr1)) }
            case rdr0: api.StringScalarRestriction =>
              r2o.ops
                .addStringScalarRestriction(
                  t1, core.OMLString.LocalName(rdr0.name), rr1,
                  rdr0.length, rdr0.minLength, rdr0.maxLength, rdr0.pattern)
                .map { rdr1 => r2o.copy(dataRanges = r2o.dataRanges + (rdr0 -> rdr1)) }
            case rdr0: api.SynonymScalarRestriction =>
              r2o.ops
                .addSynonymScalarRestriction(
                  t1, core.OMLString.LocalName(rdr0.name), rr1)
                .map { rdr1 => r2o.copy(dataRanges = r2o.dataRanges + (rdr0 -> rdr1)) }
            case rdr0: api.TimeScalarRestriction =>
              r2o.ops
                .addTimeScalarRestriction(
                  t1, core.OMLString.LocalName(rdr0.name), rr1,
                  rdr0.minInclusive, rdr0.maxInclusive,
                  rdr0.minExclusive, rdr0.maxExclusive)
                .map { rdr1 => r2o.copy(dataRanges = r2o.dataRanges + (rdr0 -> rdr1)) }
          }
          dr1 match {
            case \/-(next) =>
              convertRestrictedDataRanges(\/-(next), drs.tail, queue, true)
            case -\/(errors) =>
              -\/(errors)
          }
        case (Some(t1), None) =>
          System.out.println(s"- Defer: drs size ${drs.tail.size}, queue size ${1 + queue.size}")
          convertRestrictedDataRanges(acc, drs.tail, drs.head :: queue)
        case (None, _) =>
          Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
            s"convertRestrictedDataRanges: Failed to resolve " +
              s"tbox: $t0")).left
      }
    case _ =>
      acc
  }

  protected def convertScalarOneOfLiteralAxiom(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (s0: api.ScalarOneOfLiteralAxiom, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        r1 <- r2o.lookupScalarOneOfRestriction(s0.axiom)
        vt1 <- s0.valueType match {
          case Some(vt0) =>
            r2o.lookupDataRange(vt0).map(Option.apply)
          case None =>
            Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
              s"convertScalarOneOfLiteralAxiom: Failed to resolve " +
                s"value type: ${s0.valueType}")).left
        }
        s1 <- r2o.ops.addScalarOneOfLiteralAxiom(t1, r1, s0.value, vt1)(r2o.omfStore)
      } yield r2o.copy(scalarOneOfLiterals = r2o.scalarOneOfLiterals + (s0 -> s1))
    case (acc, _) =>
      acc
  }

  // DataRelationships

  protected def convertEntityScalarDataProperty(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (dp0: api.EntityScalarDataProperty, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        e1 <- r2o.entityLookup(dp0.domain)
        r1 <- r2o.lookupDataRange(dp0.range)
        dp1 <- r2o.ops.addEntityScalarDataProperty(
          t1, e1, r1,
          core.OMLString.LocalName(dp0.name),
          dp0.isIdentityCriteria)(r2o.omfStore)
      } yield r2o.copy(entityScalarDataProperties = r2o.entityScalarDataProperties + (dp0 -> dp1))
    case (acc, _) =>
      acc
  }

  protected def convertEntityStructuredDataProperty(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (dp0: api.EntityStructuredDataProperty, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        e1 <- r2o.entityLookup(dp0.domain)
        r1 <- r2o.lookupStructure(dp0.range)
        dp1 <- r2o.ops.addEntityStructuredDataProperty(
          t1, e1, r1,
          core.OMLString.LocalName(dp0.name),
          dp0.isIdentityCriteria)(r2o.omfStore)
      } yield r2o.copy(entityStructuredDataProperties = r2o.entityStructuredDataProperties + (dp0 -> dp1))
    case (acc, _) =>
      acc
  }

  protected def convertScalarDataProperty(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (dp0: api.ScalarDataProperty, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        e1 <- r2o.lookupStructure(dp0.domain)
        r1 <- r2o.lookupDataRange(dp0.range)
        dp1 <- r2o.ops.addScalarDataProperty(
          t1, e1, r1,
          core.OMLString.LocalName(dp0.name))(r2o.omfStore)
      } yield r2o.copy(scalarDataProperties = r2o.scalarDataProperties + (dp0 -> dp1))
    case (acc, _) =>
      acc
  }

  protected def convertStructuredDataProperty(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (dp0: api.StructuredDataProperty, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        e1 <- r2o.lookupStructure(dp0.domain)
        r1 <- r2o.lookupStructure(dp0.range)
        dp1 <- r2o.ops.addStructuredDataProperty(
          t1, e1, r1,
          core.OMLString.LocalName(dp0.name))(r2o.omfStore)
      } yield r2o.copy(structuredDataProperties = r2o.structuredDataProperties + (dp0 -> dp1))
    case (acc, _) =>
      acc
  }

  // Entity Restrictions

  protected def convertEntityRestrictionAxiom(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (er0: api.EntityRestrictionAxiom, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        d1 <- r2o.entityLookup(er0.restrictedDomain)
        r1 <- r2o.entityLookup(er0.restrictedRange)
        rel1 <- r2o.entityRelationshipLookup(er0.restrictedRelation)
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

  protected def convertEntityScalarDataPropertyRestrictionAxiom(implicit ext: api.Extent)
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
    case (acc, _) =>
      acc
  }

  // Specializations

  protected def convertSpecializationAxiom(implicit ext: api.Extent)
  : (ResolverResult, (api.TerminologyBoxStatement, api.TerminologyBox)) => ResolverResult
  = {
    case (acc, (ax0: api.SpecializationAxiom, t0)) =>
      for {
        r2o <- acc
        t1 <- r2o.getTbox(t0)
        p1 <- r2o.entityLookup(ax0.parent())
        c1 <- r2o.entityLookup(ax0.child())
        ax1 <- (ax0, p1, c1) match {
          case (_: api.ConceptSpecializationAxiom, sup: owlapi.types.terms.Concept, sub: owlapi.types.terms.Concept) =>
            r2o.ops.addConceptSpecializationAxiom(t1, sub, sup)(r2o.omfStore)
          case (_: api.AspectSpecializationAxiom, sup: owlapi.types.terms.Aspect, sub) =>
            r2o.ops.addAspectSpecializationAxiom(t1, sub, sup)(r2o.omfStore)
          case (_: api.ReifiedRelationshipSpecializationAxiom, sup: owlapi.types.terms.ReifiedRelationship, sub: owlapi.types.terms.ReifiedRelationship) =>
            r2o.ops.addReifiedRelationshipSpecializationAxiom(t1, sub, sup)(r2o.omfStore)
          case _ =>
            Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
              s"convertSpecializationAxiom: Failed to resolve " +
                s"tbox: $t0" +
                s" for defining SpecializationAxiom: $ax0")).left
        }
      } yield r2o.copy(termAxioms = r2o.termAxioms + (ax0 -> ax1))
    case (acc, _) =>
      acc
  }

  // Disjunctions

  protected def convertRootConceptTaxonomyAxiom(implicit ext: api.Extent)
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

  protected def disjunctsForConceptTreeDisjunction
  (r2o: OMLResolver2Ontology, ctd0: api.ConceptTreeDisjunction, ctd1: owlapi.types.bundleStatements.ConceptTreeDisjunction)
  (implicit ext: api.Extent)
  : Seq[(api.DisjointUnionOfConceptsAxiom, owlapi.types.bundleStatements.ConceptTreeDisjunction)]
  = ext.disjunctions.getOrElse(ctd0, Set.empty).to[Seq].map(_ -> ctd1)

  @scala.annotation.tailrec
  protected def convertConceptTreeDisjunctions
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
        r2o.ops.addAnonymousConceptTaxonomyAxiom(b1, ctd1, core.OMLString.LocalName(ax0.name))(r2o.omfStore) match {
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

  // ConceptualEntityInstances

  protected def convertConceptInstances(implicit ext: api.Extent)
  : (ResolverResult, (api.ConceptInstance, api.DescriptionBox)) => ResolverResult
  = {
    case (acc, (ax0: api.ConceptInstance, d0: api.DescriptionBox)) =>
      for {
        r2o <- acc
        d1 <- r2o.getDbox(d0)
        c1 <- r2o.lookupConcept(ax0.singletonConceptClassifier)
        ax1 <- r2o.ops.addConceptInstance(d1, c1, core.OMLString.LocalName(ax0.name))(r2o.omfStore)
      } yield r2o.copy(conceptualEntitySingletonInstances = r2o.conceptualEntitySingletonInstances + (ax0 -> ax1))
  }

  protected def convertReifiedRelationshipInstances(implicit ext: api.Extent)
  : (ResolverResult, (api.ReifiedRelationshipInstance, api.DescriptionBox)) => ResolverResult
  = {
    case (acc, (ax0: api.ReifiedRelationshipInstance, d0: api.DescriptionBox)) =>
      for {
        r2o <- acc
        d1 <- r2o.getDbox(d0)
        rr1 <- r2o.lookupReifiedRelationship(ax0.singletonReifiedRelationshipClassifier)
        ax1 <- r2o.ops.addReifiedRelationshipInstance(d1, rr1, core.OMLString.LocalName(ax0.name))(r2o.omfStore)
      } yield r2o.copy(conceptualEntitySingletonInstances = r2o.conceptualEntitySingletonInstances + (ax0 -> ax1))
  }

  protected def convertReifiedRelationshipInstanceDomains(implicit ext: api.Extent)
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

  protected def convertReifiedRelationshipInstanceRanges(implicit ext: api.Extent)
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

  protected def convertUnreifiedReifiedRelationshipInstanceTuples(implicit ext: api.Extent)
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

  protected def convertSingletonInstanceScalarDataPropertyValues(implicit ext: api.Extent)
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

  protected def convertSingletonInstanceStructuredDataPropertyValues(implicit ext: api.Extent)
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

  protected def convertStructuredPropertyTuples(implicit ext: api.Extent)
  : (ResolverResult, (api.SingletonInstanceStructuredDataPropertyContext, Set[api.StructuredDataPropertyTuple])) => ResolverResult
  = {
    case (acc, (c0, ts0)) =>
      for {
        r2o1 <- acc
        c1 <- r2o1.lookupSingletonInstanceStructuredDataPropertyContext(c0)
        r2oN <- ts0.foldLeft(acc)(convertStructuredDataPropertyValue(c0, c1)(ext))
      } yield r2oN
  }

  protected def convertStructuredDataPropertyValue
  (c0: api.SingletonInstanceStructuredDataPropertyContext,
   c1: owlapi.descriptions.SingletonInstanceStructuredDataPropertyContext)
  (implicit ext: api.Extent)
  : (ResolverResult, api.StructuredDataPropertyTuple) => ResolverResult
  = {
    case (acc, t0) =>
      acc
  }
}

case class OMLResolver2Ontology
(drc: owlapi.BuiltInDatatypeMap,
 omfStore: owlapi.OWLAPIOMFGraphStore,
 extents: Seq[api.Extent] = Seq.empty,

 // Modules
 modules: Seq[api.Module] = Seq.empty,
 gs: Map[api.TerminologyGraph, owlapiterminologies.MutableTerminologyGraph] = Map.empty,
 bs: Map[api.Bundle, owlapiterminologies.MutableBundle] = Map.empty,
 ds: Map[api.DescriptionBox, owlapidescriptions.MutableDescriptionBox] = Map.empty,

 // AnnotationProperties
 aps: Map[api.AnnotationProperty, tables.AnnotationProperty] = Map.empty,

 // ModuleEdges
 edges: Map[api.ModuleEdge, owlapi.common.ModuleEdge] = Map.empty,

 aspects: Map[api.Aspect, owlapi.types.terms.Aspect] = Map.empty,

 concepts: Map[api.Concept, owlapi.types.terms.Concept] = Map.empty,
 reifiedRelationships: Map[api.ReifiedRelationship, owlapi.types.terms.ReifiedRelationship] = Map.empty,
 unreifiedRelationships: Map[api.UnreifiedRelationship, owlapi.types.terms.UnreifiedRelationship] = Map.empty,

 dataRanges: Map[api.DataRange, owlapi.types.terms.DataRange] = Map.empty,
 structures: Map[api.Structure, owlapi.types.terms.Structure] = Map.empty,
 scalarOneOfLiterals: Map[api.ScalarOneOfLiteralAxiom, owlapi.types.termAxioms.ScalarOneOfLiteralAxiom] = Map.empty,

 entityScalarDataProperties: Map[api.EntityScalarDataProperty, owlapi.types.terms.EntityScalarDataProperty] = Map.empty,
 entityStructuredDataProperties: Map[api.EntityStructuredDataProperty, owlapi.types.terms.EntityStructuredDataProperty] = Map.empty,
 scalarDataProperties: Map[api.ScalarDataProperty, owlapi.types.terms.ScalarDataProperty] = Map.empty,
 structuredDataProperties: Map[api.StructuredDataProperty, owlapi.types.terms.StructuredDataProperty] = Map.empty,

 termAxioms: Map[api.TermAxiom, owlapi.types.termAxioms.TermAxiom] = Map.empty,
 conceptTreeDisjunctions: Map[api.ConceptTreeDisjunction, owlapi.types.bundleStatements.ConceptTreeDisjunction] = Map.empty,
 disjointUnionOfConceptAxioms: Map[api.DisjointUnionOfConceptsAxiom, owlapi.types.bundleStatements.DisjointUnionOfConceptsAxiom] = Map.empty,

 conceptualEntitySingletonInstances: Map[api.ConceptualEntitySingletonInstance, owlapi.descriptions.ConceptualEntitySingletonInstance] = Map.empty,
 reifiedRelationshipInstanceDomains: Map[api.ReifiedRelationshipInstanceDomain, owlapi.descriptions.ReifiedRelationshipInstanceDomain] = Map.empty,
 reifiedRelationshipInstanceRanges: Map[api.ReifiedRelationshipInstanceRange, owlapi.descriptions.ReifiedRelationshipInstanceRange] = Map.empty,
 unreifiedRelationshipInstanceTuples: Map[api.UnreifiedRelationshipInstanceTuple, owlapi.descriptions.UnreifiedRelationshipInstanceTuple] = Map.empty,

 singletonScalarDataPropertyValues: Map[api.SingletonInstanceScalarDataPropertyValue, owlapi.descriptions.SingletonInstanceScalarDataPropertyValue] = Map.empty,
 singletonStructuredDataPropertyValues: Map[api.SingletonInstanceStructuredDataPropertyValue, owlapi.descriptions.SingletonInstanceStructuredDataPropertyValue] = Map.empty,
 structuredPropertyTuples: Map[api.StructuredDataPropertyTuple, owlapi.descriptions.StructuredDataPropertyTuple] = Map.empty,
 scalarDataPropertyValues: Map[api.ScalarDataPropertyValue, owlapi.descriptions.ScalarDataPropertyValue] = Map.empty,

 singletonInstanceStructuredDataPropertyContexts: Map[api.SingletonInstanceStructuredDataPropertyContext, owlapi.descriptions.SingletonInstanceStructuredDataPropertyContext] = Map.empty

) {
  def withBuiltIn
  (m0: api.Module, m1: MutableTboxOrDbox)
  (implicit ext: api.Extent)
  : core.OMFError.Throwables \/ OMLResolver2Ontology
  = (m0, m1) match {
    case (g0: api.TerminologyGraph, -\/(g1: owlapiterminologies.TerminologyGraph)) =>
      g0
        .moduleElements()
        .selectByKindOf { case d0: api.DataRange => d0 }
        .foldLeft[core.OMFError.Throwables \/ OMLResolver2Ontology](this.right) {
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

  def getTboxByIRI(iri: tables.IRI): core.OMFError.Throwables \/ owlapiterminologies.MutableTerminologyBox
  = (gs.values ++ bs.values)
    .find { case tbox: owlapiterminologies.MutableTerminologyBox =>
      tbox.iri.toString == iri
    } match {
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

  def lookupDbox(iri: tables.IRI): core.OMFError.Throwables \/ owlapidescriptions.MutableDescriptionBox
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

  def lookupConcept(c0: api.Concept)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.Concept
  = concepts.get(c0) match {
    case Some(c1) =>
      c1.right
    case None =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.lookupConcept(c=${c0.iri()}) failed")).left
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

  def entityLookup(e0: api.Entity)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.Entity
  = (aspects ++ concepts ++ reifiedRelationships).get(e0) match {
    case Some(e1) =>
      e1.right
    case None =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.entityLookup(e=${e0.iri()}) failed")).left
  }

  def entityRelationshipLookup(e0: api.EntityRelationship)(implicit ext: api.Extent): core.OMFError.Throwables \/ owlapi.types.terms.EntityRelationship
  = (reifiedRelationships ++ unreifiedRelationships).get(e0) match {
    case Some(e1) =>
      e1.right
    case None =>
      Set[java.lang.Throwable](new IllegalArgumentException(
        s"OMLResolver2Ontology.entityRelationshipLookup(e=${e0.iri()}) failed")).left
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

  val ops: owlapi.OWLAPIOMFOps = omfStore.ops

  protected def convertTerminologyBox(uuid: UUID, g0: api.TerminologyGraph)
  : core.OMFError.Throwables \/ (OMLResolver2Ontology, api.Module, OMLResolver2Ontology.MutableTboxOrDbox, IRI)
  = {
    val k = OMLResolver2Ontology.convertTerminologyKind(g0.kind)
    val i = IRI.create(g0.iri)
    for {
      g1 <- this.drc.lookupBuiltInModule(i)(this.ops) match {
        case Some(mbox: owlapiterminologies.MutableTerminologyGraph) =>
          mbox.right[core.OMFError.Throwables]
        case _ =>
          this.ops.makeTerminologyGraph(i, k)(this.omfStore)
      }
      _ <- if (g1.uuid == uuid && g0.uuid == uuid)
        ().right[core.OMFError.Throwables]
      else
        Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
          s"convertTerminologyBox(g0=${g0.iri}) UUID mismatch\n g0.uuid=${g0.uuid}\n g1.uuid=${g1.uuid}"
        )).left
      that = copy(modules = this.modules :+ g0, gs = this.gs + (g0 -> g1))
    } yield (that, g0, g1.left, i)
  }

  protected def convertBundle(uuid: UUID, b0: api.Bundle)
  : core.OMFError.Throwables \/ (OMLResolver2Ontology, api.Module, OMLResolver2Ontology.MutableTboxOrDbox, IRI)
  = {
    val k = OMLResolver2Ontology.convertTerminologyKind(b0.kind)
    val i = IRI.create(b0.iri)
    for {
      b1 <- this.ops.makeBundle(i, k)(this.omfStore)
      _ <- if (b1.uuid == uuid && b0.uuid == uuid)
        ().right[core.OMFError.Throwables]
      else
        Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
          s"convertBundle(b0=${b0.iri}) UUID mismatch\n b0.uuid=${b0.uuid}\n b1.uuid=${b1.uuid}"
        )).left
      that = copy(modules = this.modules :+ b0, bs = this.bs + (b0 -> b1))
    } yield (that, b0, b1.left, i)
  }

  protected def convertDescriptionBox(uuid: UUID, d0: api.DescriptionBox)
  : core.OMFError.Throwables \/ (OMLResolver2Ontology, api.Module, OMLResolver2Ontology.MutableTboxOrDbox, IRI)
  = {
    val k = OMLResolver2Ontology.convertDescriptionKind(d0.kind)
    val i = IRI.create(d0.iri)
    for {
      d1 <- this.ops.makeDescriptionBox(i, k)(this.omfStore)
      _ <- if (d1.uuid == uuid && d0.uuid == uuid)
        ().right[core.OMFError.Throwables]
      else
        Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
          s"convertDescriptionBox(d0=${d0.iri}) UUID mismatch\n d0.uuid=${d0.uuid}\n d1.uuid=${d1.uuid}"
        )).left
      that = copy(modules = this.modules :+ d0, ds = this.ds + (d0 -> d1))
    } yield (that, d0, d1.right, i)
  }

}