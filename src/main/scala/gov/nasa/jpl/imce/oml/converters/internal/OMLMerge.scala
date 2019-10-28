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

import ammonite.ops.Path

import frameless.syntax._
import frameless._

import gov.nasa.jpl.imce.oml.converters.ConversionCommand
import gov.nasa.jpl.imce.oml.frameless.{OMLSpecificationTypedDatasets, api}
import gov.nasa.jpl.imce.oml.frameless.OMLSpecificationTypedDatasets.tagInjection
import gov.nasa.jpl.imce.oml.tables.taggedTypes
import gov.nasa.jpl.omf.scala.core.OMFError

import org.apache.spark.sql.{SQLContext, SparkSession}

import scala.collection.immutable.{Seq, Set}
import scala.util.control.Exception.nonFatalCatch
import scala.StringContext
import scala.Predef.String
import scalaz._
import Scalaz._

object OMLMerge {

  /**
    * Verify that there are no merge inconsistencies preventing merging multiple
    * `OMLSpecificationTypedDatasets` by the union of their respective `TypedDataset` fields.
    *
    * Note that merge consistency is weaker than ontological consistency per the semantics of OWL2-DL + SWRL.
    * Merge consistency is defined the property that merging multiple OML data
    * yields a merged OML data that is in bijection with an equivalent representation in OWL2-DL + SWRL.
    *
    * @param omlTDS The location where each `OMLSpecificationTypedDatasets` was read from
    * @param spark
    * @param sqlContext
    * @return
    */
  def mergeOMLTypedDatasets
  (omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = nonFatalCatch[OMFError.Throwables \/ OMLSpecificationTypedDatasets]
    .withApply { t =>
      Set[java.lang.Throwable](t).left
    }
    .apply {

      for {
        m0 <- OMLSpecificationTypedDatasets.createEmptyOMLSpecificationTypedDatasets().right[OMFError.Throwables]

        m1 <- mergeOMLModules(m0, omlTDS)

        m2 <- mergeOMLAnnotationProperties(m1, omlTDS)

        m3 <- mergeOMLUnaryTermKinds(m2, omlTDS)

        m4 <- mergeOMLModuleEdges(m3, omlTDS)

        m5 <- mergeOMLScalarRestrictions(m4, omlTDS)

        m6 <- mergeOMLEntityRelationships(m5, omlTDS)

        m7 <- mergeOMLDataRelationships(m6, omlTDS)

        m8 <- mergeOMLChainRules(m7, omlTDS)

        m9 <- mergeOMLEntityRestictionAxioms(m8, omlTDS)

        m10 <- mergeOMLEntityScalarDataRestrictionAxioms(m9, omlTDS)

        m11 <- mergeOMLEntityStructuredDataRestrictionAxioms(m10, omlTDS)

        m12 <- mergeOMLCardinalityRestrictedEntities(m11, omlTDS)

        m13 <- mergeOMLSpecializationAxioms(m12, omlTDS)

        m14 <- mergeOMLSubPropertyAxioms(m13, omlTDS)

        m15 <- mergeOMLDisjunctionAxioms(m14, omlTDS)

        m16 <- mergeOMLInstances(m15, omlTDS)

        m17 <- mergeOMLInstanceValues(m16, omlTDS)

        m18 <- mergeOMLAnnotationPropertyValues(m17, omlTDS)

      } yield m18
    }

  // TODO Verify consistency among TerminologyBox.kind
  // TODO Verify that each IRI refers to a unique OML Module
  protected def mergeOMLModules
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) { case (mi, (_, omlTD)) =>
      mi.copy(
        terminologyGraphs = (mi.terminologyGraphs union omlTD.terminologyGraphs).distinct,
        bundles = (mi.bundles union omlTD.bundles).distinct,
        descriptionBoxes = (mi.descriptionBoxes union omlTD.descriptionBoxes).distinct
      )
    }

    updated.right
  }

  // Verifies merge consistency.
  protected def mergeOMLAnnotationProperties
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) {
      case (mi, (path, omlTD)) =>

        val problems = new scala.collection.mutable.ListBuffer[String]()

        val j1: TypedDataset[(api.AnnotationProperty, api.AnnotationProperty)]
        = mi.annotationProperties.join(omlTD.annotationProperties,
          mi.annotationProperties.col[taggedTypes.IRI]('iri),
          omlTD.annotationProperties.col[taggedTypes.IRI]('iri))

        val diff1: Seq[(api.AnnotationProperty, api.AnnotationProperty)]
        = j1.filter(j1.colMany('_1, 'moduleUUID) =!= j1.colMany('_2, 'moduleUUID)).collect().run().to[Seq]

        val diff2: Seq[(api.AnnotationProperty, api.AnnotationProperty)]
        = j1.filter(j1.colMany('_1, 'abbrevIRI) =!= j1.colMany('_2, 'abbrevIRI)).collect().run().to[Seq]

        if (diff1.nonEmpty)
          problems +=
            ConversionCommand.explainProblems(
              s"${diff1.size} differences! (same iri, different moduleUUID)",  diff1.map(_.toString))

        if (diff2.nonEmpty)
          problems +=
            ConversionCommand.explainProblems(
              s"${diff2.size} differences! (same iri, different abbrevIRI)", diff2.map(_.toString))

        val j2: TypedDataset[(api.AnnotationProperty, api.AnnotationProperty)]
        = mi.annotationProperties.join(omlTD.annotationProperties,
          mi.annotationProperties.col[taggedTypes.AbbrevIRI]('abbrevIRI),
          omlTD.annotationProperties.col[taggedTypes.AbbrevIRI]('abbrevIRI))

        val diff3: Seq[(api.AnnotationProperty, api.AnnotationProperty)]
        = j2.filter(j2.colMany('_1, 'moduleUUID) =!= j2.colMany('_2, 'moduleUUID)).collect().run().to[Seq]

        val diff4: Seq[(api.AnnotationProperty, api.AnnotationProperty)]
        = j2.filter(j2.colMany('_1, 'iri) =!= j2.colMany('_2, 'iri)).collect().run().to[Seq]

        if (diff3.nonEmpty)
          problems +=
            ConversionCommand.explainProblems(
              s"${diff3.size} differences! (same abbrevIRI, different moduleUUID)", diff3.map(_.toString))

        if (diff4.nonEmpty)
          problems +=
            ConversionCommand.explainProblems(
              s"${diff4.size} differences! (same abbrevIRI, different iri)", diff4.map(_.toString))

        if (problems.nonEmpty)
          throw new IllegalArgumentException(
            ConversionCommand.explainProblems(s"Problems found when merging $path", problems))

        mi.copy(annotationProperties = (mi.annotationProperties union omlTD.annotationProperties).distinct)

    }

    updated.right
  }

  // TODO Verify that within a tboxUUID, each name refers to a unique OML UnaryTermKind
  protected def mergeOMLUnaryTermKinds
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) { case (mi, (_, omlTD)) =>
      mi.copy(
        aspects = (mi.aspects union omlTD.aspects).distinct,
        concepts = (mi.concepts union omlTD.concepts).distinct,
        scalars = (mi.scalars union omlTD.scalars).distinct,
        structures = (mi.structures union omlTD.structures).distinct
      )
    }

    updated.right
  }

  // TODO Verify consistency among conceptDesignationTerminologyAxioms
  // TODO Verify consistency among terminologyNestingAxioms
  protected def mergeOMLModuleEdges
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) { case (mi, (_, omlTD)) =>
      mi.copy(
        conceptDesignationTerminologyAxioms =
          (mi.conceptDesignationTerminologyAxioms union omlTD.conceptDesignationTerminologyAxioms).distinct,
        terminologyExtensionAxioms =
          (mi.terminologyExtensionAxioms union omlTD.terminologyExtensionAxioms).distinct,
        terminologyNestingAxioms =
          (mi.terminologyNestingAxioms union omlTD.terminologyNestingAxioms).distinct,
        bundledTerminologyAxioms =
          (mi.bundledTerminologyAxioms union omlTD.bundledTerminologyAxioms).distinct,
        descriptionBoxExtendsClosedWorldDefinitions =
          (mi.descriptionBoxExtendsClosedWorldDefinitions union omlTD.descriptionBoxExtendsClosedWorldDefinitions).distinct,
        descriptionBoxRefinements =
          (mi.descriptionBoxRefinements union omlTD.descriptionBoxRefinements).distinct
      )
    }

    updated.right
  }

  // TODO Verify that for a given tboxUUID, each name refers to a unique OML ScalarRestriction
  protected def mergeOMLScalarRestrictions
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) { case (mi, (_, omlTD)) =>
      mi.copy(
        binaryScalarRestrictions =
          (mi.binaryScalarRestrictions union omlTD.binaryScalarRestrictions).distinct,
        iriScalarRestrictions =
          (mi.iriScalarRestrictions union omlTD.iriScalarRestrictions).distinct,
        numericScalarRestrictions =
          (mi.numericScalarRestrictions union omlTD.numericScalarRestrictions).distinct,
        plainLiteralScalarRestrictions =
          (mi.plainLiteralScalarRestrictions union omlTD.plainLiteralScalarRestrictions).distinct,
        scalarOneOfRestrictions =
          (mi.scalarOneOfRestrictions union omlTD.scalarOneOfRestrictions).distinct,
        scalarOneOfLiteralAxioms =
          (mi.scalarOneOfLiteralAxioms union omlTD.scalarOneOfLiteralAxioms).distinct,
        stringScalarRestrictions =
          (mi.stringScalarRestrictions union omlTD.stringScalarRestrictions).distinct,
        synonymScalarRestrictions =
          (mi.synonymScalarRestrictions union omlTD.synonymScalarRestrictions).distinct,
        timeScalarRestrictions =
          (mi.timeScalarRestrictions union omlTD.timeScalarRestrictions).distinct
      )
    }

    updated.right
  }

  // TODO Verify that within a tboxUUID, there is exactly one OML EntityRelationship for a given name
  // TODO Verify that for a reifiedRelationshipUUID, there is exactly one OML ForwardProperty
  // TODO Verify that for a reifiedRelationshipUUID, there is exactly one OML InverseProperty
  protected def mergeOMLEntityRelationships
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) { case (mi, (_, omlTD)) =>
      mi.copy(
        reifiedRelationships =
          (mi.reifiedRelationships union omlTD.reifiedRelationships).distinct,
        forwardProperties =
          (mi.forwardProperties union omlTD.forwardProperties).distinct,
        inverseProperties =
          (mi.inverseProperties union omlTD.inverseProperties).distinct,
        unreifiedRelationships =
          (mi.unreifiedRelationships union omlTD.unreifiedRelationships).distinct
      )
    }

    updated.right
  }

  // TODO Verify that within a tboxUUID, there is exactly one OML DataRelationship for a given name
  protected def mergeOMLDataRelationships
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) { case (mi, (_, omlTD)) =>
      mi.copy(
        entityScalarDataProperties =
          (mi.entityScalarDataProperties union omlTD.entityScalarDataProperties).distinct,
        entityStructuredDataProperties =
          (mi.entityStructuredDataProperties union omlTD.entityStructuredDataProperties).distinct,
        scalarDataProperties =
          (mi.scalarDataProperties union omlTD.scalarDataProperties).distinct,
        structuredDataProperties =
          (mi.structuredDataProperties union omlTD.structuredDataProperties).distinct
      )
    }

    updated.right
  }

  // TODO Verify that within a tboxUUID, there is exactly one OML ChainRule for a given name
  // TODO Verify that within a tboxUUID, there is exactly one OML ChainRule for a given pair of (name,headUUID)
  // TODO Verify that for a given bodSegmentUUID, there is only one OML SegmentPredicate
  protected def mergeOMLChainRules
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) { case (mi, (_, omlTD)) =>
      mi.copy(
        chainRules =
          (mi.chainRules union omlTD.chainRules).distinct,
        ruleBodySegments =
          (mi.ruleBodySegments union omlTD.ruleBodySegments).distinct,
        segmentPredicates =
          (mi.segmentPredicates union omlTD.segmentPredicates).distinct
      )
    }

    updated.right
  }

  // No merge consistency to verify because all characteristics are essential and mutually independent
  // (i.e., restrictedDomainUUID, restrictedRangeUUID, restrictedRelationshipUUID).
  protected def mergeOMLEntityRestictionAxioms
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) { case (mi, (_, omlTD)) =>
      mi.copy(
        entityExistentialRestrictionAxioms =
          (mi.entityExistentialRestrictionAxioms union omlTD.entityExistentialRestrictionAxioms).distinct,
        entityUniversalRestrictionAxioms =
          (mi.entityUniversalRestrictionAxioms union omlTD.entityUniversalRestrictionAxioms).distinct
      )
    }

    updated.right
  }

  // No merge consistency to verify because all characteristics are essential and mutually independent
  // (i.e., restrictedEntityUUID, scalarPropertyUUID, scalarRestrictionUUID).
  protected def mergeOMLEntityScalarDataRestrictionAxioms
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) { case (mi, (_, omlTD)) =>
      mi.copy(
        entityScalarDataPropertyExistentialRestrictionAxioms =
          (mi.entityScalarDataPropertyExistentialRestrictionAxioms union omlTD.entityScalarDataPropertyExistentialRestrictionAxioms).distinct,
        entityScalarDataPropertyParticularRestrictionAxioms =
          (mi.entityScalarDataPropertyParticularRestrictionAxioms union omlTD.entityScalarDataPropertyParticularRestrictionAxioms).distinct,
        entityScalarDataPropertyUniversalRestrictionAxioms =
          (mi.entityScalarDataPropertyUniversalRestrictionAxioms union omlTD.entityScalarDataPropertyUniversalRestrictionAxioms).distinct
      )
    }

    updated.right
  }

  // No merge consistency to verify because all characteristics are essential and mutually independent.
  protected def mergeOMLEntityStructuredDataRestrictionAxioms
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) { case (mi, (_, omlTD)) =>
      mi.copy(
        entityStructuredDataPropertyParticularRestrictionAxioms =
          (mi.entityStructuredDataPropertyParticularRestrictionAxioms union omlTD.entityStructuredDataPropertyParticularRestrictionAxioms).distinct,
        restrictionStructuredDataPropertyTuples =
          (mi.restrictionStructuredDataPropertyTuples union omlTD.restrictionStructuredDataPropertyTuples).distinct,
        restrictionScalarDataPropertyValues =
          (mi.restrictionScalarDataPropertyValues union omlTD.restrictionScalarDataPropertyValues).distinct
      )
    }

    updated.right
  }

  // TODO: Verify that within a given TboxUUID, there is only one cardinality restriction for a given entityUUID
  protected def mergeOMLCardinalityRestrictedEntities
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) { case (mi, (_, omlTD)) =>
      mi.copy(
        cardinalityRestrictedAspects =
          (mi.cardinalityRestrictedAspects union omlTD.cardinalityRestrictedAspects).distinct,
        cardinalityRestrictedConcepts =
          (mi.cardinalityRestrictedConcepts union omlTD.cardinalityRestrictedConcepts).distinct,
        cardinalityRestrictedReifiedRelationships =
          (mi.cardinalityRestrictedReifiedRelationships union omlTD.cardinalityRestrictedReifiedRelationships).distinct
      )
    }

    updated.right
  }

  // No merge consistency to verify because all characteristics are essential and mutually independent.
  protected def mergeOMLSpecializationAxioms
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) { case (mi, (_, omlTD)) =>
      mi.copy(
        aspectSpecializationAxioms =
          (mi.aspectSpecializationAxioms union omlTD.aspectSpecializationAxioms).distinct,
        conceptSpecializationAxioms =
          (mi.conceptSpecializationAxioms union omlTD.conceptSpecializationAxioms).distinct,
        reifiedRelationshipSpecializationAxioms =
          (mi.reifiedRelationshipSpecializationAxioms union omlTD.reifiedRelationshipSpecializationAxioms).distinct
      )
    }

    updated.right
  }

  // No merge consistency to verify because all characteristics are essential and mutually independent.
  protected def mergeOMLSubPropertyAxioms
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) { case (mi, (_, omlTD)) =>
      mi.copy(
        subDataPropertyOfAxioms =
          (mi.subDataPropertyOfAxioms union omlTD.subDataPropertyOfAxioms).distinct,
        subObjectPropertyOfAxioms =
          (mi.subObjectPropertyOfAxioms union omlTD.subObjectPropertyOfAxioms).distinct
      )
    }

    updated.right
  }

  // No merge consistency to verify because all characteristics are essential and mutually independent.
  protected def mergeOMLDisjunctionAxioms
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) { case (mi, (_, omlTD)) =>
      mi.copy(
        rootConceptTaxonomyAxioms =
          (mi.rootConceptTaxonomyAxioms union omlTD.rootConceptTaxonomyAxioms).distinct,
        anonymousConceptUnionAxioms =
          (mi.anonymousConceptUnionAxioms union omlTD.anonymousConceptUnionAxioms).distinct,
        specificDisjointConceptAxioms =
          (mi.specificDisjointConceptAxioms union omlTD.specificDisjointConceptAxioms).distinct
      )
    }

    updated.right
  }

  // TODO Verify the merge consistency of reifiedRelationshipInstanceDomains and reifiedRelationshipInstanceRanges
  protected def mergeOMLInstances
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) { case (mi, (_, omlTD)) =>
      mi.copy(
        conceptInstances =
          (mi.conceptInstances union omlTD.conceptInstances).distinct,
        reifiedRelationshipInstances =
          (mi.reifiedRelationshipInstances union omlTD.reifiedRelationshipInstances).distinct,
        reifiedRelationshipInstanceDomains =
          (mi.reifiedRelationshipInstanceDomains union omlTD.reifiedRelationshipInstanceDomains).distinct,
        reifiedRelationshipInstanceRanges =
          (mi.reifiedRelationshipInstanceRanges union omlTD.reifiedRelationshipInstanceRanges).distinct,
        unreifiedRelationshipInstanceTuples =
          (mi.unreifiedRelationshipInstanceTuples union omlTD.unreifiedRelationshipInstanceTuples).distinct
      )
    }

    updated.right
  }

  // No merge consistency to verify because all characteristics are essential and mutually independent.
  protected def mergeOMLInstanceValues
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) { case (mi, (_, omlTD)) =>
      mi.copy(
        singletonInstanceStructuredDataPropertyValues =
          (mi.singletonInstanceStructuredDataPropertyValues union omlTD.singletonInstanceStructuredDataPropertyValues).distinct,
        singletonInstanceScalarDataPropertyValues =
          (mi.singletonInstanceScalarDataPropertyValues union omlTD.singletonInstanceScalarDataPropertyValues).distinct,
        structuredDataPropertyTuples =
          (mi.structuredDataPropertyTuples union omlTD.structuredDataPropertyTuples).distinct,
        scalarDataPropertyValues =
          (mi.scalarDataPropertyValues union omlTD.scalarDataPropertyValues).distinct
      )
    }

    updated.right
  }

  // No merge consistency to verify because all characteristics are essential and mutually independent.
  protected def mergeOMLAnnotationPropertyValues
  (current: OMLSpecificationTypedDatasets,
   omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = {
    val updated = omlTDS.foldLeft(current) { case (mi, (_, omlTD)) =>
      mi.copy(
        annotationPropertyValues =
          (mi.annotationPropertyValues union omlTD.annotationPropertyValues).distinct
      )
    }

    updated.right
  }

}
