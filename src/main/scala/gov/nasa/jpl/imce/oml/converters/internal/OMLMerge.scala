package gov.nasa.jpl.imce.oml.converters.internal

import ammonite.ops.Path
import frameless.TypedDataset
import gov.nasa.jpl.imce.oml.covariantTag
import gov.nasa.jpl.imce.oml.covariantTag.@@
import gov.nasa.jpl.imce.oml.frameless.{api,OMLSpecificationTypedDatasets}
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.omf.scala.core.OMFError
import org.apache.spark.sql.{SQLContext, SparkSession}

import scala.collection.immutable.{Seq, Set}
import scala.util.control.Exception.nonFatalCatch
import scalaz._
import Scalaz._

object OMLMerge {

  trait ModulePathTag
  type ModulePath = String @@ ModulePathTag

  type AnnotationPropertyModuleTuple
  = (ModulePath,
    tables.taggedTypes.AnnotationPropertyUUID,
    tables.taggedTypes.ModuleUUID,
    tables.taggedTypes.IRI,
    tables.taggedTypes.AbbrevIRI)

  def mergeOMLTypedDatasets
  (omlTDS: Seq[(Path, OMLSpecificationTypedDatasets)])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ OMLSpecificationTypedDatasets
  = nonFatalCatch[OMFError.Throwables \/ OMLSpecificationTypedDatasets]
    .withApply { t =>
      Set[java.lang.Throwable](t).left
    }
    .apply {

      val m0 = OMLSpecificationTypedDatasets.createEmptyOMLSpecificationTypedDatasets()

      // Merge of OML Modules:
      // By definition, each OML Module is identified by a unique IRI.

      val m1 = omlTDS.foldLeft(m0) { case (mi, (_, omlTD)) =>
        mi.copy(
          terminologyGraphs = mi.terminologyGraphs union omlTD.terminologyGraphs,
          bundles = mi.bundles union omlTD.bundles,
          descriptionBoxes = mi.descriptionBoxes union omlTD.descriptionBoxes
        )
      }

      // Merge of OML AnnotationProperties

//      val aps
//      : TypedDataset[AnnotationPropertyModuleTuple]
//      = TypedDataset.create[AnnotationPropertyModuleTuple](
//        omlTDS.map { case (p, omlTD) =>
//          val mp = covariantTag[ModulePathTag][String](p.toString())
//
//          omlTD
//            .annotationProperties
//            .select(
//              omlTD.annotationProperties('uuid),
//              omlTD.annotationProperties('moduleUUID),
//              omlTD.annotationProperties('iri),
//              omlTD.annotationProperties('abbrevIRI))
//            .toDF()
//              .wi
//
//
//          })

      m1.right
    }
}
