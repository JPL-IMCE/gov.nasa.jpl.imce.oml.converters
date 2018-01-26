package gov.nasa.jpl.imce.oml.converters

import java.io.PrintStream
import java.lang.System
import java.util.Properties

import ammonite.ops.Path
import gov.nasa.jpl.imce.oml.covariantTag.@@
import gov.nasa.jpl.imce.oml.frameless.OMLSpecificationTypedDatasets
import gov.nasa.jpl.imce.oml.model.extensions.OMLCatalog
import gov.nasa.jpl.imce.oml.resolver.{GraphUtilities, ResolverUtilities, TableUtilities}
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import gov.nasa.jpl.omf.scala.core.OMFError
import org.apache.spark.sql.{SQLContext, SparkSession}

import scala.collection.immutable.{Seq, Set}
import scala.{Boolean, Int, None, Option, Ordering, Some, StringContext, Unit}
import scala.Predef.{ArrowAssoc, String}
import scalaz._
import Scalaz._
import scala.util.{Failure, Success}
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc

object ConversionCommandFromOMLMerge {

  implicit def covariantOrdering[Tag]: Ordering[String @@ Tag] = new Ordering[String @@ Tag] {

    override def compare(x: @@[String, Tag], y: @@[String, Tag])
    : Int
    = x.compareTo(y)

  }

  def summarize(t: tables.OMLSpecificationTables)
  : Unit
  = {
    System.out.println(s"# ${t.terminologyGraphs.size} terminology graphs")
    System.out.println(s"# ${t.bundles.size} bundles")
    System.out.println(s"# ${t.descriptionBoxes.size} description graphs")
    System.out.println()
    System.out.println(s"# ${t.annotationProperties.size} annotation properties")
    System.out.println()
    System.out.println(s"# ${t.aspects.size} aspects")
    System.out.println(s"# ${t.concepts.size} concepts")
  }

  def merge
  (m: ConversionCommand.MergeCatalogs,
   conversions: ConversionCommand.OutputConversions,
   deleteOutputIfExists: Boolean,
   outputFolder: Path,
   verboseFiles: Option[PrintStream])
  (implicit spark: SparkSession, sqlContext: SQLContext)
  : OMFError.Throwables \/ (OMLCatalog, Seq[(tables.taggedTypes.IRI, OMLSpecificationTables)])
  = {
    val props = new Properties()
    props.setProperty("useSSL", "false")

    props.setProperty("dumpQueriesOnException", "true")
    props.setProperty("enablePacketDebug", "true")

    for {
      outCatalog <- internal.makeOutputCatalog(deleteOutputIfExists, outputFolder)

      out_store_cat <- ConversionCommand.createOMFStoreAndLoadCatalog(outCatalog)
      (outStore, outCat) = out_store_cat

      omlTDS <- m.folders.foldLeft[OMFError.Throwables \/ Seq[(Path, OMLSpecificationTypedDatasets)]] {
        Seq.empty[(Path, OMLSpecificationTypedDatasets)].right
      } { case (acc, folder) =>
        for {
          prev <- acc
          fTables <- OMLSpecificationTypedDatasets.parquetReadOMLSpecificationTables(folder) match {
            case Success(tables) =>
              \/-(tables)
            case Failure(t) =>
              -\/(Set(t))
          }

          _ = System.out.println(s"\n# Read from: $folder")
          _ = summarize(fTables)

          omlTD = OMLSpecificationTypedDatasets.convertToOMLSpecificationTypedDatasets(fTables)
        } yield prev :+ (folder -> omlTD)
      }

      omlMergedTDS <- internal.OMLMerge.mergeOMLTypedDatasets(omlTDS)

      omlTables = OMLSpecificationTypedDatasets.extractFromOMLSpecificationTypedDatasets(omlMergedTDS)

      _ = System.out.println(s"\n# Merged tables:")
      _ = summarize(omlTables)

      allModules = TableUtilities.partitionModules(omlTables)

      _ <- if (conversions.toOMLZip)
        tables
          .OMLSpecificationTables
          .saveOMLSpecificationTables(omlTables, (outputFolder / "aggregate.omlzip").toIO) match {
          case Success(_) =>
            allModules.foldLeft[OMFError.Throwables \/ Unit](\/-(())) {
              case (acc, (iri, ts)) =>
                for {
                  _ <- acc
                  outputFile <- internal
                    .resolveOutputCatalogFileWithExtension(outCat, iri, ".omlzip")

                  _ <- tables
                    .OMLSpecificationTables
                    .saveOMLSpecificationTables(ts, outputFile.toIO) match {
                    case Success(_) =>
                      System.out.println(s"... saved tables: $iri => $outputFile")
                      \/-(())
                    case Failure(t) =>
                      -\/(Set[java.lang.Throwable](t))
                  }
                } yield ()
            }
          case Failure(t) =>
            -\/(Set(t))
        }
      else
        \/-(())

      _ <- conversions.toSQL match {
        case Some(url) =>
          OMLSpecificationTypedDatasets
            .sqlWriteOMLSpecificationTables(omlTables, url, props) match {
            case Success(_) =>
              \/-(())
            case Failure(t) =>
              -\/(Set(t))
          }
        case None =>
          \/-(())
      }

      g0 = Graph[tables.taggedTypes.IRI, DiEdge](allModules.keys.toSeq: _*)

      g1 = (g0 /: allModules) { case (gi, (_, ti)) =>
        val gj = gi ++ TableUtilities.tableEdges(ti).map { case (src, dst) => src ~> dst }
        gj
      }

      gorder <- GraphUtilities.hierarchicalTopologicalSort(Seq(g1)).map(_.reverse)

      ts = gorder.map(iri => iri -> allModules(iri))

      extents <- ResolverUtilities.resolveTables(ResolverUtilities.initializeResolver(), ts)

      _ <- if (conversions.toText)
        internal
          .toText(outCatalog, extents)
          .leftMap(_.toThrowables)
      else
        \/-(())

      _ <- if (conversions.toOWL)
        internal
          .OMLResolver2Ontology
          .convert(extents, outStore)
      else
        \/-(())

    } yield outCat -> ts
  }

}
