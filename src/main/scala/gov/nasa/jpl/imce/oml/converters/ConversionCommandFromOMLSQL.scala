package gov.nasa.jpl.imce.oml.converters

import java.lang.System
import java.util.Properties

import ammonite.ops.Path
import gov.nasa.jpl.imce.oml.covariantTag.@@
import gov.nasa.jpl.imce.oml.frameless.OMLSpecificationTypedDatasets
import gov.nasa.jpl.imce.oml.resolver.GraphUtilities
import gov.nasa.jpl.imce.oml.resolver.ResolverUtilities
import gov.nasa.jpl.imce.oml.resolver.TableUtilities
import gov.nasa.jpl.imce.oml.tables.{OMLSpecificationTables, taggedTypes}
import org.apache.spark.sql.SparkSession

import scala.{sys, Int, None, Ordering, Some, StringContext, Unit}
import scala.collection.immutable.{Seq, Set}
import scala.util.{Failure, Success}
import scala.Predef.{ArrowAssoc, String}
import scalaz._
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc

object ConversionCommandFromOMLSQL {

  implicit def covariantOrdering[Tag]: Ordering[String @@ Tag] = new Ordering[String @@ Tag] {

    override def compare(x: @@[String, Tag], y: @@[String, Tag])
    : Int
    = x.compareTo(y)

  }

  def sqlInputConversion
  (s: ConversionCommand.SQLInputConversionWithServer,
   options: OMLConverter.Options,
   outCatalog: Path)
  : Unit
  = {
    val conf = internal.sparkConf(this.getClass.getSimpleName)

    val conversions: ConversionCommand.OutputConversions = options.output

    implicit val spark = SparkSession
      .builder()
      .config(conf)
      .getOrCreate()
    implicit val sqlContext = spark.sqlContext

    val props = new Properties()
    props.setProperty("useSSL", "false")

    props.setProperty("dumpQueriesOnException", "true")
    props.setProperty("enablePacketDebug", "true")

    val ok = for {

      out_store_cat <- ConversionCommand.createOMFStoreAndLoadCatalog(outCatalog)
      (outStore, outCat) = out_store_cat

      // 1) Read OML Tables from SQL

      omlTables <- OMLSpecificationTypedDatasets.sqlReadOMLSpecificationTables(s.server, props) match {
        case Success(ts) =>
          \/-(ts)
        case Failure(t) =>
          -\/(Set(t))
      }

      allModules = TableUtilities.partitionModules(omlTables)

      g0 = Graph[taggedTypes.IRI, DiEdge](allModules.keys.toSeq: _*)

      g1 = g0 ++ TableUtilities.tableEdges(omlTables).map { case (src, dst) => src ~> dst }

      gorder <- GraphUtilities.hierarchicalTopologicalSort(Seq(g1)).map(_.reverse)

      ts = gorder.map(iri => iri -> allModules(iri))

      // List of module IRIs

      _ <- conversions.modules match {
        case Some(file) =>
          internal
            .writeModuleIRIs(ts.map { case (iri, _) => iri }, file)

        case None =>
          \/-(())
      }

      // 2) Convert from OML Tables => OML Resolver

      extents <- ResolverUtilities.resolveTables(
        ResolverUtilities.initializeResolver(),
        ts)

      // 3) Convert from OML Resolver => OML Textual Concrete Syntax

      _ <- if (conversions.toText)
        internal
          .toText(outCatalog, extents)
          .leftMap(_.toThrowables)
      else
        \/-(())

      // 4) Convert from OML Resolver => OMF/OWLAPI again

      _ <- if (conversions.toOWL) {
        for {
          _ <- internal
            .OMLResolver2Ontology
            .convert(extents, outStore, options.output.modules)
          _ <- conversions.fuseki match {
            case None =>
              \/-(())
            case Some(fuseki) =>
              internal.tdbUpload(outCatalog, fuseki)
          }
        } yield ()
      } else
        \/-(())

      // 5) Convert from OML Tables => SQL

      _ <- conversions.toSQL match {
        case Some(server) =>
          val tables = ts.map(_._2).reduceLeft(OMLSpecificationTables.mergeTables)
          OMLSpecificationTypedDatasets
            .sqlWriteOMLSpecificationTables(tables, server, props) match {
            case Success(_) =>
              \/-(())
            case Failure(t) =>
              -\/(Set(t))
          }
        case None =>
          \/-(())
      }

    } yield ()

    ok match {
      case \/-(_) =>
        ()
      case -\/(ts) =>
        System.err.println(s"### ${ts.size} Conversion Errors! ###")
        ts.foreach { t =>
          System.err.println(t.getMessage)
          t.printStackTrace(System.err)
        }
        sys.exit(-1)
    }
  }

}
