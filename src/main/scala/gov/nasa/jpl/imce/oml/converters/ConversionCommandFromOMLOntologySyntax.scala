package gov.nasa.jpl.imce.oml.converters

import java.io.File
import java.lang.{IllegalArgumentException, System}
import java.nio.file.Paths

import gov.nasa.jpl.imce.oml.converters.utils.OMLResourceSet
import gov.nasa.jpl.imce.oml.resolver
import gov.nasa.jpl.imce.oml.resolver.impl.OMLResolvedFactoryImpl
import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import gov.nasa.jpl.imce.oml.uuid.JVMUUIDGenerator
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.binding.owlapi.common.ImmutableModule
import gov.nasa.jpl.omf.scala.binding.owlapi.descriptions.ImmutableDescriptionBox
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{ImmutableBundle, ImmutableTerminologyBox, ImmutableTerminologyGraph}
import gov.nasa.jpl.omf.scala.core.{OMFError}
import gov.nasa.jpl.omf.scala.core.tables.OMFTabularExport
import org.apache.xml.resolver.{Catalog, CatalogManager}
import org.apache.xml.resolver.tools.CatalogResolver
import org.eclipse.emf.common.util.{URI => EURI}
import org.semanticweb.owlapi.apibinding.OWLManager

import scala.collection.immutable.{Seq, Set, Vector}
import scala.util.control.Exception.nonFatalCatch
import scala.{Boolean, Int, None, Option, Ordering, Some, StringContext, Unit}
import scala.Predef.ArrowAssoc
import scalaz._
import Scalaz._
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc

case object ConversionCommandFromOMLOntologySyntax extends ConversionCommand {

  override def filePredicate(f: File): Boolean = f.isFile && f.getName.endsWith(".owl")

  def createOMFStoreAndLoadCatalog(catalogFile: File)
  : OMFError.Throwables \/ (OWLAPIOMFGraphStore, Catalog)
  = nonFatalCatch[OMFError.Throwables \/ (OWLAPIOMFGraphStore, Catalog)]
    .withApply {
      t: java.lang.Throwable =>
        -\/(Set[java.lang.Throwable](t))
    }
    .apply {
      val cm = new CatalogManager()
      cm.setUseStaticCatalog(false)
      val cr = new CatalogResolver(cm)
      val cat = cm.getPrivateCatalog

      val omfStore: OWLAPIOMFGraphStore
      = OWLAPIOMFGraphStore.initGraphStore(
        OWLAPIOMFModule
          .owlAPIOMFModule(cm, withOMFMetadata = false)
          .valueOr { (errors: Set[java.lang.Throwable]) =>
            val message = s"${errors.size} errors" + errors
              .map(_.getMessage)
              .toList
              .mkString("\n => ", "\n => ", "\n")
            throw new IllegalArgumentException(message)
          },
        OWLManager.createOWLOntologyManager(),
        cr,
        cat
      )

      omfStore.catalogIRIMapper
        .parseCatalog(catalogFile.toURI)
        .valueOr { (errors: Set[java.lang.Throwable]) =>
          val message = s"${errors.size} errors" + errors
            .map(_.getMessage)
            .toList
            .mkString("\n => ", "\n => ", "\n")
          throw new IllegalArgumentException(message)
        }

      \/-(omfStore -> cat)
    }

  override def convert(inCatalog: File, inputFiles: Vector[File], outputDir: File, outCatalog: File)
  : OMFError.Throwables \/ Unit
  = for {
    in_store_cat <- createOMFStoreAndLoadCatalog(inCatalog)
    (inStore, inCat) = in_store_cat
    out_store_cat <- createOMFStoreAndLoadCatalog(outCatalog)
    (outStore, outCat) = out_store_cat
    result <- convert(inStore, inCat, inputFiles, outputDir, outStore, outCat)
  } yield result

  def convert
  (inStore: OWLAPIOMFGraphStore,
   inCat: Catalog,
   inputFiles: Vector[File],
   outputDir: File,
   outStore: OWLAPIOMFGraphStore,
   outCat: Catalog)
  : OMFError.Throwables \/ Unit
  = {
    implicit val mOrder = new Ordering[OWLAPIOMF#ImmutableModule] {
      override def compare(x: ImmutableModule, y: ImmutableModule): Int = x.iri.toString.compareTo(y.iri.toString)
    }

    val result
    : OMFError.Throwables \/ Unit
    = for {
      m2i <- inputFiles.foldLeft {
        emptyMutable2ImmutableModuleMap.right[OMFError.Throwables]
      } { case (acc, inputFile) =>
        for {
          prev <- acc
          loadResult <- inStore.loadModule(prev, inputFile)
          (_, next) = loadResult
        } yield next
      }

      is = m2i.values

      in_drc = inStore.getBuildInDatatypeMap

      builtInEdges = in_drc.builtInDatatypeModules.flatMap { f =>
        val i: Option[OWLAPIOMF#ImmutableModule] = f match {
          case fI: OWLAPIOMF#ImmutableModule =>
            Some(fI)
          case fM: OWLAPIOMF#MutableModule =>
            m2i.get(fM) match {
              case Some(fI: OWLAPIOMF#ImmutableModule) =>
                Some(fI)
              case _ =>
                None
            }
        }

        i match {
          case Some(fI) =>
            fI.sig.importedTerminologies(inStore.ops).flatMap {
              case tI: OWLAPIOMF#ImmutableModule =>
                Some(fI -> tI)
              case tM: OWLAPIOMF#MutableModule =>
                m2i.get(tM) match {
                  case Some(tI: OWLAPIOMF#ImmutableModule) =>
                    Some(fI -> tI)
                  case _ =>
                    None
                }
            }
          case None =>
            None
        }
      }

      g0 = Graph[OWLAPIOMF#ImmutableModule, DiEdge](is: _*)

      g1 = (g0 /: builtInEdges) { case (gi, (fI, tI)) =>
        System.out.println(s"convert from OWL(builtin) ${fI.iri} ~> ${tI.iri}")
        gi + fI ~> tI
      }

      g2 = m2i.values.find { _.iri == OWLAPIOMFLoader.omlIRI } match {
        case Some(iOML) =>
          (g1 /: builtInEdges.map(_._1)) { case (gi, fI) =>
            System.out.println(s"convert from OWL(oml) ${iOML.iri} ~> ${fI.iri}")
            gi + iOML ~> fI
          }
        case None =>
          g1
      }

      g3 = (g2 /: is) { case (gi, i) =>
        val gk = (gi /: i.sig.importedTerminologies(inStore.ops)) {
          case (gj, itbox: OWLAPIOMF#ImmutableTerminologyBox) =>
            System.out.println(s"convert from OWL(tbox) ${i.iri} ~> ${itbox.iri}")
            gj + i ~> itbox
          case (gj, _) =>
            gj
        }
        val gl = (gk /: i.sig.importedDescriptions(inStore.ops)) {
          case (gh, idbox: OWLAPIOMF#ImmutableDescriptionBox) =>
            System.out.println(s"convert from OWL(dbox) ${i.iri} ~> ${idbox.iri}")
            gh + i ~> idbox
          case (gh, _) =>
            gh
        }
        gl
      }

      gorder <- OWLAPIOMFLoader.hierarchicalTopologicalSort(Seq(g3), Seq.empty).map(_.reverse)

      _ = gorder.foreach { g =>
        val iri = g.iri.toString
        val omlIRI = if (iri.endsWith("/"))
          iri.replaceFirst("^(.*)/([a-zA-Z0-9.]+)/$", "$1/$2/$2.oml")
        else
          iri + ".oml"
        System.out.println(s"convert from OWL(gorder): ${g.iri} => $omlIRI")
      }

      tables <- OMFTabularExport.toTables[OWLAPIOMF](gorder)(inStore, inStore.ops)

      _ = {
        (gorder zip tables).foreach { case (gi, (im, ti)) =>
          if (gi.iri != im.iri) {
            System.out.println(s"convert from OWL(tables)  gi=${gi.iri}")
            System.out.println(s"convert from OWL(tables)  im=${im.iri}")
            System.out.println(s"convert from OWL(tables): mismatch!")
          }
        }
      }

      _ <- tables.foldLeft {
        ().right[OMFError.Throwables]
      } { case (acc, (im, table)) =>

        for {
          _ <- acc
          tablesURI = outStore.catalogIRIMapper.resolveIRI(im.iri, saveResolutionStrategyForOMLTables).toURI
          tablesFile = Paths.get(tablesURI).toFile

          _ <- OMLSpecificationTables
            .saveOMLSpecificationTables(table, tablesFile)
            .toDisjunction
            .leftMap(Set[java.lang.Throwable](_))

          _ = System.out.println(s"Saved oml.tables in: $tablesFile")

        } yield ()
      }

      omlUUIDg = JVMUUIDGenerator()

      factory = OMLResolvedFactoryImpl(omlUUIDg)

      resolved <- tables.foldLeft {
        resolver.OMLTablesResolver.initializeTablesResolver(factory).right[OMFError.Throwables]
      } { case (acc, (im, table)) =>

        for {
          prev <- acc
          current = prev.copy(queue = table)

          _ = System.out.println(s"OMLTablesResolver: ${im.iri}")

          res <- resolver.OMLTablesResolver.resolve(current)
            .toDisjunction
            .leftMap(Set[java.lang.Throwable](_))

          extent = res.context

          _ <- if (!res.queue.isEmpty)
            Set[java.lang.Throwable](OMFError.omfError(
              s"Conversion of ${im.iri} incomplete:\n"+res.queue.show
            )).left[Unit]
          else
            ().right[OMFError.Throwables]

          nB = extent.bundles.size
          nG = extent.terminologyGraphs.size
          nD = extent.descriptionBoxes.size

          toBundle = nB == 1 && nG == 0 && nD == 0
          toGraph = nB == 0 && nG == 1 && nD == 0
          toDescription = nB == 0 && nG == 0 && nD == 1

          _ <- im match {
            case _: ImmutableBundle =>
              if (toBundle)
                ().right[OMFError.Throwables]
              else
                Set[java.lang.Throwable](OMFError.omfError(
                  s"Bundle conversion of ${im.iri} incomplete"
                )).left[Unit]

            case _: ImmutableTerminologyGraph =>
              if (toGraph)
                ().right[OMFError.Throwables]
              else
                Set[java.lang.Throwable](OMFError.omfError(
                  s"TerminologyGraph conversion of ${im.iri} incomplete"
                )).left[Unit]


            case _: ImmutableDescriptionBox =>
              if (toDescription)
                ().right[OMFError.Throwables]
              else
                Set[java.lang.Throwable](OMFError.omfError(
                  s"DescriptionBox conversion of ${im.iri} incomplete"
                )).left[Unit]


          }

          next = resolver.OMLTablesResolver.accumulateResultContext(res)
        } yield next
      }

      extents = resolved.otherContexts

      rs = OMLResourceSet.initializeResourceSet()

      r2t <- extents.foldLeft {
        OMLResolver2Text().right[OMFError.Throwables]
      } { case (acc, apiExtent) =>
        for {
          prev <- acc
          next <- OMLResolver2Text.convert(apiExtent, rs, prev).leftMap(_.toThrowables)
        } yield next
      }

      extentResources = {
        r2t.mappings.map { case (iri, (_, omlExtent)) =>

          val omlIRI = if (iri.endsWith("/"))
            iri.replaceFirst("^(.*)/([a-zA-Z0-9.]+)/$","$1/$2.oml")
          else
            iri + ".oml"
          val resolvedIRI = outCat.resolveURI(omlIRI)

          System.out.println(s" OML IRI = $iri")
          System.out.println(s"resolved = $resolvedIRI")

          val uri: EURI = EURI.createURI(resolvedIRI)
          val r = rs.createResource(uri)
          r.getContents.add(omlExtent)
          r
        }
      }

      _ <- (().right[OMFError.Throwables] /: extentResources) { case (acc, r) =>
        for {
          _ <- acc
          _ <- nonFatalCatch[OMFError.Throwables \/ Unit]
            .withApply { (t: java.lang.Throwable) =>
              System.err.println(
                s"OMLConverterFromOntologySyntax (Error while saving to OML): ${t.getMessage}")
              t.printStackTrace(System.err)
              Set(t).left[Unit]
            }
            .apply {
              r.save(null)
              System.out.println(s"Saved ${r.getURI}")
              ().right[OMFError.Throwables]
            }
        } yield ()
      }

      out_drc <- outStore.loadBuiltinDatatypeMap()

      r2o <- extents.foldLeft {
        OMLResolver2Ontology(out_drc, outStore).right[OMFError.Throwables]
      } { case (acc, apiExtent) =>
        for {
          prev <- acc
          next <- OMLResolver2Ontology.convert(apiExtent, prev)
        } yield next
      }

      tboxConversions <-
      r2o
        .modules
        .foldLeft[OMFError.Throwables \/ (Seq[ImmutableTerminologyBox], Mutable2ImmutableModuleMap)] {
        (Seq.empty[ImmutableTerminologyBox], emptyMutable2ImmutableModuleMap).right
      } {
        case (acc, m0: resolver.api.TerminologyBox) =>
          for {
            prev <- acc
            (convs, m2i) = prev
            m1 <- r2o.getTbox(m0)
            _ = System.out.println(s"... Converting terminology ${m1.sig.kind}: ${m1.iri}")
            next <- r2o.ops.asImmutableTerminologyBox(m1, m2i)(outStore).map { case (i1, m2iWithConv) =>
              (convs :+ i1) -> m2iWithConv
            }
          } yield next
        case (acc, _) =>
          acc
      }

      (tboxConvs, m2iTboxConv) = tboxConversions

      _ <- tboxConvs.foldLeft[OMFError.Throwables \/ Unit](().right[OMFError.Throwables]) {
        case (acc, itbox) =>
          for {
            _ <- acc
            _ = System.out.println(s"... Saving terminology ${itbox.iri}")
            _ <- r2o.ops.saveTerminology(itbox)(outStore)
          } yield ()
      }

      dboxConversions <-
      r2o
        .modules
        .foldLeft[OMFError.Throwables \/ (Seq[ImmutableDescriptionBox], Mutable2ImmutableModuleMap)] {
        (Seq.empty[ImmutableDescriptionBox], m2iTboxConv).right
      } {
        case (acc, m0: resolver.api.DescriptionBox) =>
          for {
            prev <- acc
            (convs, m2i) = prev
            m1 <- r2o.getDbox(m0)
            _ = System.out.println(s"... Converting description ${m1.sig.kind}: ${m1.iri}")
            next <- r2o.ops.asImmutableDescription(m1, m2i)(outStore).map { case (i1, m2iWithConv) =>
              (convs :+ i1) -> m2iWithConv
            }
          } yield next
        case (acc, _) =>
          acc
      }

      (dboxConvs, _) = dboxConversions

      _ <- dboxConvs.foldLeft[OMFError.Throwables \/ Unit](().right[OMFError.Throwables]) {
        case (acc, idbox) =>
          for {
            _ <- acc
            _ = System.out.println(s"... Saving description ${idbox.iri}")
            _ <- r2o.ops.saveDescriptionBox(idbox)(outStore)
          } yield ()
      }

    } yield ()

    result
  }

}