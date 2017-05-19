package gov.nasa.jpl.imce.oml.converters

import java.io.File
import java.lang.System
import java.nio.file.Paths

import gov.nasa.jpl.imce.oml.converters.text2resolver.OMLText2Resolver
import gov.nasa.jpl.imce.oml.converters.utils.{EMFProblems, OMLResourceSet}
import gov.nasa.jpl.imce.oml.model.common.Extent
import gov.nasa.jpl.imce.oml.model.extensions.{OMLCatalog, OMLCatalogManager, OMLExtensions}
import gov.nasa.jpl.imce.oml.resolver._
import gov.nasa.jpl.imce.oml.tables.{ClosedWorldDesignations, Final, OMLSpecificationTables, OpenWorldDefinitions, Partial}
import gov.nasa.jpl.imce.oml.uuid
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{ImmutableTerminologyBox => OWLAPIImmutableTerminologyBox}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{MutableBundle => OWLAPIMutableBundle}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{MutableTerminologyBox => OWLAPIMutableTerminologyBox}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{MutableTerminologyGraph => OWLAPIMutableTerminologyGraph}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{TerminologyBox => OWLAPITerminologyBox}
import gov.nasa.jpl.omf.scala.binding.owlapi.descriptions.{MutableDescriptionBox =>  OWLAPIMutableDescriptionBox}
import gov.nasa.jpl.omf.scala.binding.owlapi.{common => _, _}
import gov.nasa.jpl.omf.scala.core.{OMFError, RelationshipCharacteristics, TerminologyKind}
import gov.nasa.jpl.omf.scala.core.OMLString._
import org.apache.xml.resolver.tools.CatalogResolver
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.xtext.resource.XtextResourceSet
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI

import scala.collection.immutable._
import scala.util.{Failure, Success, Try}
import scala.{Int, None, Option, Some, StringContext, Tuple2, Unit}
import scala.Predef.{ArrowAssoc, String, classOf}
import scalax.collection.immutable.Graph
import scalaz._
import Scalaz._

object OMLConverterFromTextualConcreteSyntax {

  def convert(outTables: String, omlFiles: List[String]): Unit = {
    val rs = OMLResourceSet.initializeResourceSet()

    val result = omlFiles.foldLeft[EMFProblems \/ Seq[Extent]](Seq.empty.right) { case (acc, omlFile) =>
      for {
        extents <- acc
        extent <- OMLResourceSet.loadOMLResource(rs, URI.createFileURI(omlFile))
      } yield extents :+ extent
    }

    result match {
      case -\/(emfProblems) =>
        System.out.println(emfProblems.show)
        System.exit(-1)

      case \/-(extents) =>

        ( Option.apply(OMLExtensions.getOrCreateCatalogManager(rs)),
          Option.apply(OMLExtensions.getCatalog(rs))) match {
          case (Some(cm: OMLCatalogManager), Some(cat: OMLCatalog)) =>

            EcoreUtil.resolveAll(rs)

            val omlUUIDg = uuid.JVMUUIDGenerator()
            implicit val f: api.OMLResolvedFactory = impl.OMLResolvedFactoryImpl(omlUUIDg)

            OMLText2Resolver.convertOMLResources2Resolver(rs) match {
              case -\/(errors) =>
                System.out.println(errors.show)

              case \/-(o2r) =>
                val apiExtent: api.Extent = o2r.rextent
                val tables: OMLSpecificationTables = Extent2Tables.convert(apiExtent)

                val result = for {
                  _ <- convertToTables(outTables, rs, cm, cat, tables)
                  _ <- convertToOWL(cm, cat, apiExtent)
                } yield ()

                result match {
                  case Success(_) =>
                    System.out.println(s"... Finished.")

                  case Failure(t) =>
                    System.err.println(t.getMessage)
                    t.printStackTrace(System.err)
                    System.exit(-1)
                }
            }

          case _ =>
            System.err.println(s"There should have been a catalog on the resource set!")
            System.exit(-1)
        }
    }
  }

  def convertToTables
  (omlTablesFile: String,
   rs: XtextResourceSet,
   cm: OMLCatalogManager,
   cat: OMLCatalog,
   tables: OMLSpecificationTables): Try[Unit] = {

    val parsedCatalogsField = classOf[OMLCatalog].getDeclaredField("parsedCatalogs")
    parsedCatalogsField.setAccessible(true)
    val catalogURLs = parsedCatalogsField.get(cat).asInstanceOf[java.util.Set[java.net.URL]]
    System.out.println(s"... catalog URLs: ${catalogURLs.size()}")

    val catalogURL = catalogURLs.iterator().next()
    System.out.println(s"... catalog URL: $catalogURL")

    val outDir = Paths.get(catalogURL.toURI).getParent
    System.out.println(s"... output dir: $outDir")

    val tablesJsonZip = if (omlTablesFile.startsWith("/")) new File(omlTablesFile) else outDir.resolve(omlTablesFile).toFile
    System.out.println(s"... output tables: $tablesJsonZip")

    OMLSpecificationTables.saveOMLSpecificationTables(tables, tablesJsonZip)
  }

  def convertToOWL
  (cm: OMLCatalogManager,
   cat: OMLCatalog,
   apiExtent: api.Extent): Try[Unit] = {

    System.out.println("... creating OMF Store")

    implicit val omfStore = OWLAPIOMFGraphStore.initGraphStore(
      OWLAPIOMFModule.owlAPIOMFModule(cm, withOMFMetadata = false).valueOr { (errors: Set[java.lang.Throwable]) =>
        val message = s"${errors.size} errors" + errors.map(_.getMessage).toList.mkString("\n => ","\n => ","\n")
        throw new scala.IllegalArgumentException(message)
      },
      OWLManager.createOWLOntologyManager(),
      new CatalogResolver(cm),
      cat)

    implicit val ops = omfStore.ops

    import OMLOps._

    implicit val ex: api.Extent = apiExtent

    val g0
    : Graph[api.Module, ModuleGraphEdge]
    = Graph[api.Module, ModuleGraphEdge]()

    val g1
    : Graph[api.Module, ModuleGraphEdge]
    = apiExtent.terminologyGraphs.foldLeft(g0) { case (gi, (_, tgraph)) => gi + tgraph }

    val g2
    : Graph[api.Module, ModuleGraphEdge]
    = apiExtent.bundles.foldLeft(g1) { case (gi, (_, bundle)) => gi + bundle }

    val g3
    : Graph[api.Module, ModuleGraphEdge]
    = apiExtent.descriptionBoxes.foldLeft(g2) { case (gi, (_, dbox)) => gi + dbox }

    val g4
    : Graph[api.Module, ModuleGraphEdge]
    = apiExtent.boxAxioms.foldLeft(g3) { case (gi, (tbox, axs)) =>
      axs.foldLeft(gi) { case (gj, ax) =>
        // traverse the edge backwards
        gj + ModuleGraphEdge((ax.target(), tbox), ax)
      }
    }

    val g5
    : Graph[api.Module, ModuleGraphEdge]
    = apiExtent.bundleAxioms.foldLeft(g4) { case (gi, (bundle, axs)) =>
      axs.foldLeft(gi) { case (gj, ax) =>
        // traverse the edge backwards
        gj + ModuleGraphEdge((ax.target(), bundle), ax)
      }
    }

    val g6
    : Graph[api.Module, ModuleGraphEdge]
    = apiExtent.closedWorldDefinitions.foldLeft(g5) { case (gi, (dbox, axs)) =>
      axs.foldLeft(gi) { case (gj, ax) =>
        // traverse the edge backwards
        gj + ModuleGraphEdge((ax.closedWorldDefinitions, dbox), ax)
      }
    }

    val g7
    : Graph[api.Module, ModuleGraphEdge]
    = apiExtent.descriptionBoxRefinements.foldLeft(g6) { case (gi, (dbox, axs)) =>
      axs.foldLeft(gi) { case (gj, ax) =>
        // traverse the edge backwards
        gj + ModuleGraphEdge((ax.refinedDescriptionBox, dbox), ax)
      }
    }

    val g = g7

    val result
    : \/[Set[java.lang.Throwable], Unit]
    = for {
      moduleOrder <- g.topologicalSort().fold[\/[Set[java.lang.Throwable], g.TopologicalOrder[api.Module]]](
        (cycleNode: g.NodeT) =>
          Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
            s"TerminologyContext circularity on node: $cycleNode in graph $g")).left,
        (order: g.TopologicalOrder[g.NodeT]) =>
          order.toOuter.right)

      // map TerminologyBox
      tbox2ont <- moduleOrder.foldLeft[\/[Set[java.lang.Throwable], Seq[(api.TerminologyBox, OWLAPIMutableTerminologyBox)]]](Seq.empty.right) {
        case (acc, tbox: api.TerminologyBox) =>
          acc.flatMap { current =>
            val k: TerminologyKind = tbox.kind match {
              case OpenWorldDefinitions =>
                TerminologyKind.isDefinition
              case ClosedWorldDesignations =>
                TerminologyKind.isDesignation
            }
            val i = tbox.iri
            val m = tbox match {
              case tgraph: api.TerminologyGraph =>
                ops.makeTerminologyGraph(tgraph.uuid, LocalName(tgraph.name), IRI.create(i), k)
              case tbundle: api.Bundle =>
                ops.makeBundle(tbundle.uuid, LocalName(tbundle.name), IRI.create(i), k)
            }
            m.map { mutable =>
              current :+ (tbox -> mutable)
            }
          }
        case (acc, _) =>
          acc
      }

      // map DescriptionBox
      dbox2ont <- moduleOrder.foldLeft[\/[Set[java.lang.Throwable], Seq[(api.DescriptionBox, OWLAPIMutableDescriptionBox)]]](Seq.empty.right) {
        case (acc, dbox: api.DescriptionBox) =>
          acc.flatMap { current =>
            val k: gov.nasa.jpl.omf.scala.core.DescriptionKind = dbox.kind match {
              case Final =>
                gov.nasa.jpl.omf.scala.core.DescriptionKind.isFinal
              case Partial =>
                gov.nasa.jpl.omf.scala.core.DescriptionKind.isPartial
            }
            val i = dbox.iri
            val m = ops.makeDescriptionBox(dbox.uuid, LocalName(dbox.name), IRI.create(i), k)
            m.map { mutable =>
              current :+ (dbox -> mutable)
            }
          }
        case (acc, _) =>
          acc
      }

      _ = tbox2ont.foreach { case (tbox, mtbox) =>
        System.out.println(s"... Conversion order: ${tbox.iri}")
      }

      // Atomic terms
      _ <- tbox2ont.foldLeft[\/[Set[java.lang.Throwable], Unit]](().right) { case (acc, (tbox, mg)) =>
        for {
          _ <- acc
          _ <- tbox.aspects.foldLeft[\/[Set[java.lang.Throwable], Unit]](().right) { case (acc1, a0) =>
            acc1.flatMap { _ =>
              ops.addAspect(mg, LocalName(a0.name)).flatMap { a1 =>
                if (a0.uuid == ops.getTermUUID(a1))
                  ().right
                else
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"OMF Schema table aspect $a1 conversion " +
                      s"results in UUID mismatch: ${ops.getTermUUID(a1)}")).left
              }
            }
          }
          _ <- tbox.concepts.foldLeft[\/[Set[java.lang.Throwable], Unit]](().right) { case (acc1, c) =>
            acc1.flatMap { _ =>
              ops.addConcept(mg, LocalName(c.name)).flatMap { c1 =>
                if (c.uuid == ops.getTermUUID(c1))
                  ().right
                else
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"OMF Schema table concept $c conversion " +
                      s"results in UUID mismatch: ${ops.getTermUUID(c1)}")).left
              }
            }
          }
          _ <- tbox.dataranges.foldLeft[\/[Set[java.lang.Throwable], Unit]](().right) { case (acc1, dr) =>
            acc1.flatMap { _ =>
              dr match {
                case sc: api.Scalar =>
                  ops.addScalarDataType(mg, LocalName(sc.name)).flatMap { sc1 =>
                    if (sc.uuid == ops.getTermUUID(sc1))
                      ().right
                    else
                      Set[java.lang.Throwable](OMFError.omfError(
                        s"OMF Schema table scalar $sc conversion " +
                          s"results in UUID mismatch: ${ops.getTermUUID(sc1)}")).left
                  }
                case _ =>
                  // delay converting restricted data ranges until module edges have been converted
                  ().right
              }
            }
          }

          _ <- tbox.structures.foldLeft[\/[Set[java.lang.Throwable], Unit]](().right) { case (acc1, st) =>
            acc1.flatMap { _ =>
              ops.addStructuredDataType(mg, LocalName(st.name)).flatMap { st1 =>
                if (st.uuid == ops.getTermUUID(st1))
                  ().right
                else
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"OMF Schema table structure $st conversion " +
                      s"results in UUID mismatch: ${ops.getTermUUID(st1)}")).left
              }
            }
          }
        } yield ()
      }

      tbox2ontMap = tbox2ont.toMap
      dbox2ontMap = dbox2ont.toMap

      // ModuleEdge (TerminologyExtensionAxiom)
      _ <- g.toOuterEdges.foldLeft[\/[Set[java.lang.Throwable], Unit]](().right) {
        case (acc,
        ModuleGraphEdge(
        (et: api.TerminologyBox, es: api.TerminologyBox),
        tx: api.TerminologyExtensionAxiom)) =>
          for {
            _ <- acc
            s <- tbox2ontMap.get(es)
              .fold[\/[Set[java.lang.Throwable], OWLAPIMutableTerminologyBox]](
              Set[java.lang.Throwable](OMFError.omfError(s"invalid edge source: $es")).left
            )(_.right)
            t <- tbox2ontMap.get(et)
              .fold[\/[Set[java.lang.Throwable], OWLAPIMutableTerminologyBox]](
              Set[java.lang.Throwable](OMFError.omfError(s"invalid edge target: $et")).left
            )(_.right)
            _ <- ops.addTerminologyExtension(extendingG = s, extendedG = t).flatMap { ax =>
              if (tx.uuid == ops.getTerminologyAxiomUUID(ax))
                ax.right
              else
                Set[java.lang.Throwable](OMFError.omfError(
                  s"OMF Schema table TerminologyExtensionAxiom $tx conversion " +
                    s"results in UUID mismatch: ${ops.getTerminologyAxiomUUID(ax)}")).left
            }
          } yield ()
        case (acc, _) =>
          acc
      }

      // ModuleEdge (TerminologyNestingAxiom)
      _ <- g.toOuterEdges.foldLeft[\/[Set[java.lang.Throwable], Unit]](().right) {
        case (acc,
        ModuleGraphEdge(
        (et: api.TerminologyBox, es: api.TerminologyBox),
        tx: api.TerminologyNestingAxiom)) =>
          for {
            _ <- acc
            s <- tbox2ontMap.get(es)
              .fold[\/[Set[java.lang.Throwable], OWLAPIMutableTerminologyGraph]](
              Set[java.lang.Throwable](OMFError.omfError(s"invalid edge source: $es")).left
            ) {
              case mg: OWLAPIMutableTerminologyGraph =>
                mg.right
              case _ =>
                Set[java.lang.Throwable](OMFError.omfError(s"Invalid edge source: $es is not a graph!")).left
            }
            t <- tbox2ontMap.get(et)
              .fold[\/[Set[java.lang.Throwable], OWLAPIMutableTerminologyBox]](
              Set[java.lang.Throwable](OMFError.omfError(s"invalid edge target: $et")).left
            )(_.right)
            _ <- ops.lookupConcept(t, tx.nestingContext.iri, recursively = false)
              .fold[\/[Set[java.lang.Throwable], Unit]](
              Set[java.lang.Throwable](OMFError.omfError(
              s"Invalid terminology nesting axiom: " +
                s"Unresolved nesting context: ${tx.nestingContext}")).left
            ) { nc: types.terms.Concept =>
              ops.addNestedTerminology(nestedGraph = s, nestingGraph = t, nestingContext = nc).flatMap { ax =>
                if (tx.uuid == ops.getTerminologyAxiomUUID(ax))
                  ().right
                else
                  Set[java.lang.Throwable](OMFError.omfError(
                    s"OMF Schema table TerminologyNestingAxiom $tx conversion " +
                      s"results in UUID mismatch: ${ops.getTerminologyAxiomUUID(ax)}")).left
              }
            }
          } yield ()
        case (acc, _) =>
          acc
      }

      // ModuleEdge (ConceptDesignationTerminologyAxiom)
      _ <- g.toOuterEdges.foldLeft[\/[Set[java.lang.Throwable], Unit]](().right) {
        case (acc,
        ModuleGraphEdge(
        (et: api.TerminologyBox, es: api.TerminologyBox),
        tx: api.ConceptDesignationTerminologyAxiom)) =>
          for {
            _ <- acc
            s <- tbox2ontMap.get(es)
              .fold[\/[Set[java.lang.Throwable], OWLAPIMutableTerminologyBox]](
              Set[java.lang.Throwable](OMFError.omfError(s"invalid edge source: $es")).left) {
              case mg: OWLAPIMutableTerminologyGraph =>
                mg.right
              case _ =>
                Set[java.lang.Throwable](OMFError.omfError(s"Invalid edge source: $es is not a graph!")).left
            }
            t <- tbox2ontMap.get(et)
              .fold[\/[Set[java.lang.Throwable], OWLAPIMutableTerminologyBox]](
              Set[java.lang.Throwable](OMFError.omfError(s"invalid edge target: $et")).left
            )(_.right)
            _ <- ops.lookupConcept(t, tx.designatedConcept.iri, recursively = false)
              .fold[\/[Set[java.lang.Throwable], Unit]](Set[java.lang.Throwable](OMFError.omfError(
              s"Invalid concept designation terminology axiom: " +
                s"Unresolved designated concept: ${tx.designatedConcept}")).left) { nc: types.terms.Concept =>
              ops.addEntityConceptDesignationTerminologyAxiom(
                graph = s, entityConceptDesignation = nc, designationTerminologyGraph = t)
                .flatMap { ax =>
                  if (tx.uuid == ops.getTerminologyAxiomUUID(ax))
                    ().right
                  else
                    Set[java.lang.Throwable](OMFError.omfError(
                      s"OMF Schema table ConceptDesignationTerminologyAxiom $tx conversion " +
                        s"results in UUID mismatch: ${ops.getTerminologyAxiomUUID(ax)}")).left
                }
            }
          } yield ()
        case (acc, _) =>
          acc
      }

      // ModuleEdge (BundledTerminologyAxiom)
      _ <- g.toOuterEdges.foldLeft[\/[Set[java.lang.Throwable], Unit]](().right) {
        case (acc,
        ModuleGraphEdge(
        (et: api.TerminologyBox, es: api.Bundle),
        tx: api.BundledTerminologyAxiom)) =>
          for {
            _ <- acc
            s <- tbox2ontMap.get(es)
              .fold[\/[Set[java.lang.Throwable], OWLAPIMutableBundle]](
              Set[java.lang.Throwable](OMFError.omfError(s"invalid edge source: $es")).left
            ) {
              case b: OWLAPIMutableBundle =>
                b.right
              case _ =>
                Set[java.lang.Throwable](OMFError.omfError(s"invalid edge source: $es")).left
            }
            t <- tbox2ontMap.get(et)
              .fold[\/[Set[java.lang.Throwable], OWLAPIMutableTerminologyBox]](
              Set[java.lang.Throwable](OMFError.omfError(s"invalid edge target: $et")).left
            )(_.right)
            _ <- ops.addBundledTerminologyAxiom(terminologyBundle = s, bundledTerminology = t).flatMap { ax =>
              if (tx.uuid == ops.getElementUUID(ax))
                ax.right
              else
                Set[java.lang.Throwable](OMFError.omfError(
                  s"OMF Schema table BundledTerminologyAxiom $tx conversion " +
                    s"results in UUID mismatch: ${ops.getElementUUID(ax)}")).left
            }
          } yield ()
        case (acc, _) =>
          acc
      }

      // ModuleEdge (DescriptionBoxExtendsClosedWorldDefinitions)
      _ <- g.toOuterEdges.foldLeft[\/[Set[java.lang.Throwable], Unit]](().right) {
        case (acc,
        ModuleGraphEdge(
        (et: api.TerminologyBox, es: api.DescriptionBox),
        tx: api.DescriptionBoxExtendsClosedWorldDefinitions)) =>
          for {
            _ <- acc
            s <- dbox2ontMap.get(es)
              .fold[\/[Set[java.lang.Throwable], OWLAPIMutableDescriptionBox]](
              Set[java.lang.Throwable](OMFError.omfError(s"invalid edge source: $es")).left
            )(_.right)
            t <- tbox2ontMap.get(et)
              .fold[\/[Set[java.lang.Throwable], OWLAPITerminologyBox]](
              Set[java.lang.Throwable](OMFError.omfError(s"invalid edge target: $et")).left
            )(_.right)
            _ <- ops.addDescriptionBoxExtendsClosedWorldDefinitions(graph = s, closedWorldDefinitions = t).flatMap { ax =>
              if (tx.uuid == ops.getElementUUID(ax))
                ax.right
              else
                Set[java.lang.Throwable](OMFError.omfError(
                  s"OMF Schema table DescriptionBoxExtendsClosedWorldDefinitions $tx conversion " +
                    s"results in UUID mismatch: ${ops.getElementUUID(ax)}")).left
            }
          } yield ()
        case (acc, _) =>
          acc
      }

      // ModuleEdge (DescriptionBoxRefinement)
      _ <- g.toOuterEdges.foldLeft[\/[Set[java.lang.Throwable], Unit]](().right) {
        case (acc,
        ModuleGraphEdge(
        (et: api.DescriptionBox, es: api.DescriptionBox),
        tx: api.DescriptionBoxRefinement)) =>
          for {
            _ <- acc
            s <- dbox2ontMap.get(es)
              .fold[\/[Set[java.lang.Throwable], OWLAPIMutableDescriptionBox]](
              Set[java.lang.Throwable](OMFError.omfError(s"invalid edge source: $es")).left
            )(_.right)
            t <- dbox2ontMap.get(et)
              .fold[\/[Set[java.lang.Throwable], OWLAPIMutableDescriptionBox]](
              Set[java.lang.Throwable](OMFError.omfError(s"invalid edge target: $et")).left
            )(_.right)
            _ <- ops.addDescriptionBoxRefinement(graph = s, refinedDescriptionBox = t).flatMap { ax =>
              if (tx.uuid == ops.getElementUUID(ax))
                ax.right
              else
                Set[java.lang.Throwable](OMFError.omfError(
                  s"OMF Schema table DescriptionBoxRefinement $tx conversion " +
                    s"results in UUID mismatch: ${ops.getElementUUID(ax)}")).left
            }
          } yield ()
        case (acc, _) =>
          acc
      }

      // Relational terms
      _ <- tbox2ont.foldLeft[\/[Set[java.lang.Throwable], Unit]](().right) { case (acc, (tbox, mg)) =>
        for {
          _ <- acc
          _ = System.out.println(s"... Relational terms: ${tbox.iri}")
          _ <- convertDataRanges(tbox, mg, tbox.dataranges)
          _ <- convertReifiedRelationships(tbox, mg, tbox.reifiedRelationships)
          _ <- convertUnreifiedRelationships(tbox, mg, tbox.unreifiedRelationships)
        } yield ()
      }

      // DataRelationships
      _ <- tbox2ont.foldLeft[\/[Set[java.lang.Throwable], Unit]](().right) { case (acc, (tbox, mg)) =>
        for {
          _ <- acc
          _ = System.out.println(s"... DataRelationships: ${tbox.iri}")
          _ <- tbox.boxStatements.foldLeft[\/[Set[java.lang.Throwable], Unit]](().right) {
            case (acc1, dr: api.EntityScalarDataProperty) =>
              acc1.flatMap { _ =>
                ( ops.lookupEntity(mg, dr.source().iri, recursively = true),
                  ops.lookupDataRange(mg, dr.target().iri, recursively = true)) match {
                  case (Some(s), Some(t)) =>
                    ops
                      .addEntityScalarDataProperty(mg, s, t, LocalName(dr.name), dr.isIdentityCriteria)
                      .flatMap { odr =>
                        if (dr.uuid == ops.getTermUUID(odr))
                          ().right
                        else
                          Set[java.lang.Throwable](OMFError.omfError(
                            s"OMF Schema table EntityScalarDataProperty $dr conversion " +
                              s"results in UUID mismatch: ${ops.getTermUUID(odr)}")).left
                      }
                  case (_, _) =>
                    Set[java.lang.Throwable](OMFError.omfError(s"Unresolved EntityScalarDataProperty: $dr")).left
                }
              }
            case (acc1, dr: api.EntityStructuredDataProperty) =>
              acc1.flatMap { _ =>
                ( ops.lookupEntity(mg, dr.source().iri, recursively = true),
                  ops.lookupStructure(mg, dr.target().iri, recursively = true)) match {
                  case (Some(s), Some(t)) =>
                    ops
                      .addEntityStructuredDataProperty(mg, s, t, LocalName(dr.name), dr.isIdentityCriteria)
                      .flatMap { odr =>
                        if (dr.uuid == ops.getTermUUID(odr))
                          ().right
                        else
                          Set[java.lang.Throwable](OMFError.omfError(
                            s"OMF Schema table EntityStructuredDataProperty $dr conversion " +
                              s"results in UUID mismatch: ${ops.getTermUUID(odr)}")).left
                      }
                  case (_, _) =>
                    Set[java.lang.Throwable](OMFError.omfError(s"Unresolved EntityStructuredDataProperty: $dr")).left
                }
              }
            case (acc1, dr: api.ScalarDataProperty) =>
              acc1.flatMap { _ =>
                ( ops.lookupStructure(mg, dr.source().iri, recursively = true),
                  ops.lookupDataRange(mg, dr.target().iri, recursively = true)) match {
                  case (Some(s), Some(t)) =>
                    ops
                      .addScalarDataProperty(mg, s, t, LocalName(dr.name))
                      .flatMap { odr =>
                        if (dr.uuid == ops.getTermUUID(odr))
                          ().right
                        else
                          Set[java.lang.Throwable](OMFError.omfError(
                            s"OMF Schema table ScalarDataProperty $dr conversion " +
                              s"results in UUID mismatch: ${ops.getTermUUID(odr)}")).left
                      }
                  case (_, _) =>
                    Set[java.lang.Throwable](OMFError.omfError(s"Unresolved ScalarDataProperty: $dr")).left
                }
              }
            case (acc1, dr: api.StructuredDataProperty) =>
              acc1.flatMap { _ =>
                ( ops.lookupStructure(mg, dr.source().iri, recursively = true),
                  ops.lookupStructure(mg, dr.target().iri, recursively = true)) match {
                  case (Some(s), Some(t)) =>
                    ops
                      .addStructuredDataProperty(mg, s, t, LocalName(dr.name))
                      .flatMap { odr =>
                        if (dr.uuid == ops.getTermUUID(odr))
                          ().right
                        else
                          Set[java.lang.Throwable](OMFError.omfError(
                            s"OMF Schema table StructuredDataProperty $dr conversion " +
                              s"results in UUID mismatch: ${ops.getTermUUID(odr)}")).left
                      }
                  case (_, _) =>
                    Set[java.lang.Throwable](OMFError.omfError(s"Unresolved StructuredDataProperty: $dr")).left
                }
              }
            case (acc1, _) =>
              acc1
          }
        } yield ()
      }


      tbox2iont <- tbox2ont
        .foldLeft[\/[Set[java.lang.Throwable], (Seq[(api.TerminologyBox, OWLAPIImmutableTerminologyBox)], OWLAPIOMF#Mutable2ImmutableTerminologyMap)]](
        (Seq.empty[(api.TerminologyBox, OWLAPIImmutableTerminologyBox)], Map.empty[OWLAPIMutableTerminologyBox, OWLAPIImmutableTerminologyBox]).right
      ) { case (acc, (tbox, mg)) =>
        for {
          pair <- acc
          (convs, m2i) = pair
          _ = System.out.println(s"... Converting terminology ${mg.kind}: ${mg.ont.getOntologyID}")
          c <- ops.asImmutableTerminology(m2i, mg)
          (conv, m2iWithConversion) = c
        } yield Tuple2(convs :+ (tbox -> conv), m2iWithConversion)
      }

      _ <- tbox2iont._1.foldLeft[\/[Set[java.lang.Throwable], Unit]](().right) { case (acc, (tbox, i)) =>
        acc.flatMap { _ =>
          System.out.println(s"... Saving terminology ${i.ont.getOntologyID}")
          val next = ops.saveTerminology(i)
          next
        }
      }
    } yield ()

    result match {
      case \/-(_) =>
        Success(())
      case -\/(errors) =>
        Failure(errors.head)
    }

  }

  final protected def convertDataRanges
  (tbox: api.TerminologyBox,
   mg: OWLAPIMutableTerminologyBox,
   drs: Set[api.DataRange],
   queue: Set[api.DataRange]=HashSet.empty,
   progress: Int=0)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore, extent: api.Extent)
  : Set[java.lang.Throwable] \/ Unit
  = {
    if (drs.isEmpty) {
      if (queue.isEmpty)
        ().right
      else if (0 == progress)
        Set[java.lang.Throwable](OMFError.omfError("No-progress in convertDataRanges!")).left
      else
        convertDataRanges(tbox, mg, queue)
    } else
      drs.head match {
        case rdr: api.RestrictedDataRange =>
          ops.lookupDataRange(mg, rdr.restrictedRange.iri, recursively = true) match {
            case None =>
              convertDataRanges(tbox, mg, drs.tail, queue + drs.head, progress)
            case Some(r) =>
              val mr: Set[java.lang.Throwable] \/ Unit = rdr match {
                case sr: api.BinaryScalarRestriction =>
                  ops
                    .addBinaryScalarRestriction(
                      mg, LocalName(sr.name), r,
                      sr.length, sr.minLength, sr.maxLength)
                    .flatMap { osr =>
                      if (sr.uuid == ops.getTermUUID(osr)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table BinaryScalarRestriction $sr conversion " +
                            s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                    }
                case sr: api.IRIScalarRestriction =>
                  ops
                    .addIRIScalarRestriction(
                      mg, LocalName(sr.name), r,
                      sr.length, sr.minLength, sr.maxLength, sr.pattern.map(Pattern(_)))
                    .flatMap { osr =>
                      if (sr.uuid == ops.getTermUUID(osr)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table IRIScalarRestriction $sr conversion " +
                            s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                    }
                case sr: api.NumericScalarRestriction =>
                  ops
                    .addNumericScalarRestriction(
                      mg, LocalName(sr.name), r,
                      sr.minInclusive.map(LexicalValue(_)),
                      sr.maxInclusive.map(LexicalValue(_)),
                      sr.minExclusive.map(LexicalValue(_)),
                      sr.maxExclusive.map(LexicalValue(_)))
                    .flatMap { osr =>
                      if (sr.uuid == ops.getTermUUID(osr)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table NumericScalarRestriction $sr conversion " +
                            s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                    }
                case sr: api.PlainLiteralScalarRestriction =>
                  ops
                    .addPlainLiteralScalarRestriction(
                      mg, LocalName(sr.name), r,
                      sr.length, sr.minLength, sr.maxLength, sr.pattern.map(Pattern(_)))
                    .flatMap { osr =>
                      if (sr.uuid == ops.getTermUUID(osr)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table PlainLiteralScalarRestriction $sr conversion " +
                            s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                    }
                case sr: api.ScalarOneOfRestriction =>
                  ops
                    .addScalarOneOfRestriction(
                      mg, LocalName(sr.name), r)
                    .flatMap { osr =>
                      if (sr.uuid == ops.getTermUUID(osr)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table ScalarOneOfRestriction $sr conversion " +
                            s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                    }
                case sr: api.StringScalarRestriction =>
                  ops
                    .addStringScalarRestriction(
                      mg, LocalName(sr.name), r,
                      sr.length, sr.minLength, sr.maxLength, sr.pattern.map(Pattern(_)))
                    .flatMap { osr =>
                      if (sr.uuid == ops.getTermUUID(osr)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table StringScalarRestriction $sr conversion " +
                            s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                    }
                case sr: api.TimeScalarRestriction =>
                  ops
                    .addTimeScalarRestriction(
                      mg, LocalName(sr.name), r,
                      sr.minInclusive.map(LexicalValue(_)),
                      sr.maxInclusive.map(LexicalValue(_)),
                      sr.minExclusive.map(LexicalValue(_)),
                      sr.maxExclusive.map(LexicalValue(_)))
                    .flatMap { osr =>
                      if (sr.uuid == ops.getTermUUID(osr)) ().right
                      else
                        Set[java.lang.Throwable](OMFError.omfError(
                          s"OMF Schema table TimeScalarRestriction $sr conversion " +
                            s"results in UUID mismatch: ${ops.getTermUUID(osr)}")).left
                    }
              }
              mr match {
                case -\/(errors) =>
                  Set[java.lang.Throwable](OMFError.omfException("Errors in convertDataRanges", errors)).left
                case \/-(_) =>
                  convertDataRanges(tbox, mg, drs.tail, queue, 1+progress)
              }
          }
        case _ =>
          convertDataRanges(tbox, mg, drs.tail, queue, progress)
      }
  }

  final protected def convertReifiedRelationships
  (tbox: api.TerminologyBox,
   mg: OWLAPIMutableTerminologyBox,
   rrs: Set[api.ReifiedRelationship],
   queue: Set[api.ReifiedRelationship]=TreeSet.empty,
   progress: Int=0)
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore, extent: api.Extent)
  : Set[java.lang.Throwable] \/ Unit
  = {
    if (rrs.isEmpty) {
      if (queue.isEmpty)
        ().right
      else if (0 == progress)
        Set[java.lang.Throwable](OMFError.omfError("No progress in convertReifiedRelationships!")).left
      else
        convertReifiedRelationships(tbox, mg, queue)
    } else {
      val rr = rrs.head
      ( ops.lookupEntity(mg, rr.source.iri, recursively = true),
        ops.lookupEntity(mg, rr.target.iri, recursively = true)) match {
        case (Some(s), Some(t)) =>
          val mr: Set[java.lang.Throwable] \/ Unit = ops
            .addReifiedRelationship(
              mg, s, t,
              Iterable() ++
                (if (rr.isAsymmetric) Iterable(RelationshipCharacteristics.isAsymmetric) else Iterable()) ++
                (if (rr.isEssential) Iterable(RelationshipCharacteristics.isEssential) else Iterable()) ++
                (if (rr.isFunctional) Iterable(RelationshipCharacteristics.isFunctional) else Iterable()) ++
                (if (rr.isInverseEssential) Iterable(RelationshipCharacteristics.isInverseEssential) else Iterable()) ++
                (if (rr.isInverseFunctional) Iterable(RelationshipCharacteristics.isInverseFunctional) else Iterable()) ++
                (if (rr.isIrreflexive) Iterable(RelationshipCharacteristics.isIrreflexive) else Iterable()) ++
                (if (rr.isReflexive) Iterable(RelationshipCharacteristics.isReflexive) else Iterable()) ++
                (if (rr.isSymmetric) Iterable(RelationshipCharacteristics.isSymmetric) else Iterable()) ++
                (if (rr.isTransitive) Iterable(RelationshipCharacteristics.isTransitive) else Iterable()),
              LocalName(rr.name),
              LocalName(rr.unreifiedPropertyName),
              rr.unreifiedInversePropertyName.map(LocalName(_)))
            .flatMap { orr =>
              if (rr.uuid == ops.getTermUUID(orr))
                ().right
              else
                Set[java.lang.Throwable](OMFError.omfError(
                  s"OMF Schema table ReifiedRelationship $rr conversion " +
                    s"results in UUID mismatch: ${ops.getTermUUID(orr)}")).left
            }
          mr match {
            case -\/(errors) =>
              Set[java.lang.Throwable](OMFError.omfException("Errors in convertReifiedRelationships", errors)).left
            case \/-(_) =>
              convertReifiedRelationships(tbox, mg, rrs.tail, queue, 1+progress)
          }
        case (_, _) =>
          convertReifiedRelationships(tbox, mg, rrs.tail, queue + rrs.head, progress)
      }
    }
  }

  protected def convertUnreifiedRelationships
  (tbox: api.TerminologyBox,
   mg: OWLAPIMutableTerminologyBox,
   urs: Set[api.UnreifiedRelationship])
  (implicit ops: OWLAPIOMFOps, store: OWLAPIOMFGraphStore, extent: api.Extent)
  : Set[java.lang.Throwable] \/ Unit
  = urs.foldLeft[\/[Set[java.lang.Throwable], Unit]](().right) { case (acc1, ur) =>
    acc1.flatMap { _ =>
      (ops.lookupEntity(mg, ur.source.iri, recursively = true),
        ops.lookupEntity(mg, ur.target.iri, recursively = true)) match {
        case (Some(s), Some(t)) =>
          ops
            .addUnreifiedRelationship(mg, s, t,
              Iterable() ++
                (if (ur.isAsymmetric) Iterable(RelationshipCharacteristics.isAsymmetric) else Iterable()) ++
                (if (ur.isEssential) Iterable(RelationshipCharacteristics.isEssential) else Iterable()) ++
                (if (ur.isFunctional) Iterable(RelationshipCharacteristics.isFunctional) else Iterable()) ++
                (if (ur.isInverseEssential) Iterable(RelationshipCharacteristics.isInverseEssential) else Iterable()) ++
                (if (ur.isInverseFunctional) Iterable(RelationshipCharacteristics.isInverseFunctional) else Iterable()) ++
                (if (ur.isIrreflexive) Iterable(RelationshipCharacteristics.isIrreflexive) else Iterable()) ++
                (if (ur.isReflexive) Iterable(RelationshipCharacteristics.isReflexive) else Iterable()) ++
                (if (ur.isSymmetric) Iterable(RelationshipCharacteristics.isSymmetric) else Iterable()) ++
                (if (ur.isTransitive) Iterable(RelationshipCharacteristics.isTransitive) else Iterable()),
              LocalName(ur.name))
            .flatMap { our =>
              if (ur.uuid == ops.getTermUUID(our))
                ().right
              else
                Set[java.lang.Throwable](OMFError.omfError(
                  s"OMF Schema table UnreifiedRelationship $ur conversion " +
                    s"results in UUID mismatch: ${ops.getTermUUID(our)}")).left
            }
        case (_, _) =>
          Set[java.lang.Throwable](OMFError.omfError(s"Unresolved unreifiedRelationship: $ur")).left
      }
    }
  }

}
