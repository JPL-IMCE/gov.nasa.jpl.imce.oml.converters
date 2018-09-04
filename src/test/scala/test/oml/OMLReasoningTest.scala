package test.oml

import java.io.PrintStream
import java.lang.System

import ammonite.ops.{Path, tmp, up}
import gov.nasa.jpl.imce.oml.converters.{OMLCatalogScope, internal}
import org.apache.logging.log4j.io.IoBuilder
import org.scalatest.FlatSpec
import gov.nasa.jpl.imce.oml.converters.internal.OMLResolver2Ontology
import gov.nasa.jpl.imce.oml.converters.utils.{FileSystemUtilities, OMLResourceSet}
import gov.nasa.jpl.imce.oml.resolver.{api, impl}
import gov.nasa.jpl.imce.oml.uuid
import play.api.Logger

import scala.{Any, Some, StringContext, Unit}
import scala.Predef.{String, classOf}
import scalaz.{-\/, \/-}

import scala.collection.immutable.{Iterable, Seq, Vector}

trait OMLReasoningTest extends FlatSpec {

  val log = Logger(this.getClass)

  val logps: PrintStream = IoBuilder.forLogger(classOf[OMLReasoningTest]).buildPrintStream()

  def getOMLCatalogScopeAndCreateOutputFolder(
      name: String,
      filePredicate: FileSystemUtilities.OMLFilePredicate)
    : (OMLCatalogScope, Path) = {
    val url = classOf[OMLReasoningTest].getResource("/" + name)
    assert(null != url)
    assert("file" == url.getProtocol)
    val folder = Path(url.getFile)

    val outFolder = tmp.dir(prefix = name)

    val catalog: Path = folder / "oml.catalog.xml"
    OMLCatalogScope.toOMLCatalogScope(catalog, filePredicate, Some(System.out)) match {
      case \/-(omlCatalogScope) =>
        omlCatalogScope.omlCatalogFile.copy(
          outFolder.toNIO,
          Vector(omlCatalogScope.omlCatalogFile.segments.last))
        (omlCatalogScope, outFolder)

      case -\/(errors) =>
        fail(errors.head)
    }
  }

  def loadOMLTextFiles(
      omlCatalogScope: OMLCatalogScope): Seq[(api.Module, api.Extent)] = {
    val result = for {
      in_rs_cm_cat <- OMLResourceSet.initializeResourceSetWithCatalog(
        omlCatalogScope.omlCatalogFile)
      (in_rs, in_cm, in_cat) = in_rs_cm_cat

      omlFileScope = omlCatalogScope.omlFiles.values.to[Iterable]

      beforeLoad = System.currentTimeMillis()

      fileModules <- OMLResourceSet.loadOMLResources(
        in_rs,
        omlCatalogScope.omlCatalogFile / up,
        omlFileScope)

      load_delta = System.currentTimeMillis() - beforeLoad
      load_ms = load_delta % 1000
      load_s = load_delta / 1000
      _ = log.debug(
        s"# Loaded all OML resources (no EcoreUtil.resolveAll!) in ${load_s}s, ${load_ms}ms.")

      omlUUIDg = uuid.JVMUUIDGenerator()
      factory: api.OMLResolvedFactory = impl.OMLResolvedFactoryImpl(omlUUIDg)

      tuple <- internal.OMLText2Resolver.convert(fileModules)(factory)
      (_, m2e) = tuple
    } yield m2e

    result match {
      case \/-(m2e) =>
        m2e
      case -\/(errors) =>
        fail(errors.toThrowables.head)
    }
  }

  def withOML(testCode: OMLResolver2Ontology => Any): Unit = {}
}
