package gov.nasa.jpl.imce.oml.converters.utils

import gov.nasa.jpl.imce.oml.model.common.Extent
import gov.nasa.jpl.imce.oml.dsl.OntologicalModelingLanguageStandaloneSetup
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.emf.ecore.xcore.XcoreStandaloneSetup
import org.eclipse.xtext.resource.XtextResourceSet

import scala.collection.JavaConverters._
import scala.collection.immutable._
import scala.{StringContext,Unit}
import scalaz._, Scalaz._

object OMLResourceSet {

  def initializeResourceSet(): XtextResourceSet = {
    XcoreStandaloneSetup.doSetup()
    OntologicalModelingLanguageStandaloneSetup.doSetup()
    new XtextResourceSet()
  }

  def getOMLExtents(rs: XtextResourceSet)
  : EMFProblems \/ Set[Extent]
  = {
    rs.getResources.asScala
      .foldLeft[EMFProblems \/ Set[Extent]](Set.empty.right) { case (acc, r) =>
      for {
        extents <- acc
        extent <- getOMLResourceExtent(r)
      } yield extents + extent
    }
  }

  def getOMLResourceExtent(r: Resource)
  : EMFProblems \/ Extent
  = {
    import EMFFilterable._
    val es: Seq[Extent] = r.getContents.selectByKindOf { case ext: Extent => ext }
    if (es.isEmpty)
      new EMFProblems(new java.lang.IllegalArgumentException(
        s"OMLResourceSet.getOMFResourceExtent(r=${r.getURI}) does not have a toplevel OML Extent")).left
    else if (es.size > 1)
      new EMFProblems(new java.lang.IllegalArgumentException(
        s"OMLResourceSet.getOMFResourceExtent(r=${r.getURI}) should have 1 toplevel OML Extent, not ${es.size}")).left
    else
      es.head.right
  }

  def loadOMLResource(rs: XtextResourceSet, uri: URI)
  : EMFProblems \/ Extent
  = for {
    r <- EMFProblems.nonFatalCatch(rs.getResource(uri, true))
    _ <- EMFProblems.nonFatalCatch[Unit](EcoreUtil.resolveAll(rs))
    e <- getOMLResourceExtent(r)
  } yield e
}
