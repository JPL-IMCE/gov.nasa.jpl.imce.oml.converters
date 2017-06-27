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

package gov.nasa.jpl.imce.oml.converters.utils

import gov.nasa.jpl.imce.oml.model.common.Extent
import gov.nasa.jpl.imce.oml.dsl.OMLStandaloneSetup
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.emf.ecore.xcore.XcoreStandaloneSetup
import org.eclipse.xtext.resource.XtextResourceSet

import scala.collection.JavaConverters._
import scala.collection.immutable._
import scala.util.control.Exception._
import scala.{StringContext,Unit}
import scala.Predef.String
import scalaz._, Scalaz._

object OMLResourceSet {

  def initializeResourceSet(): XtextResourceSet = {
    XcoreStandaloneSetup.doSetup()
    OMLStandaloneSetup.doSetup()
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
    val nbErrors = r.getErrors.size
    val nbWarnings = r.getWarnings.size
    val message1: String = r.getErrors.asScala.foldLeft[String]("") { case (acc, d) =>
      acc + "\nError: " + s"${d.getLocation} @ ${d.getLine}:${d.getColumn}: ${d.getMessage}"
    }
    val message2: String = r.getWarnings.asScala.foldLeft[String](message1) { case (acc, d) =>
      acc + "\nWarning: " + s"${d.getLocation} @ ${d.getLine}:${d.getColumn}: ${d.getMessage}"
    }

    if (es.isEmpty)
      new EMFProblems(new java.lang.IllegalArgumentException(
        s"OMLResourceSet.getOMFResourceExtent(r=${r.getURI}) does not have a toplevel OML Extent")).left
    else if (es.size > 1)
      new EMFProblems(new java.lang.IllegalArgumentException(
        s"OMLResourceSet.getOMFResourceExtent(r=${r.getURI}) should have 1 toplevel OML Extent, not ${es.size}")).left
    else if (0 < nbErrors)
      new EMFProblems(new java.lang.IllegalArgumentException(
        s"OMLResourceSet.getOMFResourceExtent(r=${r.getURI}) => $nbErrors errors, $nbWarnings warnings\n$message2")).left
    else if (0 < nbWarnings)
      new EMFProblems(new java.lang.IllegalArgumentException(
        s"OMLResourceSet.getOMFResourceExtent(r=${r.getURI}) => $nbWarnings warnings\n$message2")).left
    else
      es.head.right
  }

  def verifyAbsoluteCanonicalURI(uri: URI)
  : EMFProblems \/ URI
  = if (uri.hasRelativePath)
    new EMFProblems(new java.lang.IllegalArgumentException(
      s"verifyAbsoluteCanonicalURI(uri=$uri) must be an absolute file URI!")).left
  else
    nonFatalCatch[EMFProblems \/ URI]
      .withApply { (t: java.lang.Throwable) =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"verifyAbsoluteCanonicalURI(uri=$uri) error: ${t.getMessage}", t
        )).left
      }
      .apply {
        val juri = new java.net.URI(uri.toString).normalize()
        URI.createURI(juri.toString).right
      }

  def loadOMLResource(rs: XtextResourceSet, uri: URI)
  : EMFProblems \/ Extent
  = for {
    u <- verifyAbsoluteCanonicalURI(uri)
    r <- EMFProblems.nonFatalCatch(rs.getResource(u, true))
    _ <- EMFProblems.nonFatalCatch[Unit](EcoreUtil.resolveAll(rs))
    e <- getOMLResourceExtent(r)
  } yield e
}
