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
