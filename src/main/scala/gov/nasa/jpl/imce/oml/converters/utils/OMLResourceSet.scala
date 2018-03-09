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

import ammonite.ops.Path
import gov.nasa.jpl.imce.oml.dsl.linking.OMLLinkingService
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.model.common.{Extent, Module}
import gov.nasa.jpl.imce.oml.model.extensions.{OMLCatalog, OMLCatalogManager, OMLExtensions}
import gov.nasa.jpl.imce.oml.zip.{OMLSpecificationTables, OMLZipResource, OMLZipResourceSet}
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.resource.XtextResourceSet

import scala.collection.JavaConverters._
import scala.collection.immutable._
import scala.util.control.Exception._
import scala.{Option, Some, StringContext}
import scala.Predef.{ArrowAssoc, String}
import scalaz._
import Scalaz._

object OMLResourceSet {

  def initializeResourceSet(): XtextResourceSet = {
    OMLZipResourceSet.doSetup()
    new XtextResourceSet()
  }

  def initializeResourceSetWithCatalog(catalogFile: Path)
  : EMFProblems \/ (XtextResourceSet, OMLCatalogManager, OMLCatalog)
  = {
    val rs = initializeResourceSet()
    (
      Option.apply(OMLExtensions.getOrCreateCatalogManager(rs)),
      Option.apply(OMLExtensions.getCatalog(rs))
    ) match {
      case (Some(cm: OMLCatalogManager), Some(cat: OMLCatalog)) =>
        cat.parseCatalog(catalogFile.toIO.toURI.toURL)
        (rs, cm, cat).right
      case _ =>
        new EMFProblems(new java.lang.IllegalArgumentException(
          s"Failed to create an OASIS Catalog"
        )).left
    }
  }

  def getOMLExtents(rs: XtextResourceSet)
  : EMFProblems \/ Set[Module]
  = {
    rs.getResources.asScala
      .foldLeft[EMFProblems \/ Set[Module]](Set.empty[Module].right) { case (acc, r) =>
      for {
        prev <- acc
        more <- getOMLResourceExtent(r)
      } yield prev ++ more
    }
  }

  def getOMLResourceExtent(r: Resource)
  : EMFProblems \/ Set[Module]
  = {
    import EMFFilterable._
    val es: Set[Module] =
      r
        .getContents
        .selectByKindOf { case ext: Extent => ext }
        .to[Set]
        .flatMap { ext =>
          ext.getModules.asScala.to[Set]
        }
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
    else if (0 < nbErrors)
      new EMFProblems(new java.lang.IllegalArgumentException(
        s"OMLResourceSet.getOMFResourceExtent(r=${r.getURI}) => $nbErrors errors, $nbWarnings warnings\n$message2")).left
    else if (0 < nbWarnings)
      new EMFProblems(new java.lang.IllegalArgumentException(
        s"OMLResourceSet.getOMFResourceExtent(r=${r.getURI}) => $nbWarnings warnings\n$message2")).left
    else
      es.right
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
  : EMFProblems \/ Set[Module]
  = for {
    u <- verifyAbsoluteCanonicalURI(uri)
    r <- EMFProblems.nonFatalCatch(rs.getResource(u, true))
    es <- getOMLResourceExtent(r)
  } yield es

  def loadOMLResources(rs: XtextResourceSet, dir: Path, omlFiles: Iterable[Path])
  : EMFProblems \/ Map[tables.taggedTypes.IRI, Module]
  = {
    val omlTables: OMLSpecificationTables = OMLZipResource.getOrInitializeOMLSpecificationTables(rs)
    val result = omlFiles.foldLeft(Map.empty[tables.taggedTypes.IRI, Module].right[EMFProblems]) {
      case (acc1, f) =>
        for {
          prev <- acc1
          omlFile <- {
            if (f.toIO.exists() && f.toIO.canRead)
              \/-(f)
            else
              new EMFProblems(new java.lang.IllegalArgumentException(
                s"loadOMLResources: Cannot read OML textual concrete syntax file: " +
                  f
              )).left
          }
          modules <- OMLResourceSet.loadOMLResource(
            rs,
            URI.createFileURI(omlFile.toString))
          _ = modules.foreach(omlTables.queueModule)
          updated = modules.foldLeft(prev) { case (acc2, module) =>
            acc2 + (tables.taggedTypes.iri(module.iri()) -> module)
          }
        } yield updated
    }
    OMLZipResource.clearOMLSpecificationTables(rs)

    OMLLinkingService.clearCache(rs)
    OMLLinkingService.initializeCache(rs)

    result
  }

}
