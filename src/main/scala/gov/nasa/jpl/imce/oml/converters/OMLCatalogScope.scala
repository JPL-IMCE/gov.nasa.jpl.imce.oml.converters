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

package gov.nasa.jpl.imce.oml.converters

import java.io.PrintStream
import java.lang.IllegalArgumentException

import ammonite.ops.Path
import gov.nasa.jpl.imce.oml.converters.utils.FileSystemUtilities
import gov.nasa.jpl.imce.oml.model.extensions.{OMLCatalog, OMLCatalogManager}
import gov.nasa.jpl.omf.scala.core.OMFError
import org.apache.xml.resolver.tools.CatalogResolver

import scala.collection.immutable.{Seq, Set}
import scala.{None,Option,Some,StringContext}
import scalaz._, Scalaz._

case class OMLCatalogScope
(omlCatalogFile: Path,
 filePredicate: FileSystemUtilities.OMLFilePredicate,
 omlCM: OMLCatalogManager,
 omlCR: CatalogResolver,
 omlCat: OMLCatalog,
 omlFiles: Seq[Path])

object OMLCatalogScope {

  def toOMLCatalogScope(inCatalog: Path, filePredicate: FileSystemUtilities.OMLFilePredicate, verboseFiles: Option[PrintStream])
  : OMFError.Throwables \/ OMLCatalogScope
  = for {
    omlCM <- new OMLCatalogManager().right[OMFError.Throwables]
    _ = omlCM.setUseStaticCatalog(false)
    _ = omlCM.setCatalogClassName("gov.nasa.jpl.imce.oml.extensions.OMLCatalog")

    omlCR = new CatalogResolver(omlCM)

    omlCat <- Option.apply(omlCR.getCatalog) match {
      case Some(oc: OMLCatalog) =>
        oc.parseCatalog(inCatalog.toIO.toURI.toURL)
        \/-(oc)
      case Some(c) =>
        -\/(Set[java.lang.Throwable](new IllegalArgumentException(
          s"OMLCatalogManager & CatalogResolver should have produced an OMLCatalog, not: ${c.getClass.getName}"
        )))
      case None =>
        -\/(Set[java.lang.Throwable](new IllegalArgumentException(
          s"OMLCatalogManager & CatalogResolver should have produced an OMLCatalog, got none!"
        )))
    }

    omlFiles = FileSystemUtilities.lsRecOML(omlCat, inCatalog, filePredicate, verboseFiles)

    scope <- if (omlFiles.nonEmpty)
      \/-(OMLCatalogScope(
        omlCatalogFile = inCatalog,
        filePredicate = filePredicate,
        omlCM = omlCM,
        omlCR = omlCR,
        omlCat = omlCat,
        omlFiles = omlFiles))
    else
      -\/(Set[java.lang.Throwable](new IllegalArgumentException(
        s"No OML files found for OML Catalog: $inCatalog"
      )))

  } yield scope

}