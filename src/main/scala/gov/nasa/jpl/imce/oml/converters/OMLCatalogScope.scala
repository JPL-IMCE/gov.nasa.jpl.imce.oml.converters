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
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.xml.catalog.scope.{CatalogScope, CatalogScopeManager}
import gov.nasa.jpl.omf.scala.core.OMFError

import scala.collection.immutable.{Seq, Set}
import scala.util.control.Exception.nonFatalCatch
import scala.{Option, StringContext, Unit}
import scala.Predef.ArrowAssoc
import scalaz._
import Scalaz._

case class OMLCatalogScope(omlCatalogFile: Path,
                           filePredicate: FileSystemUtilities.OMLFilePredicate,
                           omlCM: CatalogScopeManager,
                           omlCat: CatalogScope,
                           omlFiles: Seq[(tables.taggedTypes.IRI, Path)])

object OMLCatalogScope {

  def toOMLCatalogScope(inCatalog: Path,
                        filePredicate: FileSystemUtilities.OMLFilePredicate,
                        verboseFiles: Option[PrintStream])
    : OMFError.Throwables \/ OMLCatalogScope =
    for {
      omlCM <- new CatalogScopeManager().right[OMFError.Throwables]

      omlCat = omlCM.getCatalog

      _ <- nonFatalCatch[OMFError.Throwables \/ Unit]
        .withApply { t =>
          Set[java.lang.Throwable](
            new IllegalArgumentException(
              s"Failed to parse catalog: $inCatalog",
              t
            )).left
        }
        .apply {
          omlCat.parseCatalog(inCatalog.toIO.toURI.toURL).right
        }

      omlScope = omlCat.localFileScope(filePredicate)

      omlFiles = omlCat.iri2file(omlScope, filePredicate).map { case (iri, path) =>
        tables.taggedTypes.iri(iri) -> path
      }

      _ = verboseFiles.foreach { ps =>
        ps.println(
          s"Resolved ${omlFiles.size} files across ${omlScope.size} catalog rewrite rules.")
        omlScope.keys.to[Seq].sorted.foreach { uriStartPrefix =>
          val files = omlScope(uriStartPrefix)
          ps.println(
            s"=> ${files.size} resolved based on the rewrite rule for: $uriStartPrefix")
          files.foreach { f =>
            ps.println(s"  $f")
          }
        }
      }

      scope <- if (omlFiles.nonEmpty)
        \/-(
          OMLCatalogScope(omlCatalogFile = inCatalog,
                          filePredicate = filePredicate,
                          omlCM = omlCM,
                          omlCat = omlCat,
                          omlFiles = omlFiles))
      else
        -\/(
          Set[java.lang.Throwable](
            new IllegalArgumentException(
              s"No OML files found for OML Catalog: $inCatalog"
            )))

    } yield scope

}
