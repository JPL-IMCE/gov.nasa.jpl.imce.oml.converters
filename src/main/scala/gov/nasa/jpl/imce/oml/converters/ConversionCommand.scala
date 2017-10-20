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

import ammonite.ops.Path

import gov.nasa.jpl.imce.oml.model.extensions.{OMLCatalog, OMLCatalogManager}
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.core.OMFError
import org.apache.xml.resolver.tools.CatalogResolver
import org.semanticweb.owlapi.apibinding.OWLManager

import scala.collection.immutable.{Seq, Set}
import scala.util.control.Exception.nonFatalCatch
import scala.{Boolean, StringContext, Unit}
import scala.Predef.ArrowAssoc
import scalaz.{-\/, \/, \/-}

trait ConversionCommand {

  val filePredicate: (Path) => Boolean

  def convert(inCatalog: Path, inputFiles: Seq[Path], outputDir: Path, outCatalog: Path)
  : OMFError.Throwables \/ Unit
}

object ConversionCommand {

  def createOMFStoreAndLoadCatalog(catalogFile: Path)
  : OMFError.Throwables \/ (OWLAPIOMFGraphStore, OMLCatalog)
  = nonFatalCatch[OMFError.Throwables \/ (OWLAPIOMFGraphStore, OMLCatalog)]
    .withApply {
      t: java.lang.Throwable =>
        -\/(Set[java.lang.Throwable](t))
    }
    .apply {
      val cm = new OMLCatalogManager()
      cm.setUseStaticCatalog(false)
      val cr = new CatalogResolver(cm)
      val cat = cm.getPrivateCatalog match {
        case c: OMLCatalog =>
          c
        case _ =>
          throw new java.lang.IllegalArgumentException(
            s"An OMLCatalogManager should return an OMLCatalog as its private catalog."
          )
      }

      val omfStore: OWLAPIOMFGraphStore
      = OWLAPIOMFGraphStore.initGraphStore(
        OWLAPIOMFModule
          .owlAPIOMFModule(cm, withOMFMetadata = false)
          .valueOr { (errors: Set[java.lang.Throwable]) =>
            val message = s"${errors.size} errors" + errors
              .map(_.getMessage)
              .toList
              .mkString("\n => ", "\n => ", "\n")
            throw new java.lang.IllegalArgumentException(message)
          },
        OWLManager.createOWLOntologyManager(),
        cr,
        cat
      )

      omfStore.catalogIRIMapper
        .parseCatalog(catalogFile.toIO.toURI)
        .valueOr { (errors: Set[java.lang.Throwable]) =>
          val message = s"${errors.size} errors" + errors
            .map(_.getMessage)
            .toList
            .mkString("\n => ", "\n => ", "\n")
          throw new java.lang.IllegalArgumentException(message)
        }

      \/-(omfStore -> cat)
    }

}