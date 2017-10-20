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
import gov.nasa.jpl.imce.oml.filesystem
import gov.nasa.jpl.imce.oml.resolver.{initializeResolver,resolveTables}
import gov.nasa.jpl.imce.oml.resolver.{OMLResolvedTable}
import gov.nasa.jpl.imce.oml.tables.reader
import gov.nasa.jpl.omf.scala.core.OMFError

import scala.{Unit}
import scala.collection.immutable.{Seq,Set}
import scala.util.{Failure,Success}
import scalaz._
import Scalaz._

case object ConversionCommandFromOMLTabularSyntax extends ConversionCommand {

  override val filePredicate = filesystem.omlJsonZipFilePredicate _

  override def convert(inCatalog: Path, inputFiles: Seq[Path], outputDir: Path, outCatalog: Path)
  : OMFError.Throwables \/ Unit
  = for {
    tr <- resolveTables(
      initializeResolver(),
      reader.readOMLZipFiles(inputFiles)
    ).flatMap(tr => OMLResolvedTable.aggregateResolvedTables(tr)) match {
      case Success(rt) =>
        rt.right[OMFError.Throwables]
      case Failure(t) =>
        Set(t).left
    }
  } yield ()

}
