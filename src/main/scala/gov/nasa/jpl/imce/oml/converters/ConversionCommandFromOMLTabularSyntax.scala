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

import java.lang.System

import ammonite.ops.Path
import gov.nasa.jpl.imce.oml.filesystem
import gov.nasa.jpl.imce.oml.resolver.{OMLResolvedTable, initializeResolver, resolveTables}
import gov.nasa.jpl.imce.oml.tables.reader
import gov.nasa.jpl.omf.scala.core.OMFError

import scala.{StringContext, Unit}
import scala.collection.immutable.{Seq, Set}
import scala.util.{Failure, Success, Try}
import scalaz._

case object ConversionCommandFromOMLTabularSyntax extends ConversionCommand {

  override val filePredicate = filesystem.omlJsonZipFilePredicate _

  implicit def toThrowables[T](v: Try[T]): OMFError.Throwables \/ T = v match {
    case Success(t) =>
      \/-(t)
    case Failure(t) =>
      -\/(Set[java.lang.Throwable](t))
  }

  override def convert(inCatalog: Path, inputFiles: Seq[Path], outputDir: Path, outCatalog: Path)
  : OMFError.Throwables \/ Unit
  = {
    val omlTables = reader.readOMLZipFiles(inputFiles)

    System.out.println(s"${inputFiles.size} input files.")
    for {
      r1 <- toThrowables(resolveTables(initializeResolver(), omlTables))
      r2 <- toThrowables(OMLResolvedTable.aggregateResolvedTables(r1))
      _ = System.out.println(s"${r2.extents.size} extents") // @TODO only 1 extent because all tables are merged.
      _ = System.out.println(s"${r2.elements.size} elements") // @TODO this is incomplete.
      _ = System.out.println(s"${r2.terminologyBoxOfTerminologyBoxStatement.size} TerminologyBoxStatements")
    } yield ()
  }

}
