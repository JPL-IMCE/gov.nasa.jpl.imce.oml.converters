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

import ammonite.ops.{Path, rm}
import java.io.File
import java.lang.IllegalArgumentException

import gov.nasa.jpl.imce.oml.model.extensions.{OMLCatalog, OMLCatalogManager}
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.core.OMFError
import org.apache.xml.resolver.tools.CatalogResolver
import org.semanticweb.owlapi.apibinding.OWLManager

import scala.collection.immutable.{Nil, Seq, Set}
import scala.util.control.Exception.nonFatalCatch
import scala.{Boolean, Either, Left, None, Option, Right, Some, StringContext, Unit}
import scala.Predef.{ArrowAssoc, String}
import scalaz.{-\/, \/, \/-}

trait ConversionCommand {

  val filePredicate: (Path) => Boolean

  def convert
  (inCatalog: Path,
   inputFiles: Seq[Path],
   outputDir: Path,
   outCatalog: Path,
   conversions: ConversionCommand.OutputConversions)
  : OMFError.Throwables \/ Unit
}

object ConversionCommand {

  sealed abstract trait ConversionFrom
  case object ConversionFromUnspecified extends ConversionFrom
  case object ConversionFromOWL extends ConversionFrom
  case object ConversionFromText extends ConversionFrom
  case object ConversionFromOWLZip extends ConversionFrom
  case object ConversionFromParquet extends ConversionFrom
  case object ConversionFromSQL extends ConversionFrom

  sealed abstract trait Request {
    val from: ConversionFrom=ConversionFromUnspecified

    def addCatalog(catalog: File): Request
    def addParquetFolder(folder: File): Request
    def addDir1Folder(folder: File): Request
    def addDir2Folder(folder: File): Request

    def check(output: OutputConversions, outputFolder: Path, deleteIfExists: Boolean): Either[String, Unit]
  }

  object Request {

    def checkDirectory(p: Path): Seq[String] =
      checkDirectory(p.toIO)

    def checkDirectory(d: File): Seq[String]
    = Seq.empty[String] ++
      (if (d.exists) None else Some(s"$d is a non-existent directory.")) ++
      (if (d.isDirectory) None else Some(s"$d is not a directory.")) ++
      (if (d.canRead) None else Some(s"$d is not readable.")) ++
      (if (d.canExecute) None else Some(s"$d is not executable."))

    def checkFile(p: Path, end: Option[String] = None): Seq[String] =
      checkFile(p.toIO, end)

    def checkFile(f: File, end: Option[String]): Seq[String]
    = Seq.empty[String] ++
      (if (f.exists) None else Some(s"$f is a non-existent file.")) ++
      (if (f.canRead) None else Some(s"$f is not readable.")) ++
      end.flatMap {
        e => if (f.getName.endsWith(e)) None else Some(s"does not end in '$e'.")
      }

    def validateCatalog(catalog: File): Either[String, Unit]
    = checkFile(catalog, Some("oml.catalog.xml")) match {
      case Nil =>
        Right(())
      case ms =>
        Left("Invalid OML catalog file."+ms.mkString("\n", "\n", "\n"))
    }

    def validateExistingFolder(message: String)(folder: File): Either[String, Unit]
    = checkDirectory(folder) match {
      case Nil =>
        Right(())
      case ms =>
        Left(message + ms.mkString("\n", "\n", "\n"))
    }
  }

  case class NoRequest() extends Request {

    override def addCatalog(catalog: File): Request = this
    override def addParquetFolder(folder: File): Request = this
    override def addDir1Folder(folder: File): Request = this
    override def addDir2Folder(folder: File): Request = this

    override def check(output: OutputConversions, outputFolder: Path, deleteIfExists: Boolean): Either[String, Unit]
    = Left("No request specified.")
  }

  case class CompareDirectories
  (dir1: Path = Path("/dev/null"),
   dir2: Path = Path("/dev/null")
  ) extends Request {

    override def addCatalog(catalog: File): Request = this
    override def addParquetFolder(folder: File): Request = this

    override def addDir1Folder(folder: File): Request
    = copy(dir1 = Path.expandUser(folder))

    override def addDir2Folder(folder: File): Request
    = copy(dir2 = Path.expandUser(folder))

    override def check(output: OutputConversions, outputFolder: Path, deleteIfExists: Boolean): Either[String, Unit]
    = (Request.checkDirectory(dir1), Request.checkDirectory(dir2)) match {
      case (Nil, Nil) =>
        Right(())
      case (_, Nil) =>
        Left(s"Invalid command CompareDirectories: <invalid dir1>.")
      case (Nil, _) =>
        Left(s"Invalid command CompareDirectories: <invalid dir2>.")
      case (_, _) =>
        Left(s"Invalid command CompareDirectories: <invalid dir1>, <invalid dir2>.")
    }
  }

  case class CatalogInputConversion
  (override val from: ConversionFrom=ConversionFromUnspecified
  ) extends Request {

    override def addCatalog(catalogFile: File): Request
    = CatalogInputConversionWithCatalog(from, Path(catalogFile))

    override def addParquetFolder(folder: File): Request = this

    override def addDir1Folder(folder: File): Request = this

    override def addDir2Folder(folder: File): Request = this

    override def check(output: OutputConversions, outputFolder: Path, deleteIfExists: Boolean): Either[String, Unit]
    = Left("No input catalog specified!")

  }

  case class CatalogInputConversionWithCatalog
  (override val from: ConversionFrom=ConversionFromUnspecified,
   catalog: Path
  ) extends Request {

    override def addCatalog(catalogFile: File): Request
    = copy(catalog = Path(catalogFile))

    override def addParquetFolder(folder: File): Request = this

    override def addDir1Folder(folder: File): Request = this

    override def addDir2Folder(folder: File): Request = this

    override def check(output: OutputConversions, outputFolder: Path, deleteIfExists: Boolean): Either[String, Unit]
    = output.check(outputFolder, deleteIfExists)

    def conversionCommand()
    : OMFError.Throwables \/ ConversionCommand
    = from match {
      case ConversionFromOWL =>
        \/-(ConversionCommandFromOMLOntologySyntax)
      case ConversionFromText =>
        \/-(ConversionCommandFromOMLTextualSyntax)
      case ConversionFromOWLZip =>
        \/-(ConversionCommandFromOMLTabularSyntax)
      case _ =>
        -\/(Set(new IllegalArgumentException(
          s"Unspecified OML catalog-based conversion (available commands: owl, text, json)."
        )))
    }
  }

  case class ParquetInputConversion() extends Request {
    override val from: ConversionFrom=ConversionFromUnspecified
    override def addCatalog(catalog: File): Request = this

    override def addParquetFolder(dir: File): Request
    = ParquetInputConversionWithFolder(folder = Path(dir))

    override def addDir1Folder(folder: File): Request = this
    override def addDir2Folder(folder: File): Request = this

    override def check(output: OutputConversions, outputFolder: Path, deleteIfExists: Boolean): Either[String, Unit]
    = Left("No input parquet folder specified!")
  }

  case class ParquetInputConversionWithFolder
  (folder: Path) extends Request {
    override val from: ConversionFrom=ConversionFromUnspecified

    override def addCatalog(catalog: File): Request = this

    override def addParquetFolder(dir: File): Request
    = copy(folder = Path(dir))

    override def addDir1Folder(folder: File): Request = this
    override def addDir2Folder(folder: File): Request = this

    override def check(output: OutputConversions, outputFolder: Path, deleteIfExists: Boolean): Either[String, Unit]
    = Request.checkDirectory(folder) match {
      case Nil =>
        if ("oml.parquet" == folder.segments.last)
          output.check(outputFolder, deleteIfExists)
        else
          Left(s"Input parquet folder must end in 'oml.parquet' ($folder)")
      case m =>
        Left("Invalid input parquet folder." + m.mkString("\n","\n","\n"))
    }
  }

  case class SQLInputConversion() extends Request {
    override val from: ConversionFrom=ConversionFromSQL

    override def addCatalog(catalog: File): Request = this

    override def addParquetFolder(dir: File): Request = this

    override def addDir1Folder(folder: File): Request = this
    override def addDir2Folder(folder: File): Request = this

    override def check(output: OutputConversions, outputFolder: Path, deleteIfExists: Boolean): Either[String, Unit]
    = Left("No SQL server specified!")

  }

  case class SQLInputConversionWithServer(server: String) extends Request {
    override val from: ConversionFrom=ConversionFromSQL

    override def addCatalog(catalog: File): Request = this

    override def addParquetFolder(dir: File): Request = this

    override def addDir1Folder(folder: File): Request = this
    override def addDir2Folder(folder: File): Request = this

    override def check(output: OutputConversions, outputFolder: Path, deleteIfExists: Boolean): Either[String, Unit]
    = output.check(outputFolder, deleteIfExists)
  }

  case class OutputConversions
  (toOWL: Boolean=false,
   toText: Boolean=false,
   toOMLZip: Boolean=false,
   toParquet: Boolean=false,
   toSQL: Option[String]=None) {

    def isEmpty: Boolean = !toOWL && !toText && !toOMLZip && !toParquet && toSQL.isEmpty

    def check(outputFolder: Path, deleteIfExists: Boolean): Either[String, Unit]
    = nonFatalCatch[Either[String, Unit]]
      .withApply { t =>
        Left(t.getMessage)
      }
      .apply {
        if (deleteIfExists) {
          if (outputFolder.toIO.exists())
            rm(outputFolder)
          outputFolder.toIO.mkdirs()
        }
        Request.checkDirectory(outputFolder) match {
          case Nil =>
            Right(())
          case ms =>
            Left("Invalid output folder." + ms.mkString("\n", "\n", "\n"))
        }
      }

  }

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