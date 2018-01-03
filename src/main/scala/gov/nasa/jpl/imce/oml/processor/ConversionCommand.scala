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

package gov.nasa.jpl.imce.oml.processor

import ammonite.ops.Path
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

  sealed abstract trait Request {
    val fromOWL: Boolean=false
    val fromText: Boolean=false
    val fromOMLZip: Boolean=false
    val fromParquet: Boolean=false
    def isEmpty: Boolean = !fromOWL && !fromText && !fromOMLZip && !fromParquet

    def addCatalog(catalog: File): Request
    def addParquetFolder(folder: File): Request
    def addDir1Folder(folder: File): Request
    def addDir2Folder(folder: File): Request

    def check(output: OutputConversions, outputFolder: Path): Either[String, Unit]
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

    override def check(output: OutputConversions, outputFolder: Path): Either[String, Unit]
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

    override def check(output: OutputConversions, outputFolder: Path): Either[String, Unit]
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
  (override val fromOWL: Boolean=false,
   override val fromText: Boolean=false,
   override val fromOMLZip: Boolean=false,
   override val fromParquet: Boolean=false,
   catalog: Option[Path] = Option.empty
  ) extends Request {

    override def addCatalog(catalogFile: File): Request
    = copy(catalog = Some(Path(catalogFile)))

    override def addParquetFolder(folder: File): Request = this

    override def addDir1Folder(folder: File): Request = this

    override def addDir2Folder(folder: File): Request = this

    override def check(output: OutputConversions, outputFolder: Path): Either[String, Unit]
    = catalog match {
      case None =>
        Left("No input catalog specified!")
      case Some(cat) =>
        Request.checkFile(cat, Some("oml.catalog.xml")) match {
          case Nil =>
            output.check(outputFolder)
          case m =>
            Left("Invalid OML catalog." + m.mkString("\n", "\n", "\n"))
        }
    }

    def conversionCommand()
    : OMFError.Throwables \/ (ConversionCommand, Path)
    = catalog match {
      case None =>
        -\/(Set(new IllegalArgumentException(
          s"Unspecified OML catalog!"
        )))
      case Some(cat) =>
        if (fromOWL)
          \/-(ConversionCommandFromOMLOntologySyntax -> cat)
        else if (fromText)
          \/-(ConversionCommandFromOMLTextualSyntax -> cat)
        else if (fromOMLZip)
          \/-(ConversionCommandFromOMLTabularSyntax -> cat)
        else
          -\/(Set(new IllegalArgumentException(
            s"Unspecified OML catalog-based conversion (available commands: owl, text, json)."
          )))
    }
  }

  case class ParquetInputConversion
  (override val fromOWL: Boolean=false,
   override val fromText: Boolean=false,
   override val fromOMLZip: Boolean=false,
   override val fromParquet: Boolean=false,
   folder: Option[Path] = Option.empty
  ) extends Request {

    override def addCatalog(catalog: File): Request = this

    override def addParquetFolder(dir: File): Request
    = copy(folder = Some(Path(dir)))

    override def addDir1Folder(folder: File): Request = this
    override def addDir2Folder(folder: File): Request = this

    override def check(output: OutputConversions, outputFolder: Path): Either[String, Unit]
    = folder match {
      case None =>
        Left("No input parquet folder specified!")
      case Some(d) =>
        Request.checkDirectory(d) match {
          case Nil =>
            if ("oml.parquet" == d.segments.last)
              output.check(outputFolder)
            else
              Left(s"Input parquet folder must end in 'oml.parquet' ($folder)")
          case m =>
            Left("Invalid input parquet folder." + m.mkString("\n","\n","\n"))
        }
    }
  }

  case class OutputConversions
  (toOWL: Boolean=false,
   toText: Boolean=false,
   toOMLZip: Boolean=false,
   toParquet: Boolean=false) {

    def isEmpty: Boolean = !toOWL && !toText && !toOMLZip && !toParquet

    def check(outputFolder: Path): Either[String, Unit]
    = nonFatalCatch[Either[String, Unit]]
      .withApply { t =>
        Left(t.getMessage)
      }
      .apply {
        outputFolder.toIO.mkdirs()
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