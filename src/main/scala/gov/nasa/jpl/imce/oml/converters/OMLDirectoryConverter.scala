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

import java.lang.{IllegalArgumentException, System}
import java.util.Locale

import ammonite.ops.{cp,mkdir,rm,up,Path}

import gov.nasa.jpl.imce.oml.filesystem
import gov.nasa.jpl.omf.scala.core.OMFError

import scala.collection.immutable._
import scala.{Array, StringContext, Unit}
import scala.Predef.{String, augmentString, refArrayOps}
import scala.util.control.Exception.nonFatalCatch
import scalaz.{-\/, \/, \/-}

object OMLDirectoryConverter {

  def main(argv: Array[String]): Unit = {
    if (argv.length == 5) {
      val (_ :: cat :: flag :: out :: cmd :: Nil) = argv.to[List]
      val result: OMFError.Throwables \/ Unit = for {
        inCatalog <- checkCatalogFile(cat)
        inputDir = inCatalog / up
        outputDir = Path(out)
        outCatalog <- makeOutputDirectoryAndCopyCatalog(flag, outputDir, inCatalog)
        conversion <- checkConversionCommand(cmd)
        inputFiles = filesystem.lsRecOML(inputDir, conversion.filePredicate)
        _ <- conversion.convert(inCatalog, inputFiles, outputDir, outCatalog)
      } yield ()
      result match {
        case \/-(_) =>
          ()
        case -\/(ts) =>
          System.err.println(s"### ${ts.size} Conversion Errors! ###")
          ts.foreach { t =>
            System.err.println(t.getMessage)
            t.printStackTrace(System.err)
          }
          System.exit(-1)
      }
    } else if (argv.length == 3 && argv(0) == "-diff") {
      DiffConversionsCommand.diff(Path(argv(1)),Path(argv(2)))
    } else {
      System.out.println(s"argv has ${argv.length} arguments:")
      argv.foreach { arg =>
        System.out.println(s"# arg: $arg")
      }
      System.out.println(usage())
    }
  }

  def checkCatalogFile(cat: String)
  : OMFError.Throwables \/ Path
  = {
    val catalog = new java.io.File(cat)
    if (cat.endsWith(".catalog.xml") && catalog.exists() && catalog.canRead && catalog.isAbsolute)
      \/-(Path(catalog))
    else
      -\/(Set(new IllegalArgumentException(s"Invalid OASIS XML catalog absolute file: $cat")))
  }

  def makeOutputDirectoryAndCopyCatalog(flag: String, outDir: Path, inCatalog: Path)
  : OMFError.Throwables \/ Path
  = nonFatalCatch[OMFError.Throwables \/ Path]
    .withApply {
      (t: java.lang.Throwable) =>
        -\/(Set(t))
    }
    .apply {
      for {
        deleteIfExists <- flag.toLowerCase(Locale.ENGLISH) match {
          case "-out" =>
            \/-(false)
          case "-d" =>
            \/-(true)
          case other =>
            -\/(Set[java.lang.Throwable](new IllegalArgumentException(
              s"Invalid output flag: $other\n$usage()"
            )))
        }
        _ <- if (outDir.toIO.exists()) {
          if (deleteIfExists) {
            rm(outDir)
            \/-(())
          } else
            -\/(Set[java.lang.Throwable](new IllegalArgumentException(s"Output directory already exists: $outDir")))
        } else
          \/-(())
        _ = mkdir(outDir)
        outCatalog = outDir / inCatalog.segments.last
        _ = cp(inCatalog, outCatalog)
      } yield outCatalog
    }

  def checkConversionCommand(cmd: String)
  : OMFError.Throwables \/ ConversionCommand
  = cmd match {
    case "-text" =>
      \/-(ConversionCommandFromOMLTextualSyntax)
    case "-owl" =>
      \/-(ConversionCommandFromOMLOntologySyntax)
    case "-json" =>
      \/-(ConversionCommandFromOMLTabularSyntax)
    case _ =>
      -\/(Set(new IllegalArgumentException(
        s"Unrecognized OML conversion command: $cmd (available options: -text, -owl, -json"
      )))
  }

  def usage(): String =
    s"""
       |Usage:
       |
       |0) Get information about extended options
       |omlDirectoryConverter -h
       |
       |1) Compare recursively OML files (text, owl, json) between two directories
       |omlDirectoryConverter -- -diff <dir1> <dir2>
       |
       |  where <dir1> and <dir2> are absolute paths to directories, each containing an oml.catalog.xml file.
       |
       |  For `*.owl` and `*.owl`, this comparison only reports which files are different between the two directories.
       |  The comparison does not report the differences in these files.
       |
       |  For `*.oml.json.zip`, this comparison reports line-level differences (added/deleted) for each OML table.
       |
       |2) Convert all OML textual concrete syntax files *.oml
       |omlDirectoryConverter -- -cat <oml.catalog.xml> [-out|-d] <out.dir> -text
       |
       |3) Convert all OWL2-DL ontology syntax files *.owl
       |omlDirectoryConverter -- -cat <oml.catalog.xml> [-out|-d] <out.dir>] -owl
       |
       |4) Convert all normalized tabular syntax files *.oml.json.zip
       |omlDirectoryConverter -- -cat <oml.catalog.xml> [-out|-d] <out.dir> -json
       |
       |  where:
       |  <oml.catalog.xml> is an OASIS XML catalog file named 'oml.catalog.xml' for resolving OML IRIs to OML files
       |  <out.dir> is a new directory that will be created as long as it does not exist (-out) or
       |          will be deleted if it exists and created again (-d)
     """.stripMargin

}
