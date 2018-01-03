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

import java.io.File
import java.lang.{IllegalArgumentException, System}

import ammonite.ops.{Path, cp, mkdir, rm, up}
import gov.nasa.jpl.imce.oml.filesystem
import gov.nasa.jpl.imce.oml.frameless.OMLSpecificationTypedDatasets
import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import gov.nasa.jpl.omf.scala.core.OMFError
import org.apache.spark.SparkConf
import org.apache.spark.sql.SparkSession

import scala.collection.immutable._
import scala.{Array, Boolean, None, Some, StringContext, Unit}
import scala.Predef.{String, augmentString, wrapRefArray}
import scala.util.{Failure, Success}
import scala.util.control.Exception.nonFatalCatch
import scalaz.{-\/, \/, \/-}

object OMLConverter {

  case class Options
  (input: ConversionCommand.Request = ConversionCommand.NoRequest(),
   output: ConversionCommand.OutputConversions = ConversionCommand.OutputConversions(),
   deleteOutputIfExists: Boolean = false,
   outputFolder: Path = Path("/dev/null")
  )

  val optionsParser = new scopt.OptionParser[Options]("omlDirectoryConverter") {

    cmd("text")
      .text("Convert from OML textual syntax files, '*.oml'")
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.CatalogInputConversion(fromText = true))
      }

    note("")

    cmd("owl")
      .text("Convert from OML files in OWL2-DL + SWRL rules, '*.owl'")
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.CatalogInputConversion(fromOWL = true))
      }

    note("")

    cmd("json")
      .text("Convert from archives of OML tabular json files, '*.omlzip'")
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.CatalogInputConversion(fromOMLZip = true))
      }

    note("")

    cmd("parquet")
      .text("Convert from folders of OML parquet table files, '<dir>/<oml table>.parquet'.")
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.ParquetInputConversion(fromParquet = true))
      }

    note("")

    cmd("diff")
      .text("Convert from OML files in OWL2-DL + SWRL rules, '*.owl'")
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.CompareDirectories())
      }
      .children {
        arg[File]("<dir1>")
          .text("Left side comparison, <dir1>.")
          .required()
          .validate(ConversionCommand.Request.validateExistingFolder("Invalid argument <dir1>."))
          .action { (f, c) =>
            c.copy(input = c.input.addDir1Folder(f))
          }
          .children {

            arg[File]("<dir2>")
              .text("Right side comparison, <dir2>.")
              .required()
              .validate(ConversionCommand.Request.validateExistingFolder("Invalid argument <dir2>."))
              .action { (f, c) =>
                c.copy(input = c.input.addDir2Folder(f))
              }
          }
      }

    note("")
    note("Options:")
    note("")

    help("help")
      .text("Prints usage information about the OML Directory Converter.")

    note("")

    opt[File]("cat")
      .text(
        """An OASIS XML Catalog file named 'oml.catalog.xml'.
          |                          Applicable only for 'text', 'owl', 'json' commands.
        """.stripMargin)
      .abbr("c")
      .optional()
      .maxOccurs(1)
      .validate(ConversionCommand.Request.validateCatalog)
      .action { (catalog, c) =>
        c.copy(input = c.input.addCatalog(catalog))
      }

    opt[File]("dir")
      .text(
        """A folder of OML parquet table files: '<dir>/<oml table>.parquet'.
          |                          Applicable only for 'parquet' command.
        """.stripMargin)
      .abbr("d")
      .optional()
      .maxOccurs(1)
      .validate(ConversionCommand.Request.validateExistingFolder("Invalid parquet folder."))
      .action { (folder, c) =>
        c.copy(input = c.input.addParquetFolder(folder))
      }

    opt[File]("output")
      .text("Output folder where to write conversion results.")
      .abbr("out")
      .optional()
      .maxOccurs(1)
      .action { (folder, c) =>
        c.copy(outputFolder = Path.expandUser(folder))
      }

    opt[Unit]("clear")
      .text("Clears output folder before writing conversion results.")
      .optional()
      .action { (_, c) =>
        c.copy(deleteOutputIfExists = true)
      }

    opt[Unit]("text")
      .text("Output conversion includes OML as textual syntax '*.oml' files.")
      .abbr("t")
      .optional()
      .action { (_, c) =>
        c.copy(output = c.output.copy(toText = true))
      }

    opt[Unit]("owl")
      .text("Output conversion includes OML as OWL2-DL + SWRL rule '*.owl' ontology files.")
      .abbr("o")
      .optional()
      .action { (_, c) =>
        c.copy(output = c.output.copy(toOWL = true))
      }

    opt[Unit]("json")
      .text("Output conversion includes OML as archive files, '*.omlzip' of OML json tables.")
      .abbr("j")
      .optional()
      .action { (_, c) =>
        c.copy(output = c.output.copy(toOMLZip = true))
      }

    opt[Unit]("parquet")
      .text("Output conversion includes OML as a single folder of OML tables in parquet format.")
      .abbr("p")
      .optional()
      .action { (_, c) =>
        c.copy(output = c.output.copy(toParquet = true))
      }

    checkConfig(o => o.input.check(o.output, o.outputFolder))

  }

  def main(argv: Array[String]): Unit = {

    optionsParser.parse(argv, Options()) match {
      case Some(Options(ConversionCommand.NoRequest(), _, _, _)) =>
        System.err.println("Abnormal exit; no command requested.")

      case Some(options) =>
        options.input match {
          case ConversionCommand.CompareDirectories(dir1, dir2) =>
            DiffConversionsCommand.diff(dir1, dir2)

          case p: ConversionCommand.ParquetInputConversion =>
            parquetInputConversion(p, options.output, options.deleteOutputIfExists, options.outputFolder)

          case c: ConversionCommand.CatalogInputConversion =>
            catalogInputConversion(c, options.output, options.deleteOutputIfExists, options.outputFolder)

          case ConversionCommand.NoRequest() =>
            System.err.println("Abnormal exit; no operation performed.")
        }

      case None =>
        System.err.println("Abnormal exit; no operation performed.")
    }
  }

  def parquetInputConversion
  (p: ConversionCommand.ParquetInputConversion,
   output: ConversionCommand.OutputConversions,
   deleteOutputIfExists: Boolean,
   outputFolder: Path)
  : Unit
  = {
    System.out.println(p)
    System.out.println(s"output conversions: $output")
    System.out.println(s"clear: $deleteOutputIfExists")
    System.out.println(s"output folder: $outputFolder")

    val conf = new SparkConf()
      .setMaster("local")
      .setAppName(this.getClass.getSimpleName)

    implicit val spark = SparkSession
      .builder()
      .config(conf)
      .getOrCreate()
    implicit val sqlContext = spark.sqlContext

    val ok = for {
      dir <- p.folder match {
        case None =>
          Failure(new IllegalArgumentException("Unspecified OML parquet folder"))
        case Some(d) =>
          Success(d)
      }
      omlTables <- OMLSpecificationTypedDatasets.parquetReadOMLSpecificationTables(dir)
      _ <- if (output.toOMLZip)
        OMLSpecificationTables.saveOMLSpecificationTables(omlTables, outputFolder.toIO)
      else
        Success(())
    } yield ()

    ok match {
      case Success(_) =>
        ()
      case Failure(t) =>
        throw t
    }
  }

  def catalogInputConversion
  (c: ConversionCommand.CatalogInputConversion,
   output: ConversionCommand.OutputConversions,
   deleteOutputIfExists: Boolean,
   outputDir: Path)
  : Unit
  = {

    System.out.println(c)
    System.out.println(s"output conversions: $output")
    System.out.println(s"clear: $deleteOutputIfExists")
    System.out.println(s"output folder: $outputDir")

    val ok = for {
      conv_cat <- c.conversionCommand()
      (conversion, inCatalog) = conv_cat
      _ = System.out.println(s"conversion=$conversion")
      inputDir = inCatalog / up
      _ = System.out.println(s"input dir=$inputDir")
      outCatalog <- makeOutputDirectoryAndCopyCatalog(deleteOutputIfExists, outputDir, inCatalog)
      inputFiles = filesystem.lsRecOML(inputDir, conversion.filePredicate)
      _ = System.out.println(s"input files=${inputFiles.size}")
      _ <- conversion.convert(inCatalog, inputFiles, outputDir, outCatalog, output)

    } yield ()

    ok match {
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

  def makeOutputDirectoryAndCopyCatalog(deleteIfExists: Boolean, outDir: Path, inCatalog: Path)
  : OMFError.Throwables \/ Path
  = nonFatalCatch[OMFError.Throwables \/ Path]
    .withApply {
      (t: java.lang.Throwable) =>
        -\/(Set(t))
    }
    .apply {
      for {
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
       |  For `*.omlzip`, this comparison reports line-level differences (added/deleted) for each OML table.
       |
       |2) Convert all OML textual concrete syntax files *.oml
       |omlDirectoryConverter -- -cat <oml.catalog.xml> [-out|-d] <out.dir> -text
       |
       |3) Convert all OWL2-DL ontology syntax files *.owl
       |omlDirectoryConverter -- -cat <oml.catalog.xml> [-out|-d] <out.dir>] -owl
       |
       |4) Convert all normalized tabular syntax files *.omlzip
       |omlDirectoryConverter -- -cat <oml.catalog.xml> [-out|-d] <out.dir> -json
       |
       |  where:
       |  <oml.catalog.xml> is an OASIS XML catalog file named 'oml.catalog.xml' for resolving OML IRIs to OML files
       |  <out.dir> is a new directory that will be created as long as it does not exist (-out) or
       |          will be deleted if it exists and created again (-d)
     """.stripMargin

}
