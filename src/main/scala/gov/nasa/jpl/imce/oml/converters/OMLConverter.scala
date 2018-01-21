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

import java.io.File
import java.lang.{IllegalArgumentException, System}
import java.util.Properties

import ammonite.ops.{Path, pwd, up}
import gov.nasa.jpl.imce.oml.converters.utils.FileSystemUtilities
import gov.nasa.jpl.imce.oml.frameless.OMLSpecificationTypedDatasets
import gov.nasa.jpl.imce.oml.model.extensions.{OMLCatalog, OMLCatalogManager}
import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import org.apache.spark.SparkConf
import org.apache.spark.sql.SparkSession
import org.apache.xml.resolver.tools.CatalogResolver

import scala.collection.immutable._
import scala.{Array, Boolean, None, Option, Some, StringContext, Unit}
import scala.Predef.{String, augmentString, wrapRefArray}
import scala.util.{Failure, Success}
import scalaz._

object OMLConverter {

  case class Options
  (input: ConversionCommand.Request = ConversionCommand.NoRequest(),
   output: ConversionCommand.OutputConversions = ConversionCommand.OutputConversions(),
   deleteOutputIfExists: Boolean = false,
   outputFolder: Path = Path("/dev/null"),
   verboseFiles: Boolean = false
  )

  def getAbsolutePath(f: File)
  : Path
  = {
    val p = if (f.toString.startsWith("~"))
      Path.expandUser(f)
    else
      Path.apply(f, pwd)
    p
  }

  val optionsParser = new scopt.OptionParser[Options]("omlConverter") {

    cmd("text")
      .text("Relative to an `oml.catalog.xml`, converts all OML textual syntax files, '*.oml' and/or '*.omlzip'")
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.CatalogInputConversion(from = ConversionCommand.ConversionFromText))
      }

    note("")

    cmd("owl")
      .text("Relative to an `oml.catalog.xml`, converts all OML files in OWL2-DL + SWRL rules, '*.owl'")
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.CatalogInputConversion(from = ConversionCommand.ConversionFromOWL))
      }

    note("")

    cmd("json")
      .text("Relative to an `oml.catalog.xml`, converts all OML tabular json archive files, '*.omlzip'")
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.CatalogInputConversion(from = ConversionCommand.ConversionFromOWLZip))
      }

    note("")

    cmd("parquet")
      .text("Convert from folders of OML parquet table files, '<dir>/<oml table>.parquet'.")
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.ParquetInputConversion())
      }

    note("")

    cmd("sql")
      .text("Convert from an SQL server.")
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.SQLInputConversion())
      }
      .children(
        arg[String]("<server>")
          .text("SQL server")
          .required()
          .action { (server, c) =>
            c.copy(input = ConversionCommand.SQLInputConversionWithServer(server))
          }
      )

    note("")

    cmd("diff")
      .text("Convert from OML files in OWL2-DL + SWRL rules, '*.owl'")
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.CompareDirectories())
      }
      .children(
        arg[File]("<dir1>")
          .text("Left side comparison, <dir1>.")
          .required()
          .validate(ConversionCommand.Request.validateExistingFolder("Invalid argument <dir1>."))
          .action { (f, c) =>
            c.copy(input = c.input.addDir1Folder(getAbsolutePath(f)))
          },

        arg[File]("<dir2>")
          .text("Right side comparison, <dir2>.")
          .required()
          .validate(ConversionCommand.Request.validateExistingFolder("Invalid argument <dir2>."))
          .action { (f, c) =>
            c.copy(input = c.input.addDir2Folder(getAbsolutePath(f)))
          }
      )

    note("")
    note("Options:")
    note("")

    help("help")
      .text(s"Prints usage information about the OML Directory Converter (pwd = $pwd}")

    note("")

    opt[File]("cat")
      .text(
        """An OASIS XML Catalog file named 'oml.catalog.xml'.
          |                           Applicable only for 'text', 'owl', 'json' commands.
        """.stripMargin)
      .abbr("c")
      .optional()
      .maxOccurs(1)
      .validate(ConversionCommand.Request.validateCatalog)
      .action { (catalog, c) =>
        c.copy(input = c.input.addCatalog(getAbsolutePath(catalog)))
      }

    opt[File]("dir")
      .text(
        """A folder of OML parquet table files: '<dir>/<oml table>.parquet'.
          |                           Applicable only for 'parquet' command.
        """.stripMargin)
      .abbr("d")
      .optional()
      .maxOccurs(1)
      .validate(ConversionCommand.Request.validateExistingFolder("Invalid parquet folder."))
      .action { (folder, c) =>
        c.copy(input = c.input.addParquetFolder(getAbsolutePath(folder)))
      }

    opt[File]("output")
      .text("Output folder where to write conversion results.")
      .abbr("out")
      .optional()
      .maxOccurs(1)
      .action { (folder, c) =>
        c.copy(outputFolder = getAbsolutePath(folder))
      }

    opt[Unit]("verbose.files")
      .abbr("v:files")
      .text("Verbose: show the input files to be processed")
      .optional()
      .action { (_, c) =>
        c.copy(verboseFiles = true)
      }

    opt[Unit]("clear")
      .text("Make sure the output folder is deleted (if it exists) and is created empty before writing conversion results.")
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

    opt[String]("sql")
      .text("Output conversion includes OML stored on an SQL server.")
      .abbr("s")
      .optional()
      .action { (server, c) =>
        c.copy(output = c.output.copy(toSQL = Some(server)))
      }

    checkConfig(o => o.input.check(o.output, o.outputFolder, o.deleteOutputIfExists))

  }

  def main(argv: Array[String]): Unit = {

    optionsParser.parse(argv, Options()) match {
      case Some(Options(ConversionCommand.NoRequest(), _, _, _, _)) =>
        System.err.println("Abnormal exit; no command requested.")

      case Some(options) =>
        options.input match {
          case ConversionCommand.CompareDirectories(dir1, dir2) =>
            DiffConversionsCommand.diff(dir1, dir2)

          case p: ConversionCommand.ParquetInputConversionWithFolder =>
            parquetInputConversion(p, options.output, options.deleteOutputIfExists, options.outputFolder)

          case c: ConversionCommand.CatalogInputConversionWithCatalog =>
            catalogInputConversion(c, options.output, options.deleteOutputIfExists, options.outputFolder, options.verboseFiles)

          case s: ConversionCommand.SQLInputConversionWithServer =>
            ConversionCommandFromOMLSQL
              .sqlInputConversion(s, options.output, options.deleteOutputIfExists, options.outputFolder)

          case _ =>
            System.err.println("Abnormal exit; no operation performed.")
        }

      case None =>
        System.err.println("Abnormal exit; no operation performed.")
    }
  }

  def parquetInputConversion
  (p: ConversionCommand.ParquetInputConversionWithFolder,
   output: ConversionCommand.OutputConversions,
   deleteOutputIfExists: Boolean,
   outputFolder: Path)
  : Unit
  = {
    val conf = new SparkConf()
      .setMaster("local")
      .setAppName(this.getClass.getSimpleName)

    implicit val spark = SparkSession
      .builder()
      .config(conf)
      .getOrCreate()
    implicit val sqlContext = spark.sqlContext

    val props = new Properties()
    props.setProperty("useSSL", "false")

    props.setProperty("dumpQueriesOnException", "true")
    props.setProperty("enablePacketDebug", "true")

    val ok = for {
      omlTables <-
        OMLSpecificationTypedDatasets
          .parquetReadOMLSpecificationTables(p.folder) match {
          case Success(tables) =>
            \/-(tables)
          case Failure(t) =>
            -\/(Set(t))
        }
      _ <- if (output.toOMLZip)
        OMLSpecificationTables
          .saveOMLSpecificationTables(omlTables, outputFolder.toIO) match {
          case Success(_) =>
            \/-(())
          case Failure(t) =>
            -\/(Set(t))
        }
      else
        \/-(())

      _ <- output.toSQL match {
        case Some(url) =>
          OMLSpecificationTypedDatasets
            .sqlWriteOMLSpecificationTables(omlTables, url, props) match {
            case Success(_) =>
              \/-(())
            case Failure(t) =>
              -\/(Set(t))
          }
        case None =>
          \/-(())
      }
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

  def catalogInputConversion
  (c: ConversionCommand.CatalogInputConversionWithCatalog,
   output: ConversionCommand.OutputConversions,
   deleteOutputIfExists: Boolean,
   outputFolder: Path,
   verboseFiles: Boolean)
  : Unit
  = {
    val ok = for {
      conversion <- c.conversionCommand()
      inCatalog = c.catalog
      _ = System.out.println(s"conversion=$conversion")

      omlCM = new OMLCatalogManager()
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
      inputDir = inCatalog / up
      _ = System.out.println(s"Input catalog=$inCatalog")
      outCatalog <- internal.makeOutputDirectoryAndCopyCatalog(deleteOutputIfExists, outputFolder, inCatalog)
      inputFiles = FileSystemUtilities.lsRecOML(
        omlCat, inCatalog, conversion.filePredicate,
        if (verboseFiles) Some(System.out) else None)
      _ = System.out.println(s"input files=${inputFiles.size}")
      _ <- conversion.convert(inCatalog, inputFiles, outputFolder, outCatalog, output)

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

}
