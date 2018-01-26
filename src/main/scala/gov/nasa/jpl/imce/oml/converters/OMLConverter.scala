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

import java.io.{File, PrintStream}
import java.lang.System
import java.util.Properties

import ammonite.ops.{Path, pwd, up}
import gov.nasa.jpl.imce.oml.frameless.OMLSpecificationTypedDatasets
import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import org.apache.spark.sql.SparkSession

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

  val helpIndent: String = "                           "

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

    cmd("merge")
      .text(
       s"""Merge OML content from multiple 'oml.parquet' folders.
          |  To merge OML content from different representations (e.g., text, owl, json, ...),
          |  first convert each representation to parquet, then merge the resulting 'oml.parquet' folders.
          |""".stripMargin)
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.MergeCatalogs())
      }
      .children(

        arg[File]("<oml.parquet folder>")
          .text("Path to an 'oml.parquet' folder to include in the merge.")
          .required()
          .unbounded()
          .validate(ConversionCommand.Request.validateExistingFolder("Invalid argument <oml.parquet folder>."))
          .action { (f, c) =>
            c.copy(input = c.input.addMergeFolder(getAbsolutePath(f)))
          }

      )

    note("")
    note("Options:")
    note("")

    help("help")
      .text(
        s""""Prints usage information about the OML Directory Converter
           |${helpIndent}Note: current directory:
           |$helpIndent$pwd
           |""".stripMargin)

    note("")

    opt[File]("cat")
      .text(
        s"""An OASIS XML Catalog file named 'oml.catalog.xml'.
           |$helpIndent(Applicable only for 'text', 'owl' or 'json' commands.)
           |""".stripMargin)
      .abbr("c")
      .optional()
      .maxOccurs(1)
      .validate(ConversionCommand.Request.validateCatalog)
      .action { (catalog, c) =>
        c.copy(input = c.input.addCatalog(getAbsolutePath(catalog)))
      }

    opt[File]("dir")
      .text(
        s"""A folder of OML parquet table files: '<dir>/<oml table>.parquet'.
           |$helpIndent(Applicable only for 'parquet' command.)
           |""".stripMargin)
      .abbr("d")
      .optional()
      .maxOccurs(1)
      .validate(ConversionCommand.Request.validateExistingFolder("Invalid parquet folder."))
      .action { (folder, c) =>
        c.copy(input = c.input.addParquetFolder(getAbsolutePath(folder)))
      }

    opt[File]("output")
      .text(
        """Output folder where to write conversion results.
          |""".stripMargin)
      .abbr("out")
      .optional()
      .maxOccurs(1)
      .action { (folder, c) =>
        c.copy(outputFolder = getAbsolutePath(folder))
      }

    opt[Unit]("verbose.files")
      .abbr("v:files")
      .text(
        """Verbose: show the input files found for each 'rewriteURI' entry of an OML Catalog.
          |""".stripMargin)
      .optional()
      .action { (_, c) =>
        c.copy(verboseFiles = true)
      }

    opt[Unit]("clear")
      .text(
        """Make sure the output folder is deleted (if it exists) and is created empty before writing conversion results.
          |""".stripMargin)
      .optional()
      .action { (_, c) =>
        c.copy(deleteOutputIfExists = true)
      }

    opt[Unit]("text")
      .text(
        s"""Output conversion includes OML as textual syntax '*.oml' files
           |$helpIndent(Not applicable for 'merge' command).
           |""".stripMargin)
      .abbr("t")
      .optional()
      .action { (_, c) =>
        c.copy(output = c.output.copy(toText = true))
      }

    opt[Unit]("owl")
      .text(
        s"""Output conversion includes OWL2-DL + SWRL rule '*.owl' ontology files, one for each OML module.
           |$helpIndent(Not applicable for 'merge' command).
           |""".stripMargin)
      .abbr("o")
      .optional()
      .action { (_, c) =>
        c.copy(output = c.output.copy(toOWL = true))
      }

    opt[Unit]("json")
      .text(
        s"""Output conversion includes archive files, '*.omlzip' of OML json tables, one for each OML module.
           |$helpIndent(Not applicable for 'merge' command).
           |""".stripMargin)
      .abbr("j")
      .optional()
      .action { (_, c) =>
        c.copy(output = c.output.copy(toOMLZip = true))
      }

    opt[Unit]("parquet:each")
      .text(
        s"""Output conversion includes 'oml.parquet' output folders, one for each OML module.
           |$helpIndent(Caution: this is slow!)
           |""".stripMargin)
      .abbr("p:each")
      .optional()
      .action { (_, c) =>
        c.copy(output = c.output.copy(toParquetEach = true))
      }

    opt[Unit]("parquet")
      .text(
        s"""Output conversion aggregates all OML modules into a single 'oml.parquet' output folder.
           |""".stripMargin)
      .abbr("p")
      .optional()
      .action { (_, c) =>
        c.copy(output = c.output.copy(toParquetAggregate = true))
      }

    opt[String]("sql")
      .text(
        """Output conversion aggregates all OML modules into OML data stored on an SQL server.
          |""".stripMargin)
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
            parquetInputConversion(
              p, options.output, options.deleteOutputIfExists, options.outputFolder)

          case c: ConversionCommand.CatalogInputConversionWithCatalog =>
            c.conversionCommand() match {
              case \/-(conversion) =>
                OMLCatalogScope.toOMLCatalogScope(
                  c.catalog,
                  conversion.filePredicate,
                  if (options.verboseFiles) Some(System.out) else None) match {
                  case \/-(omlCatalogScope) =>
                    internal.makeOutputDirectoryAndCopyCatalog(
                      options.deleteOutputIfExists,
                      options.outputFolder,
                      c.catalog) match {
                      case \/-(outCatalog) =>
                        catalogInputConversion(
                          c,
                          conversion,
                          omlCatalogScope,
                          outCatalog,
                          options.output,
                          options.deleteOutputIfExists,
                          options.outputFolder,
                          if (options.verboseFiles) Some(System.out) else None)
                      case -\/(errors) =>
                        internal.showErrors(errors)
                    }

                  case -\/(errors) =>
                    internal.showErrors(errors)
                }


              case -\/(errors) =>
                internal.showErrors(errors)
            }

          case s: ConversionCommand.SQLInputConversionWithServer =>
            ConversionCommandFromOMLSQL
              .sqlInputConversion(
                s, options.output, options.deleteOutputIfExists, options.outputFolder)

          case m: ConversionCommand.MergeCatalogs =>
            internal.makeOutputCatalog(options.deleteOutputIfExists, options.outputFolder) match {
              case \/-(outCatalogPath) =>
                val conf = internal.sparkConf(this.getClass.getSimpleName)

                implicit val spark = SparkSession
                  .builder()
                  .config(conf)
                  .getOrCreate()
                implicit val sqlContext = spark.sqlContext

                ConversionCommandFromOMLMerge
                  .merge(
                    m, options.output, options.deleteOutputIfExists, options.outputFolder,
                    if (options.verboseFiles) Some(System.out) else None) match {
                  case \/-((outCatalog, ts)) =>
                    if (options.output.toParquet)
                      internal.toParquet(options.output, outCatalog, outCatalogPath / up, ts)

                  case -\/(errors) =>

                    internal.showErrors(errors)
                }

              case -\/(errors) =>

                internal.showErrors(errors)
            }

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
    val conf = internal.sparkConf(this.getClass.getSimpleName)

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
          .saveOMLSpecificationTables(omlTables, (outputFolder / "aggregate.omlzip").toIO) match {
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
      case -\/(errors) =>
        internal.showErrors(errors)
    }
  }

  def catalogInputConversion
  (c: ConversionCommand.CatalogInputConversionWithCatalog,
   conversion: ConversionCommand,
   omlCatalogScope: OMLCatalogScope,
   outCatalogPath: Path,
   output: ConversionCommand.OutputConversions,
   deleteOutputIfExists: Boolean,
   outputFolder: Path,
   verboseFiles: Option[PrintStream])
  : Unit
  = {

    val conf = internal.sparkConf(this.getClass.getSimpleName)

    implicit val spark = SparkSession
      .builder()
      .config(conf)
      .getOrCreate()

    implicit val sqlContext = spark.sqlContext

    System.out.println(s"conversion=$conversion")
    System.out.println(s"# => ${omlCatalogScope.omlFiles.size} OML files to convert...")

    conversion.convert(omlCatalogScope, outputFolder, outCatalogPath, output) match {
      case \/-((outCatalog, ts)) =>
        if (output.toParquet)
          internal.toParquet(output, outCatalog, outCatalogPath / up, ts)
      case -\/(errors) =>
        internal.showErrors(errors)
    }
  }

}
