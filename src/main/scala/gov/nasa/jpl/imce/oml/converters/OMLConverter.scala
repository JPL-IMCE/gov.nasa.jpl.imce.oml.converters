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
import java.lang.{IllegalArgumentException, System}
import java.net.URL
import java.util.Properties

import ammonite.ops.{Path, pwd, up}
import gov.nasa.jpl.imce.oml.frameless.OMLSpecificationTypedDatasets
import gov.nasa.jpl.imce.oml.resolver.{GraphUtilities, ResolverUtilities, TableUtilities}
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables

import org.apache.spark.sql.{SQLContext, SparkSession}

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc
import scopt.Read

import scala.collection.immutable._
import scala.{Array, Boolean, None, Option, Some, StringContext, Unit, sys}
import scala.Predef.{ArrowAssoc, String, augmentString, wrapRefArray}
import scala.util.{Failure, Success}
import scalaz._

object OMLConverter {

  case class Options
  (input: ConversionCommand.Request = ConversionCommand.NoRequest(),
   output: ConversionCommand.OutputConversions = ConversionCommand.OutputConversions(),
   deleteOutputIfExists: Boolean = false,
   outputFolder: Option[Path] = None,
   verboseFiles: Boolean = false,
   resolveAll: Boolean = false,
   hierarchicalSort: Boolean = true
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

  implicit val urlReads: Read[URL] = Read.reads { s =>
    new URL(s)
  }

  val smallIndent: String = "  "
  val helpIndent: String = "                           "

  val optionsParser = new scopt.OptionParser[Options]("omlConverter") {

    head("BuildInfo", BuildInfo.toString)

    cmd("text")
      .text(
        s"""${smallIndent}Converts all input OML textual syntax files ('*.oml' and/or '*.omlzip') found in scope of an `oml.catalog.xml`
           |""".stripMargin)
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.CatalogInputConversion(from = ConversionCommand.ConversionFromText))
      }
      .children (

        note(s"${smallIndent}[options]:"),

        opt[Unit]("resolve:all")
          .text(
            """Invoke EcoreUtils.resolveAll() before conversion
              |""".stripMargin)
          .optional()
          .action { (_, c) =>
            c.copy(resolveAll = true)
          },

        note(s"${smallIndent}required arguments:"),

        arg[File]("<oml.catalog.xml>")
          .text(
            s"""An OASIS XML Catalog file named 'oml.catalog.xml' to search for '*.oml' and '*.oml.zip' files.
               |""".stripMargin)
          .abbr("c")
          .optional()
          .maxOccurs(1)
          .validate(ConversionCommand.Request.validateCatalog)
          .action { (catalog, c) =>
            c.copy(input = c.input.addCatalog(getAbsolutePath(catalog)))
          }
      )

    note("")

    cmd("owl")
      .text(
        s"""${smallIndent}Converts all input OML files in OWL2-DL + SWRL rules ('*.owl') found in scope of an `oml.catalog.xml`
           |""".stripMargin)
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.CatalogInputConversion(from = ConversionCommand.ConversionFromOWL))
      }
      .children(

        note(s"${smallIndent}required arguments:"),

        arg[File]("<oml.catalog.xml>")
          .text(
            s"""An OASIS XML Catalog file named 'oml.catalog.xml' to search for '*.owl' files.
               |""".stripMargin)
          .abbr("c")
          .optional()
          .maxOccurs(1)
          .validate(ConversionCommand.Request.validateCatalog)
          .action { (catalog, c) =>
            c.copy(input = c.input.addCatalog(getAbsolutePath(catalog)))
          }
      )

    note("")

    cmd("json")
      .text(
        s"""${smallIndent}Converts all input OML tabular json archive files ('*.omlzip') found in scope of an `oml.catalog.xml`
           |""".stripMargin)
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.CatalogInputConversion(from = ConversionCommand.ConversionFromOWLZip))
      }
      .children(

        note(s"${smallIndent}required arguments:"),

        arg[File]("<oml.catalog.xml>")
          .text(
            s"""An OASIS XML Catalog file named 'oml.catalog.xml' to search for '*.omlzip' files.
               |""".stripMargin)
          .abbr("c")
          .optional()
          .maxOccurs(1)
          .validate(ConversionCommand.Request.validateCatalog)
          .action { (catalog, c) =>
            c.copy(input = c.input.addCatalog(getAbsolutePath(catalog)))
          }
      )

    note("")

    cmd("parquet")
      .text(
        s"""${smallIndent}Convert from folders of OML parquet table files, '<dir>/<oml table>.parquet'.
           |""".stripMargin)
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.ParquetInputConversion())
      }
      .children(

        note(s"${smallIndent}required arguments:"),

        arg[File]("<oml.parquet folder>")
          .text(
            s"""A folder of OML parquet table files: '<dir>/<oml table>.parquet'.
               |""".stripMargin)
          .abbr("d")
          .required()
          .maxOccurs(1)
          .action { (folder, c) =>
            c.copy(input = c.input.addParquetFolder(getAbsolutePath(folder)))
          }
      )

    note("")

    cmd("sql")
      .text(
        s"""${smallIndent}Convert from an SQL server.
           |""".stripMargin)
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.SQLInputConversion())
      }
      .children(
        note(s"${smallIndent}required arguments:"),
        arg[String]("<server>")
          .text("SQL server")
          .required()
          .action { (server, c) =>
            c.copy(input = ConversionCommand.SQLInputConversionWithServer(server))
          }
      )

    note("")

    cmd("diff")
      .text(
        s"""${smallIndent}Compare OML files recursively between two directories.
           |""".stripMargin)
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.CompareDirectories())
      }
      .children(
        note(s"${smallIndent}required arguments:"),
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
       s"""${smallIndent}Merge OML content from multiple 'oml.parquet' folders.
          |${smallIndent}To merge OML content from different representations (e.g., text, owl, json, ...),
          |${smallIndent}first convert each representation to parquet, then merge the resulting 'oml.parquet' folders.
          |""".stripMargin)
      .optional()
      .action { (_, c) =>
        c.copy(input = ConversionCommand.MergeCatalogs())
      }
      .children(

        note(s"${smallIndent}required arguments:"),
        arg[File]("<oml.parquet folder>...")
          .text("One or more paths to 'oml.parquet' folders to include in the merge.")
          .required()
          .unbounded()
          .validate(ConversionCommand.Request.validateExistingFolder("Invalid argument <oml.parquet folder>."))
          .action { (f, c) =>
            c.copy(input = c.input.addMergeFolder(getAbsolutePath(f)))
          }

      )

    note("")
    note("General Options:")
    note("")

    version("version")
        .text("Shows build information.")

    note("")

    help("help")
      .text(
        s"""Prints usage information about the OML Converter.
           |${helpIndent}For help about launcher arguments, try: -h.
           |${helpIndent}To specify system properties, use:
           |${helpIndent}  -Dkey1=val1 ... -DkeyN=valN -- <OML Converter command & option arguments>
           |${helpIndent}Example for Apache SPARK properties:
           |${helpIndent}  -Dspark.app.name=MyConversion -Dspark.master=local[8] ... -- ...
           |${helpIndent}Example for Apache SPARK monitoring:
           |${helpIndent}  -Dspark.eventLog.enabled=true -Dspark.eventLog.dir=file:///tmp/spark-events ... -- ...
           |${helpIndent}Note: current directory:
           |$helpIndent$pwd
           |""".stripMargin)

    note("")

    opt[Unit]("verbose:files")
      .abbr("v:files")
      .text(
        """Verbose: show the input files found for each 'rewriteURI' entry of an OML Catalog.
          |""".stripMargin)
      .optional()
      .action { (_, c) =>
        c.copy(verboseFiles = true)
      }

    opt[File]("output:modules")
      .text(
        s"""For conversion to OWL, writes a YAML file with a IRI to file location mapping for each converted OML module.
           |${helpIndent}For other conversions, writes a text file with the list of IRIs of all OML modules.
           |$helpIndent(Not applicable for 'diff' command).
           |""".stripMargin)
      .abbr("out:modules")
      .optional()
      .maxOccurs(1)
      .action { (file, c) =>
        c.copy(output = c.output.copy(modules = Some(getAbsolutePath(file))))
      }

    opt[File]("output:catalog")
      .text(
        s"""Output catalog where to write conversion results.
           |$helpIndent(Cannot be used with --output or -out).
           |""".stripMargin)
      .abbr("out:cat")
      .optional()
      .maxOccurs(1)
      .action { (catalog, c) =>
        c.copy(output = c.output.addCatalog(getAbsolutePath(catalog)))
      }
      .validate(ConversionCommand.Request.validateCatalog)

    opt[File]("output")
      .text(
        s"""Output folder where to write conversion results.
           |$helpIndent(Cannot be used with --output:catalog or -out:cat).
           |""".stripMargin)
      .abbr("out")
      .optional()
      .maxOccurs(1)
      .action { (folder, c) =>
        c.copy(outputFolder = Some(getAbsolutePath(folder)))
      }

    opt[URL]("output:fuseki")
      .text(
        s"""Output to a fuseki server where <value> is a fuseki server url of the form:
           |${helpIndent}http://<hostname>:<port>/<datasetname> (i.e. without the '/data' suffix)
           |${helpIndent}Requires output conversion with --owl or -o.
           |${helpIndent}Requires the Jena Fuseki command 's-put' to be available on the path.
           |${helpIndent}Requires the Jena Fuseki dataset to exist.
           |$helpIndent(Not applicable for 'diff' command).
           |""".stripMargin)
      .abbr("out:fuseki")
      .optional()
      .maxOccurs(1)
      .action { (url, c) =>
        c.copy(output = c.output.copy(fuseki = Some(url)))
      }

    opt[Unit]("clear")
      .text(
        """Make sure the output folder is deleted (if it exists) and is created empty before writing conversion results.
          |""".stripMargin)
      .optional()
      .action { (_, c) =>
        c.copy(deleteOutputIfExists = true)
      }

    opt[Unit]("hsort")
      .text(
        """Disable hierarchically sort the hypergraph of OML Modules before output conversion (default = enabled)
          |""".stripMargin)
      .optional()
      .action { (_, c) =>
        c.copy(hierarchicalSort = false)
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

    opt[String]("owl:combined")
      .text(
        s"""Output conversion includes a single OWL2-DL + SWRL rules ontology whose IRI is <value>.
           |${helpIndent}The output format is OWL Functional Syntax.
           |$helpIndent(Requires --output:catalog <catalog> or -out:cat <catalog>).
           |$helpIndent(Not applicable for 'merge' command).
           |""".stripMargin)
      .abbr("o:combined")
      .optional()
      .maxOccurs(1)
      .action { (iri, c) =>
        c.copy(output = c.output.copy(toOWLCombined=Some(iri)))
      }

    opt[Unit]("owl")
      .text(
        s"""Output conversion includes OWL2-DL + SWRL rules '*.owl' ontology files, one for each OML module.
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
           |${helpIndent}Note: Ignore warnings from Apache Spark like this for some <N> and <S>:
           |${helpIndent}WARN TaskSetManager: Stage <N> contains a task of very large size (<S> KB). The maximum recommended task size is 100 KB.
           |""".stripMargin)
      .abbr("p:each")
      .optional()
      .action { (_, c) =>
        c.copy(output = c.output.copy(toParquetEach = true))
      }

    opt[Unit]("parquet")
      .text(
        s"""Output conversion aggregates all OML modules into a single 'oml.parquet' output folder.
           |${helpIndent}Note: Ignore warnings from Apache Spark like this for some <N> and <S>:
           |${helpIndent}WARN TaskSetManager: Stage <N> contains a task of very large size (<S> KB). The maximum recommended task size is 100 KB.
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

    note("")
    note("Build information")
    note(BuildInfo.toString)
    note("")

    checkConfig(o => o.input.check(o.output, o.outputFolder, o.deleteOutputIfExists))

  }

  def main(argv: Array[String]): Unit = {

    val t0 = System.currentTimeMillis()

    System.out.println(argv.mkString("# Running:\n#\n# omlConverter \\\n# ", " \\\n# ", "\n#\n"))

    optionsParser.parse(argv, Options()) match {
      case Some(Options(ConversionCommand.NoRequest(), _, _, _, _, _, _)) =>
        System.err.println("Abnormal exit; no command requested.")
        sys.exit(-1)

      case Some(options) =>
        options.input match {
          case ConversionCommand.CompareDirectories(dir1, dir2) =>
            DiffConversionsCommand.diff(dir1, dir2)

          case p: ConversionCommand.ParquetInputConversionWithFolder =>
            internal.makeOutputCatalogIfNeeded(options.deleteOutputIfExists, options.outputFolder, options.output.catalog) match {
              case \/-(outCatalogPath) =>
                parquetInputConversion(
                 p, outCatalogPath, options.output, options.deleteOutputIfExists, options.outputFolder)

              case -\/(errors) =>
                internal.showErrorsAndExit(errors)
            }

          case c: ConversionCommand.CatalogInputConversionWithCatalog =>
            c.conversionCommand() match {
              case \/-(conversion) =>
                OMLCatalogScope.toOMLCatalogScope(
                  c.catalog,
                  conversion.filePredicate,
                  if (options.verboseFiles) Some(System.out) else None) match {
                  case \/-(omlCatalogScope) =>
                    internal.makeOutputDirectoryAndCopyCatalogIfNoOutputCatalog(
                      options.deleteOutputIfExists,
                      options.outputFolder,
                      c.catalog,
                      options.output.catalog) match {
                      case \/-(outCatalog) =>
                        catalogInputConversion(
                          c,
                          conversion,
                          omlCatalogScope,
                          outCatalog,
                          options,
                          if (options.verboseFiles) Some(System.out) else None)
                      case -\/(errors) =>
                        internal.showErrorsAndExit(errors)
                    }

                  case -\/(errors) =>
                    internal.showErrorsAndExit(errors)
                }

              case -\/(errors) =>
                internal.showErrorsAndExit(errors)
            }

          case s: ConversionCommand.SQLInputConversionWithServer =>
            internal.makeOutputCatalogIfNeeded(options.deleteOutputIfExists, options.outputFolder, options.output.catalog) match {
              case \/-(outCatalogPath) =>
                ConversionCommandFromOMLSQL
                  .sqlInputConversion(s, options, outCatalogPath)

              case -\/(errors) =>
                internal.showErrorsAndExit(errors)
            }

          case m: ConversionCommand.MergeCatalogs =>
            internal.makeOutputCatalogIfNeeded(options.deleteOutputIfExists, options.outputFolder, options.output.catalog) match {
              case \/-(outCatalogPath) =>
                val conf = internal.sparkConf(this.getClass.getSimpleName)

                implicit val spark: SparkSession = SparkSession
                  .builder()
                  .config(conf)
                  .getOrCreate()
                implicit val sqlContext: SQLContext = spark.sqlContext

                ConversionCommandFromOMLMerge
                  .merge(
                    m, options, outCatalogPath) match {
                  case \/-((outCatalog, ts)) =>
                    if (options.output.toParquet)
                      internal.toParquet(options.output, outCatalog, outCatalogPath / up, ts)

                  case -\/(errors) =>
                    internal.showErrorsAndExit(errors)
                }

              case -\/(errors) =>
                internal.showErrorsAndExit(errors)
            }

          case _ =>
            System.err.println("Abnormal exit; no operation performed.")
            sys.exit(-1)
        }

      case None =>
        System.err.println("Abnormal exit; no operation performed.")
        sys.exit(-1)
    }

    val t1 = System.currentTimeMillis()
    val dt = t1 - t0
    val dt_ms = dt % 1000
    val dt_s = dt / 1000
    System.out.println(s"Converter ran in ${dt_s}s, ${dt_ms}ms")
  }

  def parquetInputConversion
  (p: ConversionCommand.ParquetInputConversionWithFolder,
   outCatalog: Path,
   output: ConversionCommand.OutputConversions,
   deleteOutputIfExists: Boolean,
   outputFolder: Option[Path])
  : Unit
  = {
    import internal.covariantOrdering

    val conf = internal.sparkConf(this.getClass.getSimpleName)

    implicit val spark: SparkSession = SparkSession
      .builder()
      .config(conf)
      .getOrCreate()
    implicit val sqlContext: SQLContext = spark.sqlContext

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

      _ <- output.modules match {
        case Some(file) =>
          internal
            .writeModuleIRIs(
              omlTables.terminologyGraphs.map(_.iri) ++
                omlTables.bundles.map(_.iri) ++
                omlTables.descriptionBoxes.map(_.iri),
              file)

        case None =>
          \/-(())
      }

      _ <- if (output.toOMLZip)
        outputFolder match {
          case Some(outDir) =>
            OMLSpecificationTables
              .saveOMLSpecificationTables(omlTables, (outDir / "aggregate.omlzip").toIO) match {
              case Success(_) =>
                \/-(())
              case Failure(t) =>
                -\/(Set(t))
            }
          case None =>
            -\/(Set[java.lang.Throwable](new IllegalArgumentException(
              "parquet conversion with omlZip output requires an output folder (--output <dir> or -out <dir>)"
            )))
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

      allModules = TableUtilities.partitionModules(omlTables)

      g0 = Graph[tables.taggedTypes.IRI, DiEdge](allModules.keys.toSeq: _*)

      g1 = (g0 /: allModules) { case (gi, (_, ti)) =>
        val gj = gi ++ TableUtilities.tableEdges(ti).map { case (src, dst) => src ~> dst }
        gj
      }

      gorder <- GraphUtilities.hierarchicalTopologicalSort(Seq(g1)).map(_.reverse)

      ts = gorder.map(iri => iri -> allModules(iri))

      extents <- ResolverUtilities.resolveTables(ResolverUtilities.initializeResolver(), ts)

      _ <- if (output.toText)
        internal
          .toText(outCatalog, extents)
          .leftMap(_.toThrowables)
      else
        \/-(())

      _ <- if (output.toOWL) {
        for {
          out_store_cat <- ConversionCommand.createOMFStoreAndLoadCatalog(outCatalog)
          (outStore, outCat) = out_store_cat
          _ <- internal
            .OMLResolver2Ontology
            .convert(extents, outStore, outputFolder)
          _ <- output.fuseki match {
            case None =>
              \/-(())
            case Some(fuseki) =>
              internal.tdbUpload(outCatalog, fuseki)
          }
        } yield ()
      } else
        \/-(())

    } yield ()

    ok match {
      case \/-(_) =>
        ()
      case -\/(errors) =>
        internal.showErrorsAndExit(errors)
    }
  }

  def catalogInputConversion
  (c: ConversionCommand.CatalogInputConversionWithCatalog,
   conversion: ConversionCommand,
   omlCatalogScope: OMLCatalogScope,
   outCatalogPath: Option[Path],
   options: Options,
   verboseFiles: Option[PrintStream])
  : Unit
  = {

    val conf = internal.sparkConf(this.getClass.getSimpleName)

    implicit val spark: SparkSession = SparkSession
      .builder()
      .config(conf)
      .getOrCreate()

    implicit val sqlContext: SQLContext = spark.sqlContext

    System.out.println(s"conversion=$conversion")
    System.out.println(s"# => ${omlCatalogScope.omlFiles.size} OML files to convert...")

    conversion.convert(omlCatalogScope, outCatalogPath, options) match {
      case \/-(_) =>
        ()
      case -\/(errors) =>
        internal.showErrorsAndExit(errors)
    }
  }

}
