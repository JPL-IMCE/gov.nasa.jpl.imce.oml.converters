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

import java.io.{File, FileInputStream, FileOutputStream}
import java.lang.{IllegalArgumentException, System}
import java.util.Locale

import gov.nasa.jpl.imce.oml.converters.utils.FileUtils
import gov.nasa.jpl.omf.scala.core.OMFError

import scala.collection.immutable._
import scala.{Array, Long, StringContext, Unit}
import scala.Predef.{String, augmentString, refArrayOps}
import scala.util.control.Exception.nonFatalCatch
import scalaz.{-\/, \/, \/-}

object OMLDirectoryConverter {

  def main(argv: Array[String]): Unit = {
    if (argv.length != 5)
      System.out.println(usage())
    else {
      val (_ :: cat :: flag :: out :: cmd :: Nil) = argv.to[List]
      val result: OMFError.Throwables \/ Unit = for {
        inCatalog <- checkCatalogFile(cat)
        inputDir = inCatalog.getParentFile
        outPair <- makeOutputDirectoryAndCopyCatalog(flag, out, inCatalog)
        (outputDir, outCatalog) = outPair
        conversion <- checkConversionCommand(cmd)
        inputFiles <- FileUtils.listFilesRecursively(inputDir, conversion.filePredicate)
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
    }
  }

  def checkCatalogFile(cat: String)
  : OMFError.Throwables \/ File
  = {
    val catalog = new File(cat)
    if (cat.endsWith(".catalog.xml") && catalog.exists() && catalog.canRead)
      \/-(catalog)
    else
      -\/(Set(new IllegalArgumentException(s"Invalid OASIS XML catalog file: $cat")))
  }

  def makeOutputDirectoryAndCopyCatalog(flag: String, out: String, catalog: File)
  : OMFError.Throwables \/ (File, File)
  = nonFatalCatch[OMFError.Throwables \/ (File, File)]
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
        outDir = new File(out)
        _ <- if (outDir.exists()) {
          if (deleteIfExists) {
            deleteRecursively(outDir)
          } else
            -\/(Set[java.lang.Throwable](new IllegalArgumentException(s"Output directory already exists: $out")))
        } else
          \/-(())
        result <- {
          if (outDir.mkdirs()) {
            val outCatalog = outDir.toPath.resolve(catalog.getName).toFile
            new FileOutputStream(outCatalog)
              .getChannel
              .transferFrom(new FileInputStream(catalog).getChannel, 0, Long.MaxValue)
            \/-((outDir, outCatalog))
          } else
            -\/(Set[java.lang.Throwable](new IllegalArgumentException(s"Cannot create output directory: $out")))
        }
      } yield result
    }


  def deleteRecursively(dir: File): OMFError.Throwables \/ Unit
  = {

    def deleteRecursively(files: Seq[File]): OMFError.Throwables \/ Unit
    = if (files.isEmpty)
      \/-(())
    else {
      val (f, fs) = (files.head, files.tail)
      if (f.isDirectory) {
        if (!f.canExecute)
          -\/(Set[java.lang.Throwable](new IllegalArgumentException(
            s"While deleting $dir, cannot delete $f because it is not a directory with execute permissions"
          )))
        else {
          val subs = f.listFiles().to[Seq]
          if (subs.isEmpty) {
            val confirmed = f.delete()
            if (confirmed)
              deleteRecursively(fs)
            else
              -\/(Set[java.lang.Throwable](new IllegalArgumentException(
                s"While deleting $dir, failed to delete $f"
              )))
          } else
            deleteRecursively(subs ++ files)
        }
      } else {
        val confirmed = f.delete()
        if (confirmed)
          deleteRecursively(fs)
        else
          -\/(Set[java.lang.Throwable](new IllegalArgumentException(
            s"While deleting $dir, failed to delete $f"
          )))
      }
    }

    if (dir.isDirectory && dir.exists() && dir.canExecute)
      deleteRecursively(Seq(dir))
    else
      -\/(Set[java.lang.Throwable](new IllegalArgumentException(
        s"Cannot delete $dir because it is not a directory with execute permissions"
      )))
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
       |1) Convert all OML textual concrete syntax files *.oml
       |omlDirectoryConverter -cat <oml.catalog.xml> [-out|-d] <out.dir> -text
       |
       |2) Convert all OWL2-DL ontology syntax files *.owl
       |omlDirectoryConverter -cat <oml.catalog.xml> [-out|-d] <out.dir>] -owl
       |
       |3) Convert all normalized tabular syntax files *.oml.json.zip
       |omlDirectoryConverter -cat <oml.catalog.xml> [-out|-d] <out.dir> -json
       |
       |where:
       |<oml.catalog.xml> is an OASIS XML catalog file named 'oml.catalog.xml' for resolving OML IRIs to OML files
       |<out.dir> is a new directory that will be created as long as it does not exist (-out) or
       |          will be deleted if it exists and created again (-d)
     """.stripMargin

}
