package gov.nasa.jpl.imce.oml.converters.utils

import java.io.{File, FileFilter}
import java.lang.IllegalArgumentException

import gov.nasa.jpl.omf.scala.core.OMFError

import scala.collection.immutable.{Set,Vector}
import scala.{Boolean, StringContext}
import scala.Predef.refArrayOps
import scalaz.{\/,\/-,-\/}

object FileUtils {

  def listFilesRecursively
  (dir: File,
   predicate: File => Boolean)
  : OMFError.Throwables \/ Vector[File]
  = if (!dir.isDirectory || !dir.canRead)
      -\/(Set(new IllegalArgumentException(s"Directory must be readable: $dir")))
  else {

    val directoryFilter = new FileFilter() {
      override def accept(pathname: File): Boolean = pathname.isDirectory
    }

    val fileFilter = new FileFilter() {
      override def accept(pathname: File): Boolean = predicate(pathname)
    }

    @scala.annotation.tailrec
    def listFilesRecursively
    (dirs: Vector[File], files: Vector[File])
    : OMFError.Throwables \/ Vector[File]
    = if (dirs.isEmpty)
      \/-(files)
    else {
      val (dir, rest) = (dirs.head, dirs.tail)
      val moreDirs = dir.listFiles(directoryFilter).to[Vector]
      val moreFiles = dir.listFiles(fileFilter).to[Vector]
      listFilesRecursively(moreDirs ++ rest, files ++ moreFiles)
    }

    listFilesRecursively(Vector[File](dir), Vector.empty[File])

  }
}
