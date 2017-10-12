package gov.nasa.jpl.imce.oml.converters

import java.io.File

import gov.nasa.jpl.omf.scala.core.OMFError

import scala.{Boolean, Unit}
import scala.collection.immutable.Vector
import scalaz.{\/,\/-}

case object ConversionCommandFromOMLTabularSyntax extends ConversionCommand {

  override def filePredicate(f: File): Boolean = f.isFile && f.getName.endsWith(".json.zip")

  override def convert(inCatalog: File, inputFiles: Vector[File], outputDir: File, outCatalog: File)
  : OMFError.Throwables \/ Unit
  = \/-(())

}
