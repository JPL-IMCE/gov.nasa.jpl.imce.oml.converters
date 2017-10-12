package gov.nasa.jpl.imce.oml.converters

import java.io.File

import gov.nasa.jpl.omf.scala.core.OMFError

import scala.{Boolean, Unit}
import scala.collection.immutable.Vector
import scalaz.\/

trait ConversionCommand {

  def filePredicate(f: File): Boolean

  def convert(inCatalog: File, inputFiles: Vector[File], outputDir: File, outCatalog: File)
  : OMFError.Throwables \/ Unit
}
