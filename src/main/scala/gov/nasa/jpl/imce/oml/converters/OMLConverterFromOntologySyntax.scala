package gov.nasa.jpl.imce.oml.converters

import java.lang.System

import scala.{Array, StringContext, Unit}
import scala.Predef.{refArrayOps,String}

object OMLConverterFromOntologySyntax {

  def convert(omlFiles: Array[String]): Unit = {
    System.err.println(s"convertFromOntologySyntax: ${omlFiles.mkString("\n - ", "\n - ", "\n")}")
    System.exit(-1)
  }
}
