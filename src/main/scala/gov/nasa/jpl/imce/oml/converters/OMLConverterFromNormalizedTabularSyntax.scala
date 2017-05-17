package gov.nasa.jpl.imce.oml.converters

import java.lang.System

import scala.{Array,StringContext, Unit}
import scala.Predef.{refArrayOps,String}

object OMLConverterFromNormalizedTabularSyntax {

  def convert(omlFiles: Array[String]): Unit = {
    System.err.println(s"convertFromNormalizedTabularSyntax: ${omlFiles.mkString("\n - ", "\n - ", "\n")}")
    System.exit(-1)
  }
}
