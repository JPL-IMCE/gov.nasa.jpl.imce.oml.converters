package gov.nasa.jpl.imce.oml.converters

object test extends scala.App {

  val s1 = "[\\-+]?[0-9]+/[1-9][0-9]*"

  val s2 = s1.replaceAll("([^\\\\])/", "$1\\\\\\\\/")

  java.lang.System.out.println(s2)
}
