package gov.nasa.jpl.imce.oml.converters

import scala.Predef.String

sealed trait OMLFormat {
  val label: String
}

object OMLTextFormat extends OMLFormat {
  val label = "OML Textual Syntax"
}
