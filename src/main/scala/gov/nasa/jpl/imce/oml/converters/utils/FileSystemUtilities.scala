package gov.nasa.jpl.imce.oml.converters.utils

import ammonite.ops.{Path, up}
import gov.nasa.jpl.imce.xml.catalog.scope.CatalogEntryFilePredicate

import scala.collection.immutable.Set
import scala.{Boolean, None, Some}
import scala.Predef.String

/**
  * File system utilities for OML.
  */
object FileSystemUtilities {

  sealed trait OMLFilePredicate extends CatalogEntryFilePredicate

  /**
    * Predicate for an OML concrete syntax representation file in OML's Xtext DSL (*.oml and/or *.omlzip)
    */
  case object OMLTextOrZipFilePredicate extends OMLFilePredicate {

    override def toString: String = "OML '*.text' or '*.omlzip'"

    override def apply(uriStartString: String, p: Path)
    : Boolean
    = uriStartString.startsWith("http://") &&
      p.isFile &&
      ( p.segments.last.endsWith(".oml") || p.segments.last.endsWith(".omlzip") )

    override def expandLocalFilePath(pathPrefix: Path): Set[Path] = {
      val oml: Path = pathPrefix / up / (pathPrefix.last + ".oml")
      val omlzip: Path = pathPrefix / up / (pathPrefix.last + ".omlzip")
      Set.empty[Path] ++
        (if (oml.toIO.exists()) Some(oml) else None) ++
        (if (omlzip.toIO.exists()) Some(omlzip) else None)
    }

    override val fileExtensions: Set[String] = Set(".oml", ".omlzip")
  }

  /**
    * Predicate for an OML concrete syntax representation file in 4th normal database tabular form (*.omlzip)
    */
  case object OMLJsonZipFilePredicate extends OMLFilePredicate {

    override def toString: String = "OML '*.omlzip'"

    override def apply(uriStartString: String, p: Path)
    : Boolean
    = uriStartString.startsWith("http://") &&
      p.isFile &&
      p.segments.last.endsWith(".omlzip")

    override def expandLocalFilePath(pathPrefix: Path): Set[Path] = {
      val omlzip: Path = pathPrefix / up / (pathPrefix.last + ".omlzip")
      Set(omlzip)
    }

    override val fileExtensions: Set[String] = Set(".omlzip")
  }

  /**
    * Predicate for an OML concrete syntax representation file in W3C OWL2-DL + SWRL (*.owl)
    */
  case object OMLOntologyFilePredicate extends OMLFilePredicate {

    override def toString: String = "OML '*.owl'"

    override def apply(uriStartString: String, p: Path)
    : Boolean
    = uriStartString.startsWith("http://") &&
      p.isFile &&
      p.segments.last.endsWith(".owl")

    override def expandLocalFilePath(pathPrefix: Path): Set[Path] = {
      val owl: Path = pathPrefix / up / (pathPrefix.last + ".owl")
      Set(owl)
    }

    override val fileExtensions: Set[String] = Set(".owl")
  }

}
