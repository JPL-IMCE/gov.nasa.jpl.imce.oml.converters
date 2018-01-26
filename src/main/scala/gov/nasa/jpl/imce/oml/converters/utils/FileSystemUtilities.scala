package gov.nasa.jpl.imce.oml.converters.utils

import java.io.{File, PrintStream}

import ammonite.ops.{Path, ls, up}
import gov.nasa.jpl.imce.oml.model.extensions.OMLCatalog
import org.apache.xml.resolver.{Catalog, CatalogEntry}

import scala.collection.immutable.{Map, Seq, Set}
import scala.{Boolean, None, Option, Some, StringContext, Unit}
import scala.Predef.{ArrowAssoc, String, augmentString, require}

/**
  * File system utilities for OML.
  */
object FileSystemUtilities {

  sealed trait OMLFilePredicate {

    def apply(p: Path): Boolean

  }

  /**
    * Predicate for an OML concrete syntax representation file in OML's Xtext DSL (*.oml and/or *.omlzip)
    */
  case object OMLTextOrZipFilePredicate extends OMLFilePredicate {

    override def toString: String = "OML '*.text' or '*.omlzip'"

    override def apply(p: Path)
    : Boolean
    = p.isFile && ( p.segments.last.endsWith(".oml") || p.segments.last.endsWith(".omlzip") )

  }

  /**
    * Predicate for an OML concrete syntax representation file in 4th normal database tabular form (*.omlzip)
    */
  case object OMLJsonZipFilePredicate extends OMLFilePredicate {

    override def toString: String = "OML '*.omlzip'"

    override def apply(p: Path)
    : Boolean
    = p.isFile && p.segments.last.endsWith(".omlzip")
  }

  /**
    * Predicate for an OML concrete syntax representation file in W3C OWL2-DL + SWRL (*.owl)
    */
  case object OMLOntologyFilePredicate extends OMLFilePredicate {

    override def toString: String = "OML '*.owl'"

    override def apply(p: Path)
    : Boolean
    = p.isFile && p.segments.last.endsWith(".owl")

  }

  /**
    * Predicate for an OML OASIS Catalog XML file
    */
  val omlCatalogFilter = (p: Path) => p.isFile && p.segments.last == "oml.catalog.xml"

  /**
    * Recursively finds OML files of a certain kind from a given path
    *
    * @param cat The OMLCatalog to explore the rewrite rules recursively if `p` is the path to the catalog
    * @param p Either the path to the OMLCatalog (to explore the rewrite rules recursively)
    *          or to a directory, which is explored recursively.
    * @param omlFilePredicate A Path predicate for the kind of OML file to include in the result
    * @param verboseFiles If true, prints the files found.
    * @return A sequence of Path files satisfying the kindFilter found by recursively exploring `p`
    *         if it is a directory or `p`'s parent folder if it is a file named 'oml.catalog.xml'.
    *         In other cases, the result sequence is empty.
    */
  def lsRecOML
  (cat: OMLCatalog, p: Path, omlFilePredicate: OMLFilePredicate, verboseFiles: Option[PrintStream])
  : Seq[Path]
  = {
    def lsRecNoCatalog(dir: Path): Seq[Path]
    = {
      val files: Seq[Path]
      = ls
        .rec((p: Path) => p.isFile && !omlFilePredicate(p))(dir)
        .filter(omlFilePredicate.apply)
        .to[Seq]
        .sortBy(_.toString())

      verboseFiles.fold[Unit](()) { ps =>
        ps.println(s"# lsRecNoCatalog: found ${files.size} OML files in $p")
        val prefix = p.toString()
        files.foreach { f =>
          ps.println(s"# => ${f.toString().stripPrefix(prefix)}")
        }
        ps.println(s"# -------------------")
      }

      files
    }

    def lsRecCatalog(dir: Path): Seq[Path]
    = {
      import scala.collection.JavaConverters._
      import scala.util.control.Exception.nonFatalCatch

      val entries: Seq[CatalogEntry] = cat.entries().asScala.to[Seq]

      require(2 == CatalogEntry.getEntryArgCount(Catalog.REWRITE_URI))

      val rewritePaths: Map[Path, String]
      = entries.foldLeft[Map[Path, String]](Map.empty[Path, String]) {
        case (acc, entry: CatalogEntry) if Catalog.REWRITE_URI== entry.getEntryType =>

          val rewriteEntry
          : Option[(Path, String)]
          = nonFatalCatch[Option[(Path, String)]]
            .withApply { _ => None }
            .apply {
              val prefix = entry.getEntryArg(0)
              val rewrite = new File(entry.getEntryArg(1).stripPrefix("file:"))
              if (rewrite.exists() && prefix.startsWith("http://"))
                Some(Path(rewrite) -> prefix)
              else
                None
            }

          rewriteEntry.fold[Map[Path, String]](acc) { case (rewritePath, prefix) =>

              if (acc.contains(rewritePath))
                acc
              else
                acc + (rewritePath -> prefix)
          }

        case (acc, _) =>
          acc
      }

      val files: Set[Path]
      = rewritePaths.foldLeft[Set[Path]](Set.empty[Path]) { case (acc, (rewritePath, prefix)) =>

        val rfiles
        : Set[Path]
        = if (rewritePath.isDir)
          ls
            .rec((p: Path) => p.isFile && !omlFilePredicate(p))(rewritePath)
            .filter(omlFilePredicate.apply)
            .to[Set]
        else
          Set(rewritePath)

        verboseFiles.fold[Unit](()) { ps =>
          ps.println(s"#  lsRecCatalog: Found ${rfiles.size} OML files for Catalog entry:")
          ps.println(s"#    URI prefix: $prefix")
          ps.println(s"#  rewrite path: $rewritePath")
          val rprefix = rewritePath.toString()
          rfiles.to[Seq].sortBy(_.toString()).foreach { f =>
            val local = f.toString().stripPrefix(rprefix)
            val lpath = if (local.startsWith("/")) "."+local else local
            ps.println(s"# relative path: $lpath")
          }
          ps.println(s"# -------------------")
        }

        acc ++ rfiles
      }

      files
        .to[Seq]
        .sortBy(_.toString())
    }

    if (omlCatalogFilter(p))
      lsRecCatalog(p / up)
    else if (p.isDir)
      lsRecNoCatalog(p)
    else
      Seq.empty[Path]
  }


}
