package gov.nasa.jpl.imce.oml.converters

import java.lang.System

import gov.nasa.jpl.imce.oml.converters.text2resolver.OMLText2Resolver
import gov.nasa.jpl.imce.oml.converters.utils.OMLResourceSet
import gov.nasa.jpl.imce.oml.resolver.{api, impl}
import gov.nasa.jpl.imce.oml.uuid
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.util.EcoreUtil

import scala.{Array, StringContext, Unit}
import scala.Predef.{augmentString,String}
import scalaz._

object OMLConverter {

  def usage(): String =
    s"""
       |Usage: OMLConverter <file> where <file> is an OML file in one of 3 supported formats:
       |1) textual concrete syntax: *.oml
       |2) OWL2-DL ontology syntax: *.owl
       |3) normalized tabular syntax: *.oml.json.zip
       |An OML file in one of the above formats is loaded transitively and converted transitively to the other two formats.
     """.stripMargin

  def main(argv: Array[String]): Unit = {
    if (argv.length != 1)
      System.out.println(usage())
    else {
      val file = argv(0)
      if (file.endsWith(".oml"))
        convertFromTextualConcreteSyntax(file)
      else if (file.endsWith(".owl"))
        convertFromOntologySyntax(file)
      else if (file.endsWith(".oml.json.zip"))
        convertFromNormalizedTabularSyntax(file)
      else {
        System.err.println(s"Unrecognized OML file syntax: $file")
        System.err.println(usage())
        System.exit(-1)
      }

    }
  }

  def convertFromTextualConcreteSyntax(omlFile: String): Unit = {
    val rs = OMLResourceSet.initializeResourceSet()

    val result = for {
      e <- OMLResourceSet.loadOMLResource(rs, URI.createFileURI(omlFile))
    } yield e

    result match {
      case -\/(emfProblems) =>
        System.out.println(emfProblems.show)
        System.exit(-1)

      case \/-(extent) =>
        System.out.println(extent)
        EcoreUtil.resolveAll(rs)

        val omlUUIDg = uuid.JVMUUIDGenerator()
        implicit val f: api.OMLResolvedFactory = impl.OMLResolvedFactoryImpl(omlUUIDg)

        OMLText2Resolver.convertOMLResources2Resolver(rs) match {
          case -\/(errors) =>
            System.out.println(errors.show)

          case \/-(o2r) =>
            System.out.println(s"Mapped to the OML Resolver API!")
        }

    }

    ()
  }

  def convertFromOntologySyntax(omlFile: String): Unit = {
    System.err.println(s"convertFromOntologySyntax $omlFile")
    System.exit(-1)
  }

  def convertFromNormalizedTabularSyntax(omlFile: String): Unit = {
    System.err.println(s"convertFromNormalizedTabularSyntax: $omlFile")
    System.exit(-1)
  }
}