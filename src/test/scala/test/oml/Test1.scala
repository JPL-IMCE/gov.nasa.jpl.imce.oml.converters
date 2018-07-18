package test.oml

import java.lang.System

import gov.nasa.jpl.imce.oml.converters.utils.FileSystemUtilities

import scala.StringContext

class Test1 extends OMLReasoningTest {

  "Test" should "be loading resources" in {
    val (omlCatalogScope, outFolder) = getOMLCatalogScopeAndCreateOutputFolder("Test1", FileSystemUtilities.OMLTextOrZipFilePredicate)

    log.debug(s"scope=$omlCatalogScope")
    System.out.println(s"scope=$omlCatalogScope")
    System.out.println(s"outFolder=$outFolder")



  }

}