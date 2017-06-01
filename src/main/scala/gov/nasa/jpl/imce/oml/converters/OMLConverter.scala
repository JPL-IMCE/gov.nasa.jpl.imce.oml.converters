/*
 * Copyright 2017 California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * License Terms
 */

package gov.nasa.jpl.imce.oml.converters

import java.lang.System

import scala.collection.immutable._
import scala.{Array, StringContext, Unit}
import scala.Predef.{augmentString, refArrayOps, String}

object OMLConverter {

  def usage(): String =
    s"""
       |Usage:
       |
       |1) Convert OML textual concrete syntax files
       |omlConvert -cat <oml.catalog.xml> <OML Textual files>
       |
       |2) OWL2-DL ontology syntax: *.owl
       |omlConvert -cat <oml.catalog.xml> <OML Ontology files>
       |
       |3) normalized tabular syntax: *.oml.json.zip
       |omlConvert -cat <oml.catalog.xml> <OML Tabular files>
       |
       |where:
       |<oml.catalog.xml> is an OASIS XML catalog file named 'oml.catalog.xml' for resolving OML IRIs to OML files
       |<OML Tabular files> is a space-separated list of files, each ending in '.oml.json.zip'
       |<OML Textual files> is a space-separated list of files, each ending in '.oml'
       |<OML Ontologies> is a space-separated list of `iri` resolvable to `*.owl` files via the <oml.catalog.xml>
     """.stripMargin

  def main(argv: Array[String]): Unit = {
    if (argv.length == 0)
      System.out.println(usage())
    else {
      val args = argv.to[List]
      System.out.println(s"# ${args.size} args")
      args.foreach { arg =>
        System.out.println(s"# $arg")
      }

      if (args.size > 2 &&
          args.head == "-cat" &&
          args.tail.head.endsWith("oml.catalog.xml") &&
          args.tail.tail.forall(_.endsWith(".oml")))
        OMLConverterFromTextualConcreteSyntax.convert(args.tail.head,
                                                      args.tail.tail)
      else if (args.size > 2 &&
               args.head == "-cat" &&
               args.tail.head.endsWith("oml.catalog.xml") &&
               args.tail.tail.forall(_.startsWith("http")))
        OMLConverterFromOntologySyntax.convert(args.tail.head, args.tail.tail)
      else if (args.size > 2 &&
               args.head == "-cat" &&
               args.tail.head.endsWith(".oml.json.zip"))
        OMLConverterFromNormalizedTabularSyntax.convert(args.tail.head,
                                                        args.tail.tail)
      else {
        System.err.println(
          s"All supplied arguments must be OML files in the same OML format.")
        System.err.println()
        System.err.println(usage())
        System.exit(-1)
      }

    }
  }

}
