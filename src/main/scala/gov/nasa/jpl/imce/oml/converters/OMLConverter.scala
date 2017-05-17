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
import scala.Predef.{augmentString,refArrayOps,String}

object OMLConverter {

  def usage(): String =
    s"""
       |Usage:
       |
       |1) Convert OML textual concrete syntax files
       |omlConvert -out <OML Tabular file> <OML Textual files>
       |
       |2) OWL2-DL ontology syntax: *.owl
       |omlConvert -out <OML Tabular file> <OML Ontology files>
       |
       |3) normalized tabular syntax: *.oml.json.zip
       |omlConvert -out <OML Catalog file> <OML Tabular files>
       |
       |where:
       |<OML Tabular file> is a file ending in '.oml.json.zip'
       |<OML Tabular files> is a space-separated list of <OML Tabular file>
       |<OML Textual files> is a space-separated list of files, each ending in '.oml'
       |<OML Ontology files> is a space-separated list of files, each ending in '.owl'
     """.stripMargin

  def main(argv: Array[String]): Unit = {
    if (argv.length == 0)
      System.out.println(usage())
    else {
      val args = argv.to[List]

      if (args.size > 2 &&
          args.head == "-out" &&
          args.tail.head.endsWith(".oml.json.zip") &&
          args.tail.tail.forall(_.endsWith(".oml")))
        OMLConverterFromTextualConcreteSyntax.convert(args.tail.head, args.tail.tail)
      else if (argv.forall(_.endsWith(".owl")))
        OMLConverterFromOntologySyntax.convert(argv)
      else if (argv.forall(_.endsWith(".oml.json.zip")))
        OMLConverterFromNormalizedTabularSyntax.convert(argv)
      else {
        System.err.println(s"All supplied arguments must be OML files in the same OML format.")
        System.err.println()
        System.err.println(usage())
        System.exit(-1)
      }

    }
  }



}