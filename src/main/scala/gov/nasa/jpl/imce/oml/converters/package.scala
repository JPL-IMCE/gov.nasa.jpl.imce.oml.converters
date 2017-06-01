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

package gov.nasa.jpl.imce.oml

import java.io.File
import java.net.{URI, URL}

import org.semanticweb.owlapi.model.IRI

import scala.{Option,None,Some}
import scala.Predef.{augmentString,String}

package object converters {

  def saveResolutionStrategyForOMLTables(resolved: String): Option[IRI] = {
    val normalized = new URI(resolved)
    val normalizedPath = normalized.toString
    val normalizedTablesPath =
      (if (normalizedPath.endsWith(".owl"))
        normalizedPath.stripSuffix(".owl")
      else normalizedPath) + ".oml.json.zip"

    val f1 = new URL(normalizedTablesPath)
    val outputFile =
      if (resolved.startsWith("file:")) new File(resolved.substring(5))
      else new File(resolved)

    outputFile.getParentFile match {
      case null =>
        None

      case outputDir =>
        if (!outputDir.exists)
          outputDir.mkdirs

        if (outputDir.exists && outputDir.isDirectory && outputDir.canWrite)
          Some(IRI.create(f1.toString))
        else
          None
    }
  }
}
