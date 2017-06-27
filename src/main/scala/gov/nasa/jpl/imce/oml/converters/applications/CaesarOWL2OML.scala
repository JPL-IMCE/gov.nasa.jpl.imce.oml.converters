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

package gov.nasa.jpl.imce.oml.converters.applications

import gov.nasa.jpl.imce.oml.converters.OMLConverter
import scala.{Array,Unit}
import scala.Predef.String

object CaesarOWL2OML {

  def main(argv: Array[String]): Unit = {
    val args: Array[String] = Array[String](
      "-cat",
      "oml.catalog.xml",
      "-out",
      "oml.metadata.json",
      "./imce.jpl.nasa.gov/discipline/mass_management.owl",
      "./imce.jpl.nasa.gov/foundation/annotation/annotation.owl",
      "./imce.jpl.nasa.gov/foundation/base/base.owl",
      "./imce.jpl.nasa.gov/foundation/mission/mission.owl",
      "./imce.jpl.nasa.gov/foundation/project/project-bundle.owl",
      "./imce.jpl.nasa.gov/foundation/project/project.owl",
      "./imce.jpl.nasa.gov/projects/MassManagementDemonstration/DIMM.owl",
      "./imce.jpl.nasa.gov/oml/provenance/MagicDraw.owl",
      "./imce.jpl.nasa.gov/oml/oml.owl",
      "./imce.jpl.nasa.gov/oml/runtime.owl",
      "./purl.org/dc/elements/1.1.owl",
      "./www.w3.org/2001/XMLSchema.owl"
    )

    OMLConverter.main(args)
  }
}
