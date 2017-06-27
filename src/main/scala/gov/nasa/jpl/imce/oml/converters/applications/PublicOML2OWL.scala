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

object PublicOML2OWL {

  def main(argv: Array[String]): Unit = {
    val args: Array[String] = Array[String](
      "-cat",
      "oml.catalog.xml",
      "-out",
      "oml.metadata.json",
      "./purl.org/dc/elements/1.1.oml",
      "./www.w3.org/2001/XMLSchema.oml",
      "./www.w3.org/2000/01/rdf-schema.oml",
      "./www.w3.org/1999/02/22-rdf-syntax-ns.oml",
      "./www.w3.org/2002/07/owl.oml",
      "./imce.jpl.nasa.gov/oml/runtime.oml",
      "./imce.jpl.nasa.gov/oml/provenance/MagicDraw.oml",
      "./imce.jpl.nasa.gov/oml/oml.oml",
      "./imce.jpl.nasa.gov/foundation/annotation/annotation.oml",
      "./imce.jpl.nasa.gov/foundation/base/base.oml",
      "./imce.jpl.nasa.gov/foundation/behavior/behavior.oml",
      "./imce.jpl.nasa.gov/foundation/mission/mission.oml",
      "./imce.jpl.nasa.gov/foundation/analysis/analysis.oml",
      "./imce.jpl.nasa.gov/discipline/mechanical/mechanical.oml",
      "./imce.jpl.nasa.gov/foundation/project/project.oml",
      "./imce.jpl.nasa.gov/discipline/vandv/vandv.oml",
      "./imce.jpl.nasa.gov/discipline/fault-management/fault-management.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/PrimitiveTypes/20110701/PrimitiveTypes.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/UML.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/Metrology.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/StandardProfileL2.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/SysML.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/QUDV.oml",
      "./imce.jpl.nasa.gov/math/math.oml",

      "./imce.jpl.nasa.gov/foundation/omf/omf.oml",
      "./imce.jpl.nasa.gov/foundation/owl2-mof2/datatype-map.oml",
      "./imce.jpl.nasa.gov/foundation/owl2-mof2/owl2-mof2-embedding.oml",
      "./imce.jpl.nasa.gov/foundation/owl2-mof2/owl2-mof2.oml",
      "./imce.jpl.nasa.gov/foundation/base/base-embedding.oml",
      "./imce.jpl.nasa.gov/discipline/fault-management/fault-management-embedding.oml",
      "./imce.jpl.nasa.gov/discipline/mechanical/mechanical-embedding.oml",
      "./imce.jpl.nasa.gov/foundation/mission/mission-embedding.oml",
      "./imce.jpl.nasa.gov/discipline/vandv/vandv-embedding.oml",
      "./imce.jpl.nasa.gov/foundation/analysis/analysis-embedding.oml",
      "./imce.jpl.nasa.gov/foundation/behavior/behavior-embedding.oml",
      "./imce.jpl.nasa.gov/foundation/project/project-embedding.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/PrimitiveTypes/20110701/PrimitiveTypes-embedding.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/PrimitiveTypes/20110701/PrimitiveTypes-metamodel.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/UML-embedding.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/UML-metamodel.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/Metrology-embedding.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/Metrology-metamodel.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/StandardProfileL2-embedding.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/StandardProfileL2-metamodel.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/SysML-embedding.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/SysML-metamodel.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/QUDV-embedding.oml",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/QUDV-metamodel.oml",
      "./imce.jpl.nasa.gov/math/math-embedding.oml"
    )

    OMLConverter.main(args)
  }
}
