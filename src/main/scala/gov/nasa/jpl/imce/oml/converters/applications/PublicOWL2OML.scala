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

object PublicOWL2OML {

  def main(argv: Array[String]): Unit = {
    val args: Array[String] = Array[String](
      "-cat",
      "oml.catalog.xml",
      "-out",
      "oml.metadata.json",
      "./imce.jpl.nasa.gov/discipline/fault-management/fault-management-embedding.owl",
      "./imce.jpl.nasa.gov/discipline/fault-management/fault-management.owl",
      "./imce.jpl.nasa.gov/discipline/mechanical/mechanical-embedding.owl",
      "./imce.jpl.nasa.gov/discipline/mechanical/mechanical.owl",
      "./imce.jpl.nasa.gov/discipline/vandv/vandv-embedding.owl",
      "./imce.jpl.nasa.gov/discipline/vandv/vandv.owl",
      "./imce.jpl.nasa.gov/foundation/analysis/analysis-embedding.owl",
      "./imce.jpl.nasa.gov/foundation/analysis/analysis.owl",
      "./imce.jpl.nasa.gov/foundation/annotation/annotation.owl",
      "./imce.jpl.nasa.gov/foundation/base/base-embedding.owl",
      "./imce.jpl.nasa.gov/foundation/base/base.owl",
      "./imce.jpl.nasa.gov/foundation/behavior/behavior-embedding.owl",
      "./imce.jpl.nasa.gov/foundation/behavior/behavior.owl",
      "./imce.jpl.nasa.gov/foundation/mission/mission-embedding.owl",
      "./imce.jpl.nasa.gov/foundation/mission/mission.owl",
      "./imce.jpl.nasa.gov/foundation/omf/omf.owl",
      "./imce.jpl.nasa.gov/foundation/owl2-mof2/datatype-map.owl",
      "./imce.jpl.nasa.gov/foundation/owl2-mof2/owl2-mof2-embedding.owl",
      "./imce.jpl.nasa.gov/foundation/owl2-mof2/owl2-mof2.owl",
      "./imce.jpl.nasa.gov/foundation/project/project-embedding.owl",
      "./imce.jpl.nasa.gov/foundation/project/project.owl",
      "./imce.jpl.nasa.gov/math/math-mapping.owl",
      "./imce.jpl.nasa.gov/math/math.owl",
      "./imce.jpl.nasa.gov/oml/oml.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/PrimitiveTypes/20110701/PrimitiveTypes-embedding.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/PrimitiveTypes/20110701/PrimitiveTypes-metamodel.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/PrimitiveTypes/20110701/PrimitiveTypes.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/Metrology-embedding.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/Metrology-metamodel.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/Metrology.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/QUDV-embedding.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/QUDV-metamodel.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/QUDV.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/SysML-embedding.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/SysML-metamodel.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/SysML/20140311/SysML.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/StandardProfileL2-embedding.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/StandardProfileL2-metamodel.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/StandardProfileL2.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/UML-embedding.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/UML-metamodel.owl",
      "./imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/UML.owl",
      "./purl.org/dc/elements/1.1.owl"
    )

    OMLConverter.main(args)
  }
}
