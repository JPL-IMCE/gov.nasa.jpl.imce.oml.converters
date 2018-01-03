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

package gov.nasa.jpl.imce.oml.processor.metadata

import scala.{Int,Ordering}

/**
  * Specifies the provenance of an OMLConvertedModule
  */
sealed trait OMLMetadataProvenance

object OMLMetadataProvenance {

  /**
    * An OMLConvertedModule with OMLBuiltinModuleProvenance
    * corresponds to an OML TerminologyGraph ontology that defines either datatypes
    * that are part of the OWL2-DL datatype map or annotation properties
    * that are used in defining the OWL2-DL datatype map or the OWL2 vocabulary.
    */
  case object OMLBuiltinModuleProvenance extends OMLMetadataProvenance

  /**
    * An OMLConvertedModule with OMLBundleModuleProvenance
    * corresponds to an OML TerminologyBox
    * that is either an OML Bundle or an OML TerminologyBox that
    * is directly or indirectly imported by an OML Bundle
    */
  case object OMLBundleModuleProvenance extends OMLMetadataProvenance

  /**
    * An OMLConvertedModule with OMLExtensionModuleProvenance
    * corresponds to an OML Module that directly or indirectly
    * imports an OML Bundle and is not directly or indirectly imported by an OML Bundle.
    */
  case object OMLExtensionModuleProvenance extends OMLMetadataProvenance

  /**
    * An OMLConvertedModule with OMLOtherModuleProvenance
    * corresponds to an OML Module that is not OMLBuiltinModuleProvenance,
    * OMLBundleModuleProvenance or OMLExtensionModuleProvenance
    */
  case object OMLOtherModuleProvenance extends OMLMetadataProvenance

  implicit val ordering
  : Ordering[OMLMetadataProvenance]
  = new Ordering[OMLMetadataProvenance] {

    def compare(x: OMLMetadataProvenance, y: OMLMetadataProvenance)
    : Int
    = if (x == y) 0
    else x match {
      case OMLBuiltinModuleProvenance =>
        -1
      case OMLBundleModuleProvenance =>
        y match {
          case (OMLExtensionModuleProvenance | OMLOtherModuleProvenance) =>
            -1
          case _ =>
            1
        }
      case OMLExtensionModuleProvenance =>
        y match {
          case OMLOtherModuleProvenance =>
            -1
          case _ =>
            1
        }
      case OMLOtherModuleProvenance =>
        1
    }
  }

  import io.circe._

  implicit val decoder
  : Decoder[OMLMetadataProvenance]
  = generic.semiauto.deriveDecoder[OMLMetadataProvenance]

  implicit val encoder
  : Encoder[OMLMetadataProvenance]
  = new Encoder[OMLMetadataProvenance] {
    final def apply(a: OMLMetadataProvenance): Json
    = Json.fromString(a.toString)
  }
}