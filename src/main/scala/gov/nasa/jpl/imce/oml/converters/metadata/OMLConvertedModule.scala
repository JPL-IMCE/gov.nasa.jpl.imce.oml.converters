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

package gov.nasa.jpl.imce.oml.converters.metadata

import io.circe._

import scala.{Any,Boolean,Int,Left,Ordering,Right}
import scala.Predef.{augmentString,String}

case class OMLConvertedModule
( iri: OMLMetadataString.ModuleIRI,
  filename: OMLMetadataString.RelativeFilename,
  provenance: OMLMetadataProvenance) {

  override val hashCode
  : Int
  = (iri, filename, provenance).##
  
  override def equals(other: Any): Boolean = other match {
  	case that: OMLConvertedModule =>
  	  (this.iri == that.iri) &&
  	  (this.filename == that.filename) &&
  	  (this.provenance == that.provenance)
    case _ =>
      false
  }
  
}

object OMLConvertedModule {

  val TABLE_JSON_FILENAME 
  : String
  = "OMLConvertedModules.json"

  implicit val ordering
  : Ordering[OMLConvertedModule]
  = new Ordering[OMLConvertedModule] {

    def compare(x: OMLConvertedModule, y: OMLConvertedModule)
    : Int
    = OMLMetadataProvenance.ordering.compare(x.provenance, y.provenance) match {
      case 0 =>
        x.iri.compare(y.iri)
      case c =>
        c
    }
  }

  implicit val encoder
  : Encoder[OMLConvertedModule]
  = new Encoder[OMLConvertedModule] {
    final def apply(m: OMLConvertedModule)
    : Json
    = Json.obj(
      ("iri", OMLMetadataString.encoderModuleIRI(m.iri)),
      ("filename", OMLMetadataString.encoderRelativeFilename(m.filename)),
      ("provenance", OMLMetadataProvenance.encoder(m.provenance))
    )
  }

  implicit val decoder
  : Decoder[OMLConvertedModule]
  = new Decoder[OMLConvertedModule] {
    final def apply(c: HCursor)
    : Decoder.Result[OMLConvertedModule]
    = {
      val iriF =
        c
          .downField("iri")
          .as[OMLMetadataString.ModuleIRI](OMLMetadataString.decoderModuleIRI)

      val filenameF =
        c
          .downField("filename")
          .as[OMLMetadataString.RelativeFilename](OMLMetadataString.decoderRelativeFilename)

      val provenanceF =
        c
          .downField("provenance")
          .as[OMLMetadataProvenance](OMLMetadataProvenance.decoder)

      iriF.fold[Decoder.Result[OMLConvertedModule]](
        (f) =>
          Left(f),
        (iriV) =>
          filenameF.fold[Decoder.Result[OMLConvertedModule]](
            (f) =>
              Left(f),
            (filenameV) =>
              provenanceF.fold[Decoder.Result[OMLConvertedModule]](
                (f) =>
                  Left(f),
                (provenanceV) =>
                  Right(OMLConvertedModule(iriV, filenameV, provenanceV))
              )
          )
      )
    }
  }

}	
