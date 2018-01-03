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

import scala.{Any,Boolean,Int,Left,Ordering,Right}
import scala.Predef.{augmentString,String}

case class OMLConvertedModuleEdge
( importing: OMLMetadataString.ImportingModuleIRI,
  imported: OMLMetadataString.ImportedModuleIRI ) {

  override val hashCode
  : Int
  = (importing, imported).##

  override def equals(other: Any): Boolean = other match {
    case that: OMLConvertedModuleEdge =>
      (this.importing == that.importing) &&
        (this.imported == that.imported)
    case _ =>
      false
  }
}

object OMLConvertedModuleEdge {

  val TABLE_JSON_FILENAME
  : String
  = "OMLConvertedModuleEdges.json"

  import io.circe._

  implicit val ordering
  : Ordering[OMLConvertedModuleEdge]
  = new Ordering[OMLConvertedModuleEdge] {

    def compare(x: OMLConvertedModuleEdge, y: OMLConvertedModuleEdge)
    : Int
    = x.importing.compare(y.importing) match {
      case 0 =>
        x.imported.compare(y.imported)
      case c =>
        c
    }

  }

  implicit val encoder
  : Encoder[OMLConvertedModuleEdge]
  = new Encoder[OMLConvertedModuleEdge] {
    final def apply(edge: OMLConvertedModuleEdge)
    : Json
    = Json.obj(
      ("importing", OMLMetadataString.encoderImportingModuleIRI(edge.importing)),
      ("imported", OMLMetadataString.encoderImportedModuleIRI(edge.imported))
    )
  }

  implicit val decoder
  : Decoder[OMLConvertedModuleEdge]
  = new Decoder[OMLConvertedModuleEdge] {
    final def apply(c: HCursor)
    : Decoder.Result[OMLConvertedModuleEdge]
    = {
      val importingF =
        c
          .downField("importing")
          .as[OMLMetadataString.ImportingModuleIRI](OMLMetadataString.decoderImportingModuleIRI)

      val importedF =
        c
          .downField("imported")
          .as[OMLMetadataString.ImportedModuleIRI](OMLMetadataString.decoderImportedModuleIRI)

      importingF.fold[Decoder.Result[OMLConvertedModuleEdge]](
        (f) =>
          Left(f),
        (importingV) =>
          importedF.fold[Decoder.Result[OMLConvertedModuleEdge]](
            (f) =>
              Left(f),
            (importedV) =>
             Right(OMLConvertedModuleEdge(importingV, importedV))
          )
      )
    }
  }

}
