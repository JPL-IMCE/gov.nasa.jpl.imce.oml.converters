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

import scala.collection.immutable.Seq
import scala.{Any,Boolean,Int}
import scala.Predef.String

case class OMLMetadataGraph
( nodes: Seq[OMLConvertedModule],
  edges: Seq[OMLConvertedModuleEdge] ) {

  override val hashCode
  : Int
  = (nodes, edges).##

  override def equals(other: Any): Boolean = other match {
    case that: OMLMetadataGraph =>
      (this.nodes == that.nodes) &&
        (this.edges == that.edges)
    case _ =>
      false
  }
}

object OMLMetadataGraph {

  val TABLE_JSON_FILENAME
  : String
  = "oml.metadata.json"

  import io.circe._, io.circe.generic.semiauto._

  implicit val encoder
  : Encoder[OMLMetadataGraph]
  = new Encoder[OMLMetadataGraph] {
    final def apply(a: OMLMetadataGraph): Json = Json.obj(
      ("nodes", Json.arr(a.nodes.map(n => OMLConvertedModule.encoder(n)): _*)),
      ("edges", Json.arr(a.edges.map(e => OMLConvertedModuleEdge.encoder(e)): _*))
    )
  }

  implicit val decoder
  : Decoder[OMLMetadataGraph]
  = deriveDecoder[OMLMetadataGraph]

}