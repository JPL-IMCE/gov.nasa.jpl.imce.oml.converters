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

import scala.Predef.{String, identity}

/**
  * OMLString defines multiple tag types on String:
  * - LocalName
  * - LexicalValue
  * - LexicalNumber
  * - LexicalTime
  * - Pattern
  * - LangRange
  * - AbbrevIRI
  * - NamespacePrefix
  *
  * Since these String-tagged types are disjoint from each other,
  * there is a compile-time guarantee that a particular tagged-String value can't be misused
  * as a legimate String of a different tag.
  * (e.g., an AbbrevIRI String value can't be used as an argument for a function accepting a Pattern String)
  *
  * @see https://code.launchpad.net/~scompall/+junk/high-cost-of-anyval-subclasses
  * @see https://failex.blogspot.com/2017/04/the-high-cost-of-anyval-subclasses.html
  */
object OMLMetadataString {

  sealed abstract class OMLMetadataStringImpl {
    type T <: String

    def apply(s: String): T

    def unwrap(lbl: T): String

    /**
      * Convert an F[_] structure of Strings to an F[_] structure of T in constant time.
      */
    def subst[F[_]](fs: F[String]): F[T]

    // In S. Compall's article, the code is written with the Scala kind-projector compiler plugin:
    //    Lambda[x => F[x] => F[String]]
    // Without this plugin, we have to write instead:
    //    ({type U[x] = F[x] => F[String]})#U
    /**
      * Convert an F[_] structure of T to an F[String] structure in constant time.
      */
    def untag[F[_]](f: F[T]): F[String] = subst[({type U[x] = F[x] => F[String]})#U](identity)(f)
  }

  /**
    * The IRI of an OML Module
    */
  val ModuleIRI : OMLMetadataStringImpl = new OMLMetadataStringImpl {

    override type T = String

    override def apply(s: String): T = s

    override def unwrap(lbl: T): String = lbl

    override def subst[F[_]](fs: F[String]): F[T] = fs

  }

  type ModuleIRI = ModuleIRI.T



  /**
    * The IRI of an importing OML Module
    */
  val ImportingModuleIRI : OMLMetadataStringImpl = new OMLMetadataStringImpl {

    override type T = String

    override def apply(s: String): T = s

    override def unwrap(lbl: T): String = lbl

    override def subst[F[_]](fs: F[String]): F[T] = fs

  }

  type ImportingModuleIRI = ImportingModuleIRI.T


  /**
    * The IRI of an imported OML Module
    */
  val ImportedModuleIRI : OMLMetadataStringImpl = new OMLMetadataStringImpl {

    override type T = String

    override def apply(s: String): T = s

    override def unwrap(lbl: T): String = lbl

    override def subst[F[_]](fs: F[String]): F[T] = fs

  }

  type ImportedModuleIRI = ImportedModuleIRI.T

  /**
    * The relative filename of an OML Module (without extension: .oml, .oml.json.zip, .owl)
    */
  val RelativeFilename : OMLMetadataStringImpl = new OMLMetadataStringImpl {

    override type T = String

    override def apply(s: String): T = s

    override def unwrap(lbl: T): String = lbl

    override def subst[F[_]](fs: F[String]): F[T] = fs

  }

  type RelativeFilename = RelativeFilename.T

  import scala.util.Either
  import cats.syntax.either._
  import io.circe._

  implicit val decoderModuleIRI
  : Decoder[ModuleIRI]
  = Decoder.decodeString.emap { str =>
    Either.catchNonFatal(ModuleIRI(str)).leftMap(t => "ModuleIRI")
  }

  implicit val encoderModuleIRI
  : Encoder[ModuleIRI]
  = Encoder.encodeString.contramap[ModuleIRI](_.toString)

  implicit val decoderImportingModuleIRI
  : Decoder[ImportingModuleIRI]
  = Decoder.decodeString.emap { str =>
    Either.catchNonFatal(ImportingModuleIRI(str)).leftMap(t => "ImportingModuleIRI")
  }

  implicit val encoderImportingModuleIRI
  : Encoder[ImportingModuleIRI]
  = Encoder.encodeString.contramap[ImportingModuleIRI](_.toString)

  implicit val decoderImportedModuleIRI
  : Decoder[ImportedModuleIRI]
  = Decoder.decodeString.emap { str =>
    Either.catchNonFatal(ImportedModuleIRI(str)).leftMap(t => "ImportedModuleIRI")
  }

  implicit val encoderImportedModuleIRI
  : Encoder[ImportedModuleIRI]
  = Encoder.encodeString.contramap[ImportedModuleIRI](_.toString)

  implicit val decoderRelativeFilename
  : Decoder[RelativeFilename]
  = Decoder.decodeString.emap { str =>
    Either.catchNonFatal(RelativeFilename(str)).leftMap(t => "RelativeFilename")
  }

  implicit val encoderRelativeFilename
  : Encoder[RelativeFilename]
  = Encoder.encodeString.contramap[RelativeFilename](_.toString)

}
