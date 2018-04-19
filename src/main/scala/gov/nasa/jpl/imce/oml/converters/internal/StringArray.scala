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

package gov.nasa.jpl.imce.oml.converters.internal

import scala.util.matching.Regex
import scala.Predef.{String, augmentString}

object StringArray {

  val StringArray: Regex = "\"(\\\\\\\"|\\n|\\r|[^\"]+?)\",?".r

  /**
    * Convert a plain String or a StringArray representation of an escaped Json string to a String.
    * This conversion is used in the OMLText2Resolver.convertAnnotations
    * TODO: Investigate why the OML EMF API produces AnnotationPropertyValues whose
    * value can be either a plain String or a StringArray representation of an escaped Json string.
    *
    * @param a
    * @return
    */
  def decode2StringIfNeeded(a: String): String = {
    if (null == a)
      ""
    else if (a.length < 4)
      a
    else if (! a.startsWith("[\"") || ! a.endsWith("\"]"))
      a
    else {
      val buff = new scala.collection.mutable.StringBuilder()
      StringArray.findAllMatchIn(a).foreach { m =>
        val s = m.group(1)
        s match {
          case "\\\\n" =>
            buff.append("\n")
          case "\\\\r" =>
            buff.append("\r")
          case "\\\"" =>
            buff.append("\"")
          case x =>
            buff.append(x)
        }
      }
      buff.toString
    }
  }
}
