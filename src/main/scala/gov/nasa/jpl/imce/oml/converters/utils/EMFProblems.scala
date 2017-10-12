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

package gov.nasa.jpl.imce.oml.converters.utils

import org.eclipse.emf.ecore.resource.Resource

import scala.collection.immutable._
import scala.util.control.Exception
import scala.{Option,None,Some,StringContext,Unit}
import scala.Predef.{require,wrapRefArray,String}
import scalaz._, Scalaz._

case class EMFProblems
(errors: Map[Resource, List[Resource.Diagnostic]] = Map.empty,
 warnings: Map[Resource, List[Resource.Diagnostic]] = Map.empty,
 exceptions: List[java.lang.Throwable] = List.empty[java.lang.Throwable]) {

  def toThrowables
  : Set[java.lang.Throwable]
  = Set.empty[java.lang.Throwable] ++
    errors.map { case (r, ds) =>
      new java.lang.IllegalArgumentException(
        s"${ds.size} errors in ${r.getURI}: " +
          ds
            .map(d => s"${d.getLine}:${d.getColumn} in ${d.getLocation}: ${d.getMessage}")
            .mkString("\n#", "\n#", "\n"))
    } ++
    errors.map { case (r, ds) =>
      new java.lang.IllegalArgumentException(
        s"${ds.size} warnings in ${r.getURI}: " +
          ds
            .map(d => s"${d.getLine}:${d.getColumn} in ${d.getLocation}: ${d.getMessage}")
            .mkString("\n#", "\n#", "\n"))
    } ++
    exceptions.to[Set]

  def this(exception: java.lang.Throwable) = {
    this(exceptions = exception :: Nil)
    exception.fillInStackTrace()
  }

  require(errors.nonEmpty || warnings.nonEmpty || exceptions.nonEmpty)
  require(errors.forall { case (_, ds) => ds.nonEmpty })
  require(warnings.forall { case (_, ds) => ds.nonEmpty })

  def show: String = {
    val buff = new scala.collection.mutable.StringBuilder()

    @scala.annotation.tailrec
    def showException(ex: java.lang.Throwable): Unit = {
      buff ++= s"-- ${ex.getMessage}\n"
      ex.getStackTrace.foreach { st =>
        buff ++= s"\n  at $st"
      }
      Option.apply(ex.getCause) match {
        case None =>
          buff ++= "\n\n"
          ()
        case Some(c) =>
          buff ++= s"\n Caused by:"
          showException(c)
      }
    }

    if (errors.nonEmpty) {
      buff ++= s"- ${errors.size} resource(s) with errors:\n"
      errors.foreach { case (r, es) =>
        buff ++= s"-- ${es.size} error(s) in resource: ${r.getURI}:\n"
        es.foreach { e =>
          buff ++= s"--- Error at ${e.getLocation}, line: ${e.getLine}, column: ${e.getColumn}\n"
          buff ++= e.getMessage
        }
      }
    }
    if (warnings.nonEmpty) {
      buff ++= s"- ${warnings.size} resource(s) with warnings:\n"
      warnings.foreach { case (r, ws) =>
        buff ++= s"-- ${ws.size} warning(s) in resource: ${r.getURI}:\n"
        ws.foreach { w =>
          buff ++= s"--- Warning at ${w.getLocation}, line: ${w.getLine}, column: ${w.getColumn}\n"
          buff ++= w.getMessage + "\n"
        }
      }
    }
    if (exceptions.nonEmpty) {
      buff ++= s"- ${exceptions.size} exceptions have occured:\n"
      exceptions.foreach(showException)
    }
    buff.toString
  }
}

object EMFProblems {

  def nonFatalCatch[U](body: => U)
  : EMFProblems \/ U
  = Exception.nonFatalCatch[EMFProblems \/ U]
  .withApply { (cause: java.lang.Throwable) => new EMFProblems(cause).left }
  .apply(body.right)
}