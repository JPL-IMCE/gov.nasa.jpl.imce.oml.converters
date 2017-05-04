package gov.nasa.jpl.imce.oml.converters.utils

import org.eclipse.emf.ecore.resource.Resource

import scala.collection.immutable._
import scala.collection.mutable.StringBuilder
import scala.util.control.Exception
import scala.StringContext
import scala.Predef.{require,wrapRefArray,String}
import scalaz._, Scalaz._

case class EMFProblems
(errors: Map[Resource, List[Resource.Diagnostic]] = Map.empty,
 warnings: Map[Resource, List[Resource.Diagnostic]] = Map.empty,
 exceptions: List[java.lang.Throwable] = List.empty[java.lang.Throwable]) {

  def this(exception: java.lang.Throwable) = this(exceptions = exception :: Nil)

  require(errors.nonEmpty || warnings.nonEmpty || exceptions.nonEmpty)
  require(errors.forall { case (_, ds) => ds.nonEmpty })
  require(warnings.forall { case (_, ds) => ds.nonEmpty })

  def show: String = {
    val buff = new StringBuilder()
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
      exceptions.foreach { ex =>
        buff ++= s"-- ${ex.getMessage}\n"
        ex.fillInStackTrace()
        ex.getStackTrace.foreach { st =>
          buff ++= s"  at $st"
        }
      }
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