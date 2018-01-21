package gov.nasa.jpl.imce.oml.converters

import java.lang.System
import ammonite.ops._
import org.scalatest.Assertions.intercept
import scala.collection.immutable.{Iterable,Seq,Vector}
import scala.{Either,Int,None,Some,StringContext,Unit}
import scala.Predef.{augmentString,require,ArrowAssoc,String}

case object DiffConversionsCommand {

  val differLine = "Files (.*) and (.*) differ".r
  val inflating = "^ *inflating: ([^ ]*) *$".r

  def diff(dir1: Path, dir2: Path): Unit = {
    val prefix1 = dir1.toString()
    val prefix2 = dir2.toString()
    System.out.println(s"Diff:\ndir1=$dir1\ndir2=$dir2")

    implicit val pw = pwd
    val diffs = intercept[ShelloutException] {
      // If there are differences (i.e., non-zero exit), this will throw ShelloutException
      %% diff("-rq", dir1.toString, dir2.toString)

      // If there are no differences, then throw a ShelloutException with no output.
      val chunks = scala.collection.mutable.Buffer.empty[Either[Bytes, Bytes]]
      throw ShelloutException(CommandResult(0, chunks))
    }

    val lines = diffs.result.out.lines
    val omlDiffs = lines.filter(_.contains(".oml ")).map { line =>
      val differLine(f1, _) = line
      f1.stripPrefix(prefix1)
    }.sorted
    val owlDiffs = lines.filter(_.contains(".owl ")).map { line =>
      val differLine(f1, _) = line
      f1.stripPrefix(prefix1)
    }.sorted
    val zipDiffs = lines.filter(_.contains(".zip "))

    System.out.println(s"\n=> ${omlDiffs.size} differences for *.oml")
    omlDiffs.foreach(System.out.println)

    System.out.println(s"\n=> ${owlDiffs.size} differences for *.owl")
    owlDiffs.foreach(System.out.println)

    val zips = zipDiffs.flatMap { line =>
      val differLine(f1, f2) = line
      val u1 = %% unzip("-cq", f1)
      val u2 = %% unzip("-cq", f2)
      if (u1.out.string == u2.out.string)
        None
      else
        Some(f1 -> f2)
    }
    System.out.println(s"\n=> ${zips.size} differences for *.omlzip")
    zips.foreach { case (f1, f2) =>
      System.out.println(s"\n==> ${f1.stripPrefix(prefix1)}")
      val u1 = (%% unzip("-c", f1)).out.lines.drop(1)
      val u2 = (%% unzip("-c", f2)).out.lines.drop(1)
      val fileLines1 = jsonFileLines(u1)
      val fileLines2 = jsonFileLines(u2)
      require(fileLines1.size == fileLines2.size)
      fileLines1.zip(fileLines2).foreach { case ((file1, lines1), (file2, lines2)) =>
        require(file1 == file2)
        val diffs = diffLines(prefix1, lines1.zipWithIndex, prefix2, lines2.zipWithIndex)
        if (diffs.nonEmpty) {
          System.out.println(s"\n===> $file1")
          diffs.foreach(System.out.println)
        }
      }
    }
  }

  @scala.annotation.tailrec
  final def diffLines
  (prefix1: String,
   lines1: Vector[(String, Int)],
   prefix2: String,
   lines2: Vector[(String, Int)],
   diffs: Seq[String] = Seq.empty)
  : Seq[String]
  = (lines1.isEmpty, lines2.isEmpty) match {
    case (false, false) =>
      val (d1, d2) = findDiff(lines1, lines2)
      val upto1 = if (-1 == d1) Vector.empty else lines1.drop(d1)
      val upto2 = if (-1 == d2) Vector.empty else lines2.drop(d2)

      (d1, d2) match {
        case (-1, -1) =>
          diffs
        case (_, -1) =>
          (diffs :+ s"... in $prefix1 @ ${upto1.head._2}:") ++ upto1.map(_._1)
        case (-1, _) =>
          (diffs :+ s"... in $prefix2 @ ${upto2.head._2}:") ++ upto2.map(_._1)
        case (_, _) =>
          val (next1, next2, nextDiffs) = explainDiffs(prefix1, lines1, prefix2, lines2, diffs)
          diffLines(prefix1, next1, prefix2, next2, nextDiffs)
      }
    case (true, false) =>
      (diffs :+ s"... in $prefix2 @ ${lines2.head._2}:") ++ lines2.map(_._1)
    case (false, true) =>
      (diffs :+ s"... in $prefix1 @ ${lines1.head._2}:") ++ lines1.map(_._1)
    case (true, true) =>
      diffs
  }

  @scala.annotation.tailrec
  final def explainDiffs
  (prefix1: String,
   lines1: Vector[(String, Int)],
   prefix2: String,
   lines2: Vector[(String, Int)],
   diffs: Seq[String])
  : (Vector[(String, Int)], Vector[(String, Int)], Seq[String])
  = if (lines1.isEmpty && lines2.isEmpty)
    (Vector.empty, Vector.empty, diffs)
  else if (lines1.isEmpty && lines2.nonEmpty) {
    val result = (diffs :+ s"... in $prefix2 @ ${lines2.head._2}:") ++ lines2.map(_._1)
    (Vector.empty, Vector.empty, result)
  } else if (lines1.nonEmpty && lines2.isEmpty) {
    val result = (diffs :+ s"... in $prefix1 @ ${lines1.head._2}:") ++ lines1.map(_._1)
    (Vector.empty, Vector.empty, result)
  } else if (lines1.head._1 == lines2.head._1)
    explainDiffs(prefix1, lines1.drop(1), prefix2, lines2.drop(1), diffs)
  else {
    val next =
    diffs ++ Seq(
      s"... in $prefix1 @ ${lines1.head._2}:",
      lines1.head._1,
      s"... in $prefix2 @ ${lines2.head._2}:",
      lines2.head._1)
    explainDiffs(prefix1, lines1.drop(1), prefix2, lines2.drop(1), next)
  }

  @scala.annotation.tailrec
  final def findDiff
  (lines1: Iterable[(String, Int)],
   lines2: Iterable[(String, Int)],
   n1: Int = 0,
   n2: Int = 0)
  : (Int, Int)
  = if (lines1.isEmpty && lines2.isEmpty)
    -1 -> -1
  else if (lines1.isEmpty)
    -1 -> n2
  else if (lines2.isEmpty)
    n1 -> -1
  else if (lines1.head._1 != lines2.head._1)
    n1 -> n1
  else
    findDiff(lines1.tail, lines2.tail, 1+n1, 1+n2)

  @scala.annotation.tailrec
  final def jsonFileLines
  (lines: Vector[String],
   acc: Seq[(String, Vector[String])] = Seq.empty)
  : Seq[(String, Vector[String])]
  = if (lines.isEmpty)
    acc
  else {
    val inflating(file) = lines.head
    val rest = lines.drop(1)
    val next = rest.indexWhere(l => 0 == l.size)
    val upto = if (-1 == next) rest.size else next
    val sublines = lines.slice(1, 1+upto)
    val remainder = lines.drop(2+upto)
    jsonFileLines(remainder, acc :+ (file -> sublines))
  }
}
