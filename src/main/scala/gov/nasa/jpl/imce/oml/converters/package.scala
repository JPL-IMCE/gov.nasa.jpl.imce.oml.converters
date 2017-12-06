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

package gov.nasa.jpl.imce.oml

import java.io.File
import java.net.{URI, URL}

import org.semanticweb.owlapi.model.IRI

import scala.{None, Option, Some}
import scala.Predef.{String, augmentString}

package object converters {

  def saveResolutionStrategyForOMLTables(resolved: String): Option[IRI] = {
    val normalized = new URI(resolved)
    val normalizedPath = normalized.toString
    val normalizedTablesPath =
      (if (normalizedPath.endsWith(".owl"))
        normalizedPath.stripSuffix(".owl")
      else if (normalizedPath.endsWith("/"))
        normalizedPath.stripSuffix("/")
      else normalizedPath) + ".oml.json.zip"

    val f1 = new URL(normalizedTablesPath)
    val outputFile =
      if (resolved.startsWith("file:")) new File(resolved.substring(5))
      else new File(resolved)

    outputFile.getParentFile match {
      case null =>
        None

      case outputDir =>
        if (!outputDir.exists)
          outputDir.mkdirs

        if (outputDir.exists && outputDir.isDirectory && outputDir.canWrite)
          Some(IRI.create(f1.toString))
        else
          None
    }
  }

  def tables2emf
  (v: tables.LiteralValue)
  : model.common.LiteralValue
  = v.literalType match {
    case tables.LiteralBooleanType =>
      val d = model.common.CommonFactory.eINSTANCE.createLiteralBoolean()
      d.setBool(java.lang.Boolean.getBoolean(v.value))
      d
    case tables.LiteralDateTimeType =>
      val d = model.common.CommonFactory.eINSTANCE.createLiteralDateTime()
      d.setDateTime(new model.datatypes.DateTimeValue(v.value))
      d
    case tables.LiteralStringType =>
      val d = model.common.CommonFactory.eINSTANCE.createLiteralRawString()
      d.setString(new model.datatypes.RawStringValue(v.value))
      d
    case tables.LiteralUUIDType =>
      val d = model.common.CommonFactory.eINSTANCE.createLiteralUUID()
      d.setUuid(new model.datatypes.UUIDValue(v.value))
      d
    case tables.LiteralURIType =>
      val d = model.common.CommonFactory.eINSTANCE.createLiteralURI()
      d.setUri(new model.datatypes.URIValue(v.value))
      d
    case tables.LiteralRealType =>
      val d = model.common.CommonFactory.eINSTANCE.createLiteralReal()
      d.setReal(new model.datatypes.RealValue(v.value))
      d
    case tables.LiteralRationalType =>
      val d = model.common.CommonFactory.eINSTANCE.createLiteralRational()
      d.setRational(new model.datatypes.RationalValue(v.value))
      d
    case tables.LiteralFloatType =>
      val d = model.common.CommonFactory.eINSTANCE.createLiteralFloat()
      d.setFloat(new model.datatypes.FloatValue(v.value))
      d
    case tables.LiteralPositiveIntegerType =>
      val d = model.common.CommonFactory.eINSTANCE.createLiteralDecimal()
      d.setDecimal(new model.datatypes.PositiveIntegerValue(v.value))
      d
    case tables.LiteralDecimalType =>
      val d = model.common.CommonFactory.eINSTANCE.createLiteralDecimal()
      d.setDecimal(new model.datatypes.DecimalValue(v.value))
      d
  }

  def tables2emf
  (v: tables.LiteralNumber)
  : model.common.LiteralNumber
  = v.literalType match {
    case tables.LiteralRealType =>
      val d = model.common.CommonFactory.eINSTANCE.createLiteralReal()
      d.setReal(new model.datatypes.RealValue(v.value))
      d
    case tables.LiteralRationalType =>
      val d = model.common.CommonFactory.eINSTANCE.createLiteralRational()
      d.setRational(new model.datatypes.RationalValue(v.value))
      d
    case tables.LiteralFloatType =>
      val d = model.common.CommonFactory.eINSTANCE.createLiteralFloat()
      d.setFloat(new model.datatypes.FloatValue(v.value))
      d
    case tables.LiteralPositiveIntegerType =>
      val d = model.common.CommonFactory.eINSTANCE.createLiteralDecimal()
      d.setDecimal(new model.datatypes.PositiveIntegerValue(v.value))
      d
    case tables.LiteralDecimalType =>
      val d = model.common.CommonFactory.eINSTANCE.createLiteralDecimal()
      d.setDecimal(new model.datatypes.DecimalValue(v.value))
      d
  }

  def tables2emf
  (v: tables.LiteralDateTime)
  : model.common.LiteralDateTime
  = {
    val d = model.common.CommonFactory.eINSTANCE.createLiteralDateTime()
    d.setDateTime(new model.datatypes.DateTimeValue(v.value))
    d
  }

  def emf2tables
  (v: model.common.LiteralValue)
  : tables.LiteralValue
  = v match {
    case d: model.common.LiteralBoolean =>
      tables.LiteralValue(tables.LiteralBooleanType,if (d.isBool) "true" else "false")
    case d: model.common.LiteralDateTime =>
      tables.LiteralValue(tables.LiteralDateTimeType,d.getDateTime.value)
    case d: model.common.LiteralString =>
      tables.LiteralValue(tables.LiteralStringType,d.value())
    case d: model.common.LiteralUUID =>
      tables.LiteralValue(tables.LiteralUUIDType,d.getUuid.value)
    case d: model.common.LiteralURI =>
      tables.LiteralValue(tables.LiteralURIType,d.getUri.value)
    case d: model.common.LiteralReal =>
      tables.LiteralValue(tables.LiteralRealType,d.getReal.value)
    case d: model.common.LiteralRational =>
      tables.LiteralValue(tables.LiteralRationalType,d.getRational.value)
    case d: model.common.LiteralFloat =>
      tables.LiteralValue(tables.LiteralFloatType,d.getFloat.value)
    case d: model.common.LiteralDecimal =>
      d.getDecimal match {
        case dv: model.datatypes.PositiveIntegerValue =>
          tables.LiteralValue(tables.LiteralPositiveIntegerType,dv.value)
        case dv: model.datatypes.DecimalValue =>
          tables.LiteralValue(tables.LiteralDecimalType,dv.value)
      }
  }

  def emf2tables
  (v: model.common.LiteralNumber)
  : tables.LiteralNumber
  = v match {
    case d: model.common.LiteralReal =>
      tables.LiteralNumber(tables.LiteralRealType,d.getReal.value)
    case d: model.common.LiteralRational =>
      tables.LiteralNumber(tables.LiteralRationalType,d.getRational.value)
    case d: model.common.LiteralFloat =>
      tables.LiteralNumber(tables.LiteralFloatType,d.getFloat.value)
    case d: model.common.LiteralDecimal =>
      d.getDecimal match {
        case dv: model.datatypes.PositiveIntegerValue =>
          tables.LiteralNumber(tables.LiteralPositiveIntegerType,dv.value)
        case dv: model.datatypes.DecimalValue =>
          tables.LiteralNumber(tables.LiteralDecimalType,dv.value)
      }
  }

  def emf2tables
  (d: model.datatypes.PositiveIntegerValue)
  : tables.taggedTypes.PositiveIntegerLiteral
  = tables.taggedTypes.positiveIntegerLiteral(d.value)

  def emf2tables
  (d: model.common.LiteralDateTime)
  : tables.LiteralDateTime
  = tables.LiteralDateTime(d.getDateTime.value)

  def emf2tables
  (d: model.datatypes.LanguageTagValue)
  : tables.taggedTypes.LanguageTagDataType
  = tables.taggedTypes.languageTagDataType(d.value)

  def emf2tables
  (d: model.datatypes.PatternValue)
  : tables.taggedTypes.LiteralPattern
  = tables.taggedTypes.literalPattern(d.value)
}
