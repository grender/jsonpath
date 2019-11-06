/*
 * Copyright 2011-2019 GatlingCorp (https://gatling.io)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.gatling.jsonpath

import java.lang.{ StringBuilder => JStringBuilder }

import io.circe.Json

import scala.util.parsing.combinator.RegexParsers
import io.gatling.jsonpath.AST._

object FastStringOps {

  private val StringBuilderPool = new ThreadLocal[JStringBuilder] {
    override def initialValue(): JStringBuilder = new JStringBuilder
  }

  private def pooledStringBuilder = {
    val sb = StringBuilderPool.get()
    sb.setLength(0)
    sb
  }

  implicit class RichString(val text: String) extends AnyVal {
    def fastReplaceAll(searchString: String, replacement: String): String =
      if (searchString.isEmpty || replacement.isEmpty) {
        text
      } else {
        var start = 0
        var end = text.indexOf(searchString, start)
        if (end == -1) {
          text
        } else {
          val buf = pooledStringBuilder
          while (end != -1) {
            buf.append(text, start, end).append(replacement)
            start = end + searchString.length
            end = text.indexOf(searchString, start)
          }
          buf.append(text, start, text.length)
          buf.toString
        }
      }
  }
}

object Parser extends RegexParsers {

  private val NumberRegex = """-?\d+""".r
  private val FieldRegex = """[^\*\.\[\]\(\)=!<>\s]+""".r
  private val SingleQuotedFieldRegex = """(\\.|[^'])+""".r
  private val DoubleQuotedFieldRegex = """(\\.|[^"])+""".r
  private val SingleQuotedValueRegex = """(\\.|[^'])*""".r
  private val DoubleQuotedValueRegex = """(\\.|[^"])*""".r
  private val NumberValueRegex = """-?\d+(\.\d*)?""".r

  /// general purpose parsers ///////////////////////////////////////////////

  private def number: Parser[Int] = NumberRegex ^^ (_.toInt)

  private def field: Parser[String] = FieldRegex

  import FastStringOps._
  private def singleQuotedField = "'" ~> SingleQuotedFieldRegex <~ "'" ^^ (_.fastReplaceAll("\\'", "'"))
  private def doubleQuotedField = "\"" ~> DoubleQuotedFieldRegex <~ "\"" ^^ (_.fastReplaceAll("\\\"", "\""))
  private def singleQuotedValue = "'" ~> SingleQuotedValueRegex <~ "'" ^^ (_.fastReplaceAll("\\'", "'"))
  private def doubleQuotedValue = "\"" ~> DoubleQuotedValueRegex <~ "\"" ^^ (_.fastReplaceAll("\\\"", "\""))
  private def quotedField: Parser[String] = singleQuotedField | doubleQuotedField
  private def quotedValue: Parser[Json] = singleQuotedValue.map(Json.fromString) | doubleQuotedValue.map(Json.fromString)

  /// array parsers /////////////////////////////////////////////////////////

  private def arraySliceStep: Parser[Option[Int]] = ":" ~> number.?

  private def arraySlice: Parser[ArraySlice] =
    (":" ~> number.?) ~ arraySliceStep.? ^^ {
      case end ~ step => ArraySlice(None, end, step.flatten.getOrElse(1))
    }

  private def arrayRandomAccess: Parser[Option[ArrayRandomAccess]] =
    rep1("," ~> number).? ^^ (_.map(ArrayRandomAccess))

  private def arraySlicePartial: Parser[ArrayAccessor] =
    number ~ arraySlice ^^ {
      case i ~ as => as.copy(start = Some(i))
    }

  private def arrayRandomAccessPartial: Parser[ArrayAccessor] =
    number ~ arrayRandomAccess ^^ {
      case i ~ Some(ArrayRandomAccess(indices)) => ArrayRandomAccess(i :: indices)
      case i ~ _                                => ArrayRandomAccess(i :: Nil)
    }

  private def arrayPartial: Parser[ArrayAccessor] =
    arraySlicePartial | arrayRandomAccessPartial

  private def arrayAll: Parser[ArraySlice] =
    "*" ^^^ ArraySlice.All

  private[jsonpath] def arrayAccessors: Parser[ArrayAccessor] =
    "[" ~> (arrayAll | arrayPartial | arraySlice) <~ "]"

  /// filters parsers ///////////////////////////////////////////////////////

  private def numberValue: Parser[JPNumber] = NumberValueRegex ^^ { s =>
    if (s.indexOf('.') != -1) JPDouble(Json.fromDouble(s.toDouble).get) else JPLong(Json.fromLong(s.toLong))
  }

  private def booleanValue: Parser[FilterDirectValue] =
    "true" ^^^ JPTrue |
      "false" ^^^ JPFalse

  private def nullValue: Parser[FilterValue] =
    "null" ^^^ JPNull

  private def stringValue: Parser[JPString] = quotedValue ^^ { JPString }
  private def value: Parser[FilterValue] = booleanValue | numberValue | nullValue | stringValue

  private def comparisonOperator: Parser[ComparisonOperator] =
    "==" ^^^ EqOperator |
      "!=" ^^^ NotEqOperator |
      "<=" ^^^ LessOrEqOperator |
      "<" ^^^ LessOperator |
      ">=" ^^^ GreaterOrEqOperator |
      ">" ^^^ GreaterOperator

  private def current: Parser[PathToken] = "@" ^^^ CurrentNode

  private def subQuery: Parser[SubQuery] =
    (current | root) ~ pathSequence ^^ { case c ~ ps => SubQuery(c :: ps) }

  private def expression1: Parser[FilterToken] =
    subQuery ~ (comparisonOperator ~ (subQuery | value)).? ^^ {
      case subq1 ~ None         => HasFilter(subq1)
      case lhs ~ Some(op ~ rhs) => ComparisonFilter(op, lhs, rhs)
    }

  private def expression2: Parser[FilterToken] =
    value ~ comparisonOperator ~ subQuery ^^ {
      case lhs ~ op ~ rhs => ComparisonFilter(op, lhs, rhs)
    }

  private def expression: Parser[FilterToken] = expression1 | expression2

  private def booleanOperator: Parser[BinaryBooleanOperator] = "&&" ^^^ AndOperator | "||" ^^^ OrOperator

  private def booleanExpression: Parser[FilterToken] =
    expression ~ (booleanOperator ~ booleanExpression).? ^^ {
      case lhs ~ None => lhs
      // Balance the AST tree so that all "Or" operations are always on top of any "And" operation.
      // Indeed, the "And" operations have a higher priority and must be executed first.
      case lhs1 ~ Some(AndOperator ~ BooleanFilter(OrOperator, lhs2, rhs2)) =>
        BooleanFilter(OrOperator, BooleanFilter(AndOperator, lhs1, lhs2), rhs2)
      case lhs ~ Some(op ~ rhs) => BooleanFilter(op, lhs, rhs)
    }

  private def recursiveSubscriptFilter: Parser[RecursiveFilterToken] =
    (("..*" | "..") ~> subscriptFilter) ^^ RecursiveFilterToken

  private[jsonpath] def subscriptFilter: Parser[FilterToken] =
    "[?(" ~> booleanExpression <~ ")]"

  /// child accessors parsers ///////////////////////////////////////////////

  private[jsonpath] def subscriptField: Parser[FieldAccessor] =
    "[" ~> rep1sep(quotedField, ",") <~ "]" ^^ {
      case f1 :: Nil => Field(f1)
      case fields    => MultiField(fields)
    }

  private[jsonpath] def dotField: Parser[FieldAccessor] =
    "." ~> field ^^ Field

  // TODO recursive with `subscriptField`
  private def recursiveField: Parser[FieldAccessor] =
    ".." ~> field ^^ RecursiveField

  private def anyChild: Parser[FieldAccessor] = (".*" | "['*']" | """["*"]""") ^^^ AnyField

  private def recursiveAny: Parser[FieldAccessor] = "..*" ^^^ RecursiveAnyField

  private[jsonpath] def fieldAccessors = (
    dotField
      | recursiveSubscriptFilter
      | recursiveAny
      | recursiveField
      | anyChild
      | subscriptField
  )

  /// Main parsers //////////////////////////////////////////////////////////

  private def childAccess = fieldAccessors | arrayAccessors

  private[jsonpath] def pathSequence: Parser[List[PathToken]] = rep(childAccess | subscriptFilter)

  private[jsonpath] def root: Parser[PathToken] = "$" ^^^ RootNode

  private def query: Parser[List[PathToken]] =
    phrase(root ~ pathSequence) ^^ { case r ~ ps => r :: ps }
}

class Parser {
  private val query = Parser.query
  def compile(jsonpath: String): Parser.ParseResult[List[PathToken]] = Parser.parse(query, jsonpath)
}
