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

import io.circe._
import io.gatling.jsonpath.AST._

import scala.math.abs

case class JPError(reason: String)

object JsonPath {
  private val JsonPathParser = ThreadLocal.withInitial[Parser](() => new Parser)

  def compile(query: String): Either[JPError, JsonPath] =
    JsonPathParser.get.compile(query) match {
      case Parser.Success(q, _) => Right(new JsonPath(q))
      case ns: Parser.NoSuccess => Left(JPError(ns.msg))
    }

  def query(query: String, jsonObject: Json): Either[JPError, Iterable[Json]] =
    compile(query).right.map(_.query(jsonObject))
}

class JsonPath(path: List[PathToken]) {
  def query(jsonObject: Json) = new JsonPathWalker(jsonObject, path).walk()
}

class JsonPathWalker(rootNode: Json, fullPath: List[PathToken]) {

  def walk(): Iterable[Json] = walk(rootNode, fullPath)

  private[this] def walk(node: Json, path: List[PathToken]): Iterable[Json] =
    path match {
      case head :: tail => walk1(node, head).flatMap(walk(_, tail))
      case Nil          => Iterable(node)
    }

  private[this] def walk1(node: Json, query: PathToken): Iterable[Json] = {
    query match {
      case RootNode => Iterable(rootNode)

      case CurrentNode => Iterable(node)

      case Field(name) =>
        node match {
          case json if json.isObject =>
            json.asObject.flatMap(_.apply(name))
          case _ => Iterable.empty
        }

      case RecursiveField(name) => recFieldFilter(node, name)

      case MultiField(fieldNames) =>
        node match {
          case json if json.isObject =>
            val jsonObjOpt = json.asObject
            fieldNames.flatMap(fieldName => jsonObjOpt.flatMap(_.apply(fieldName)))
          case _ => Iterable.empty
        }

      case AnyField =>
        node match {
          case json if json.isObject => json.asObject.map(_.values).getOrElse(Vector.empty)
          case _                     => Iterable.empty
        }

      case ArraySlice(None, None, 1) =>
        node match {
          case json if json.isArray => json.asArray.getOrElse(Vector.empty)
          case _                    => Iterable.empty
        }

      case ArraySlice(start, stop, step) =>
        node match {
          case json if json.isArray => sliceArray(json.asArray.getOrElse(Vector.empty), start, stop, step)
          case _                    => Iterable.empty
        }

      case ArrayRandomAccess(indices) =>
        node match {
          case json if json.isArray =>
            val jsonArray = json.asArray.getOrElse(Vector.empty)
            indices.collect {
              case i if i >= 0 && i < jsonArray.size  => jsonArray(i)
              case i if i < 0 && i >= -jsonArray.size => jsonArray(i + jsonArray.size)
            }
          case _ => Iterable.empty
        }

      case RecursiveFilterToken(filterToken) => recFilter(node, filterToken)

      case filterToken: FilterToken => applyFilter(node, filterToken)

      case RecursiveAnyField => recFieldExplorer(node)
    }
  }

  private[this] def recFilter(node: Json, filterToken: FilterToken): Iterable[Json] = {

    def allNodes(curr: Json): Iterable[Json] = curr match {
      case json if json.isArray                                      => json.asArray.getOrElse(Vector.empty).flatMap(allNodes)
      case json if json.isObject && json.asObject.exists(_.nonEmpty) => Iterable(json) ++ json.asObject.map(_.values).getOrElse(Vector.empty).flatMap(allNodes)
      case _                                                         => Iterable.empty
    }

    allNodes(node).flatMap(applyFilter(_, filterToken))
  }

  private[this] def applyFilter(currentNode: Json, filterToken: FilterToken): Iterable[Json] = {

    def resolveSubQuery(node: Json, q: List[AST.PathToken], nextOp: Json => Boolean): Boolean = {
      val it = walk(node, q)
      it.headOption.exists(nextOp)
    }

    def applyBinaryOpWithResolvedLeft(node: Json, op: ComparisonOperator, lhsNode: Json, rhs: FilterValue): Boolean =
      rhs match {
        case direct: FilterDirectValue => op(lhsNode, direct.value)
        case SubQuery(q)               => resolveSubQuery(node, q, op(lhsNode, _))
      }

    def applyBinaryOp(node: Json, op: ComparisonOperator, lhs: FilterValue, rhs: FilterValue): Boolean =
      lhs match {
        case direct: FilterDirectValue => applyBinaryOpWithResolvedLeft(node, op, direct.value, rhs)
        case SubQuery(q)               => resolveSubQuery(node, q, applyBinaryOpWithResolvedLeft(node, op, _, rhs))
      }

    def elementsToFilter(node: Json): Iterable[Json] =
      node match {
        case json if json.isArray  => json.asArray.getOrElse(Vector.empty)
        case json if json.isObject => Iterable(json)
        case _                     => Iterable.empty
      }

    def evaluateFilter(filterToken: FilterToken): Json => Boolean =
      filterToken match {
        case HasFilter(subQuery) =>
          (node: Json) => walk(node, subQuery.path).nonEmpty

        case ComparisonFilter(op, lhs, rhs) =>
          (node: Json) => applyBinaryOp(node, op, lhs, rhs)

        case BooleanFilter(op, filter1, filter2) =>
          val f1 = evaluateFilter(filter1)
          val f2 = evaluateFilter(filter2)
          (node: Json) => op(f1(node), f2(node))
      }

    val filterFunction = evaluateFilter(filterToken)
    elementsToFilter(currentNode).filter(filterFunction)
  }

  def recFieldFilter(node: Json, name: String): Iterable[Json] = {
    def _recFieldFilter(node: Json): Iterable[Json] =
      node match {
        case json if json.isObject =>
          json.asObject
            .map(_.toMap.flatMap {
              case (key, value) if key == name => Iterable(value)
              case (key, value)                => _recFieldFilter(value)
            })
            .getOrElse(Iterable.empty)
        case json if json.isArray => json.asArray.getOrElse(Vector.empty).flatMap(_recFieldFilter)
        case _                    => Iterable.empty
      }

    _recFieldFilter(node)
  }

  def recFieldExplorer(node: Json): Iterable[Json] =
    node match {
      case json if json.isObject =>
        Iterable(json) ++ json.asObject.map(_.values).getOrElse(Vector.empty).flatMap(recFieldExplorer)
      case json if json.isArray =>
        Iterable(json) ++ json.asArray.getOrElse(Vector.empty).flatMap(recFieldExplorer)
      case json => Iterable(json)
    }

  private[this] def sliceArray(array: Iterable[Json], start: Option[Int], stop: Option[Int], step: Int): Iterable[Json] = {
    val size = array.size

    def lenRelative(x: Int) = if (x >= 0) x else size + x
    def stepRelative(x: Int) = if (step >= 0) x else -1 - x
    def relative(x: Int) = lenRelative(stepRelative(x))

    val absStart = start match {
      case Some(v) => relative(v)
      case _       => 0
    }
    val absEnd = stop match {
      case Some(v) => relative(v)
      case _       => size
    }
    val absStep = abs(step)

    val elts: Iterable[Json] = if (step < 0) array.toSeq.reverse else array
    val fromStartToEnd = elts.slice(absStart, absEnd)

    if (absStep != 1)
      fromStartToEnd.grouped(absStep).map(_.head).toIterable
    else
      fromStartToEnd
  }
}
