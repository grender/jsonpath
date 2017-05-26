/**
 * Copyright 2011-2017 GatlingCorp (http://gatling.io)
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

import io.circe.Json

sealed trait ComparisonOperator {
  def apply(lhs: Json, rhs: Json): Boolean
}

// Comparison operators
sealed trait ComparisonWithOrderingOperator extends ComparisonOperator {

  def compare[T: Ordering](lhs: T, rhs: T): Boolean

  def apply(lhs: Json, rhs: Json): Boolean = lhs match {
    case s1 if s1.isString => rhs match {
      case s2 if s2.isString => compare(s1.asString.get, s2.asString.get)
      case _ => false
    }
    case b1 if b1.isBoolean => rhs match {
      case b2 if b2.isBoolean => compare(b1.asBoolean.get, b2.asBoolean.get)
      case _ => false
    }
    case i1 if i1.isNumber => rhs match {
      case i2 if i2.isNumber =>
        (i1.asNumber.get.toBigDecimal, i2.asNumber.get.toBigDecimal) match {
          case (Some(n1), Some(n2)) => compare(n1, n2)
          case _ => false
        }
      case _ => false
    }
    case n1 if n1.isNull => rhs match {
      case n2 if n2.isNull => false
      case _ => false
    }
    case _ => false
  }
}

case object EqOperator extends ComparisonOperator {
  override def apply(lhs: Json, rhs: Json): Boolean = lhs == rhs
}

case object NotEqOperator extends ComparisonOperator {
  override def apply(lhs: Json, rhs: Json): Boolean = lhs != rhs
}

case object LessOperator extends ComparisonWithOrderingOperator {
  override def compare[T: Ordering](lhs: T, rhs: T): Boolean = Ordering[T].lt(lhs, rhs)
}

case object GreaterOperator extends ComparisonWithOrderingOperator {
  override def compare[T: Ordering](lhs: T, rhs: T): Boolean = Ordering[T].gt(lhs, rhs)
}

case object LessOrEqOperator extends ComparisonWithOrderingOperator {
  override def compare[T: Ordering](lhs: T, rhs: T): Boolean = Ordering[T].lteq(lhs, rhs)
}

case object GreaterOrEqOperator extends ComparisonWithOrderingOperator {
  override def compare[T: Ordering](lhs: T, rhs: T): Boolean = Ordering[T].gteq(lhs, rhs)
}

// Binary boolean operators
sealed trait BinaryBooleanOperator {
  def apply(lhs: Boolean, rhs: Boolean): Boolean
}

case object AndOperator extends BinaryBooleanOperator {
  def apply(lhs: Boolean, rhs: Boolean): Boolean = lhs && rhs
}

case object OrOperator extends BinaryBooleanOperator {
  def apply(lhs: Boolean, rhs: Boolean): Boolean = lhs || rhs
}
