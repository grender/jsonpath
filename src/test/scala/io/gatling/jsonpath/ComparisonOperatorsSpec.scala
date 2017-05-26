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

import io.circe._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import Ordered.orderingToOrdered

class ComparisonOperatorsSpec
    extends FlatSpec
    with Matchers
    with GeneratorDrivenPropertyChecks {

  lazy val genJsonString:Gen[Json] = for {
    v <- arbitrary[String]
  } yield Json.fromString(v)

  lazy val genJsonBoolean:Gen[Json] = for {
    v <- arbitrary[Boolean]
  } yield Json.fromBoolean(v)

  lazy val genJsonObj:Gen[Json] = for {
    v <- arbitrary[String]
  } yield Json.fromFields(Iterable(("test", Json.fromString(v))))

  lazy val genJsonInt:Gen[Json] = for {
    v <- arbitrary[Int]
  } yield Json.fromInt(v)

  lazy val genJsonLong:Gen[Json] = for {
    v <- arbitrary[Long]
  } yield Json.fromLong(v)

  lazy val genJsonDouble:Gen[Json] = for {
    v <- arbitrary[Double]
  } yield Json.fromDouble(v).get

  lazy val genJsonFloat:Gen[Json] = for {
    v <- arbitrary[Float]
  } yield Json.fromFloat(v).get

  implicit val jsonOrdering: Ordering[Json] = (x: Json, y: Json) => {
    (x, y) match {
      case _ if x.isString && y.isString => x.asString.get.compare(y.asString.get)
      case _ if x.isBoolean && y.isBoolean => x.asBoolean.get.compare(y.asBoolean.get)
      case _ if x.isNumber && y.isNumber => x.asNumber.get.toBigDecimal.get.compare(y.asNumber.get.toBigDecimal.get)
    }
  }

  "comparison operators" should "return false if types aren't compatible" in {
    forAll(genJsonString, genJsonObj) { (s, anyVal) =>
      LessOperator(s, anyVal) shouldBe false
      GreaterOperator(s, anyVal) shouldBe false
      LessOrEqOperator(s, anyVal) shouldBe false
      GreaterOrEqOperator(s, anyVal) shouldBe false
    }

    forAll(genJsonBoolean, genJsonString) { (s, anyVal) =>
      LessOperator(s, anyVal) shouldBe false
      GreaterOperator(s, anyVal) shouldBe false
      LessOrEqOperator(s, anyVal) shouldBe false
      GreaterOrEqOperator(s, anyVal) shouldBe false
    }

    forAll(genJsonObj, genJsonString) { (s, anyVal) =>
      LessOperator(s, anyVal) shouldBe false
      GreaterOperator(s, anyVal) shouldBe false
      LessOrEqOperator(s, anyVal) shouldBe false
      GreaterOrEqOperator(s, anyVal) shouldBe false
    }
  }

  it should "properly compare Strings" in {
    forAll(genJsonString, genJsonString) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }
  }

  it should "properly compare Booleans" in {
    forAll(genJsonBoolean, genJsonBoolean) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }
  }

  it should "properly compare Int with other numeric types" in {
    forAll(genJsonInt, genJsonInt) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(genJsonInt, genJsonLong) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(genJsonInt, genJsonDouble) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(genJsonInt, genJsonFloat) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }
  }

  it should "properly compare Long with other numeric types" in {
    forAll(genJsonLong, genJsonInt) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(genJsonLong, genJsonLong) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(genJsonLong, genJsonDouble) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(genJsonLong, genJsonFloat) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }
  }

  it should "properly compare Double with other numeric types" in {
    forAll(genJsonDouble, genJsonInt) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(genJsonDouble, genJsonLong) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(genJsonDouble, genJsonDouble) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(genJsonDouble, genJsonFloat) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }
  }

  it should "properly compare Float with other numeric types" in {
    forAll(genJsonFloat, genJsonInt) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(genJsonFloat, genJsonLong) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(genJsonFloat, genJsonDouble) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(genJsonFloat, genJsonFloat) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }
  }

  "AndOperator" should "&& the lhs and rhs" in {
    forAll(Arbitrary.arbBool.arbitrary, Arbitrary.arbBool.arbitrary) { (b1, b2) =>
      AndOperator(b1, b2) shouldBe (b1 && b2)
    }
  }

  "OrOperator" should "|| the lhs and rhs" in {
    forAll(Arbitrary.arbBool.arbitrary, Arbitrary.arbBool.arbitrary) { (b1, b2) =>
      OrOperator(b1, b2) shouldBe (b1 || b2)
    }
  }
}
