package com.datascience.education.tutorial.lecture4


import scala.Option
import scala.Some
import scala.None

import cats.Applicative
import cats.Traverse
import cats.std.list._


object SafeDivision {

  // Task (2a)
  //safeDiv should catch division by zero and results that are NaN, positive infinity, or negative infinity.
  //Use methods defined on Java's Double class to determine these last three conditions.


  def safeDiv(x: Int, y: Int): Option[Double] =
    try {
      val z = x/y.toDouble
      if (z.isNaN || z.isInfinite) None: Option[Double]
      else Some(z)
    }
    catch {
      case e: ArithmeticException => None: Option[Double]
    }

  def divTuple(tup: (Int, Int)): Option[Double] =
    safeDiv(tup._1, tup._2)


  import TraverseOption._
  
  // Task (2b)
  def traverseFractions(ll: List[(Int, Int)]): Option[List[Double]] = 
    traverse(ll)(x => divTuple(x))


  // Task (2c)
  def safeSqrt(tup: (Int, Int)): Option[Double] = {
    val sqrtx = divTuple(tup).map(x => math.sqrt(x))
    if (sqrtx == None) None: Option[Double]
    else if (sqrtx.map(x => x.isNaN).contains(true)) None: Option[Double]
    else sqrtx
  }

  def traverseSqrtFractions(ll: List[(Int, Int)]): Option[List[Double]] = {
    traverse(ll)(x => safeSqrt(x))

    //This line will perform the traverseSqrtFractions, but returns NaN while we want to return None for a NaN:
    //traverseFractions(ll).map(aa => aa.map(math.sqrt(_)))

    //This removes NaN instead returning a None:
    //val mixed = traverseFractions(ll).map(aa => aa.map(math.sqrt(_)))
    //mixed.map(ld => ld.filter(x => !x.isNaN))
  }



}


object SafeDivisionExamples extends App {

  import SafeDivision._

  println("Divide 7 by 2")
  println(divTuple((7,2)))

  println("Divide 7 by 0")
  println(divTuple((7,0)))

}

object SafeDivisionTraversalExamples extends App {
  import SafeDivision._

  val a = (6 to 11).toList
  val b = (-3 to 2).toList
  val fracsFailing: List[Tuple2[Int, Int]] = a.zip(b)

  val optionList1: Option[List[Double]] =
    traverseFractions(fracsFailing)

  println("Option[List[Double]] in one step, using `traverse`: ")
  println("should fail")
  println(optionList1)


  println("-----------------")
  val c = (6 to 11).toList
  val d = (2 to 7).toList
  val fracsSuccessful: List[Tuple2[Int, Int]] = c.zip(d)

  println("These fractions do not include an undefined number")
  val optionList2: Option[List[Double]] =
    traverseFractions(fracsSuccessful)

  println("Option[List[Double]] in one step, using `traverse`: ")
  println(optionList2)

  println("-----------------")

  val optionSqrt1: Option[List[Double]] = traverseSqrtFractions(fracsFailing)
  println("Square root of fractions using `traverse`: ")
  println("Should fail")
  println(optionSqrt1)

  val optionSqrt2: Option[List[Double]] = traverseSqrtFractions(fracsSuccessful)
  println("Square root of fractions using `traverse`: ")
  println("These fractions do not include an undefined number and should succeed")
  println(optionSqrt2)

  val p = List(-6, 1, 2, -3, 5)
  val q = (1 to 6).toList
  val fracsNaN: List[Tuple2[Int, Int]] = p.zip(q)

  val optionSqrt3: Option[List[Double]] = traverseSqrtFractions(fracsNaN)
  println("Square root of fractions with negative numerators: ")
  println(optionSqrt3)

}


