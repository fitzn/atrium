//
// DCTSpec.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.core

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DCTSpec extends FlatSpec {

  val MATRIX_0 = Vector(
    Vector(54, 35),
    Vector(128, 185)
  )

  val RESULT_0 = Vector(
    Vector(201, -19),
    Vector(-112, 38)
  )

  val MATRIX_1 = Vector(
    Vector(-76, -74, -67, -62, -58, -67, -64, -55),
    Vector(-65, -69, -73, -38, -19, -43, -59, -56),
    Vector(-66, -69, -60, -15, 16, -24, -62, -55),
    Vector(-65, -70, -57, -6, 26, -22, -58, -59),
    Vector(-61, -67, -60, -24, -2, -40, -60, -58),
    Vector(-49, -63, -68, -58, -51, -60, -70, -53),
    Vector(-43, -57, -64, -69, -73, -67, -63, -45),
    Vector(-41, -49, -59, -60, -63, -52, -50, -34)
  )

  behavior of "Atrium's DCT"

  it should "apply the DCT on a small matrix" in {
    val matrix0 = MATRIX_0.map(_.map(_.toDouble))
    val result0 = DCT.applyDCT(matrix0).
      map(_.map(_.round))   // Rounds to a Long.

    assert(result0 == RESULT_0)
  }

  it should "apply the IDCT on a small matrix" in {
    val dctMatrix0 = RESULT_0.map(_.map(_.toDouble))
    val result0 = DCT.applyIDCT(dctMatrix0).
      map(_.map(_.round))   // Rounds to a Long.

    assert(result0 == MATRIX_0)
  }

  it should "apply the DCT and then the IDCT to reverse it on a large matrix" in {
    val inputMatrix = MATRIX_1.map(_.map(_.toDouble))
    val dctMatrix = DCT.applyDCT(inputMatrix)
    val resultMatrix = DCT.applyIDCT(dctMatrix).
      map(_.map(_.round))   // Rounds to a Long.

    assert(resultMatrix == MATRIX_1)
  }
}
