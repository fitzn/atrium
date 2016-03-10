//
// AtriumDCTSpec.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.core

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AtriumDCTSpec extends FlatSpec {

  val testMat = Vector(
    Vector(-76, -74, -67, -62, -58, -67, -64, -55),
    Vector(-65, -69, -73, -38, -19, -43, -59, -56),
    Vector(-66, -69, -60, -15, 16, -24, -62, -55),
    Vector(-65, -70, -57, -6, 26, -22, -58, -59),
    Vector(-61, -67, -60, -24, -2, -40, -60, -58),
    Vector(-49, -63, -68, -58, -51, -60, -70, -53),
    Vector(-43, -57, -64, -69, -73, -67, -63, -45),
    Vector(-41, -49, -59, -60, -63, -52, -50, -34)
  ).map(_.map(_.toDouble))

}
