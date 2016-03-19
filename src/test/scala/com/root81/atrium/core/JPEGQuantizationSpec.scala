//
// JPEGQuantizationSpec.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.core

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class JPEGQuantizationSpec extends FlatSpec {

  val EXAMPLE_CHANNEL = Vector(
    Vector(-128.0, -434.5, 30.0, 56.0, 12.0, 0.0, 25.5, 0.0),
    Vector(-144.0, 66.0, 189.0, 76.0, -26.0, -29.0, 0.0, 0.0),
    Vector(49.0, 65.0, 8.0, -72.0, -40.0, 28.5, 0.0, 0.0),
    Vector(-21.0, -8.5, -11.0, -14.5, 25.5, 43.5, 0.0, 0.0),
    Vector(9.0, 22.0, 18.5, 0.0, 0.0, 0.0, 0.0, 0.0),
    Vector(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    Vector(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    Vector(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  ).map(_.map(_ + 0.1))

  behavior of "JPEGQuantization"

  it should "quantize idempotently" in {
    val quantizedMatrix = JPEGQuantization.quantize(EXAMPLE_CHANNEL, 75)
    val originalChannel = JPEGQuantization.unquantize(quantizedMatrix)
    val secondQuantizedMatrix = JPEGQuantization.quantize(originalChannel, 75)
    val secondOriginalChannel = JPEGQuantization.unquantize(secondQuantizedMatrix)

    assert(originalChannel == secondOriginalChannel)
  }

}
