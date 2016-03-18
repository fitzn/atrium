//
// ImageConversionsSpec.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.utils

import java.awt.Color
import com.root81.atrium.core.RGBRegion
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ImageConversionsSpec extends FlatSpec {

  val TEST_RED = 234
  val TEST_GREEN = 101
  val TEST_BLUE = 8
  val TEST_COLOR = new Color(TEST_RED, TEST_GREEN, TEST_BLUE, ImageConversions.COLOR_ALPHA_CONSTANT)
  val TEST_PIXEL = TEST_COLOR.getRGB

  val WIDTH = 2
  val HEIGHT = 3

  behavior of "ImageConversions"

  it should "produce red, green and blue byte values from a pixel" in {
    assert(ImageConversions.getRed(TEST_PIXEL) == TEST_RED)
    assert(ImageConversions.getGreen(TEST_PIXEL) == TEST_GREEN)
    assert(ImageConversions.getBlue(TEST_PIXEL) == TEST_BLUE)
  }

  it should "produce a pixel from red, green and blue byte values" in {
    val pixel = ImageConversions.getPixel(TEST_RED, TEST_GREEN, TEST_BLUE)
    assert(pixel == TEST_PIXEL)
  }

  it should "convert from RGBRegion to YCCRegion and back again" in {
    val pixels = for (i <- (0 until (WIDTH * HEIGHT)).toList) yield {
      ImageConversions.getPixel(TEST_RED + i, TEST_GREEN + i, TEST_BLUE + i)
    }

    val rgbRegionInput = RGBRegion(WIDTH, HEIGHT, pixels)
    val yccRegion = ImageConversions.toYCCRegion(rgbRegionInput)
    val rgbRegionOutput = ImageConversions.toRGBRegion(yccRegion)

    assert(rgbRegionInput == rgbRegionOutput)
  }
}
