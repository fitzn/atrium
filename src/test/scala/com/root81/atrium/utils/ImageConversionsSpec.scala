//
// ImageConversionsSpec.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.utils

import com.root81.atrium.core.RGBRegion
import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ImageConversionsSpec extends FlatSpec {

  private val TEST_IMAGE_PATH = "src/test/resources/images/keyboard.jpg"
  private val TEST_REGION_DEFAULT_DIMENSION = 8
  private val TEST_REGION_WIDTH_ODD = 7
  private val TEST_REGION_HEIGHT_ODD = 3
  private val inputImage = ImageIO.read(new File(TEST_IMAGE_PATH))
  private val inputImageRGB = getRGB(inputImage)

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

    val rgbRegionInput = RGBRegion(0, 0, WIDTH, HEIGHT, pixels)
    val yccRegion = ImageConversions.toYCCRegion(rgbRegionInput)
    val rgbRegionOutput = ImageConversions.toRGBRegion(yccRegion)

    assert(rgbRegionInput == rgbRegionOutput)
  }

  it should "convert an image to a RegionedImage and back again without modifying the data for in-order regions" in {
    val regionedImage = ImageConversions.toRegionedImage(inputImage, TEST_REGION_DEFAULT_DIMENSION, TEST_REGION_DEFAULT_DIMENSION)

    val outputImage = ImageConversions.toBufferedImage(regionedImage)
    val outputRGB = getRGB(outputImage)

    assert(inputImageRGB == outputRGB)
  }

  it should "convert an image to a RegionedImage and back again without modifying the data for out-of-order regions" in {
    val regionedImage = ImageConversions.toRegionedImage(inputImage, TEST_REGION_DEFAULT_DIMENSION, TEST_REGION_DEFAULT_DIMENSION)

    val shuffledRegions = regionedImage.regions.grouped(2).toList.flatMap(_.reverse)
    val outputImage = ImageConversions.toBufferedImage(regionedImage.copy(regions = shuffledRegions))
    val outputRGB = getRGB(outputImage)

    assert(inputImageRGB == outputRGB)
  }

  it should "handle an image with dimensions that are not a multiple of the regions" in {
    val regionedImage = ImageConversions.toRegionedImage(inputImage, TEST_REGION_WIDTH_ODD, TEST_REGION_HEIGHT_ODD)

    val outputImage = ImageConversions.toBufferedImage(regionedImage)
    val outputRGB = getRGB(outputImage)

    assert(inputImageRGB == outputRGB)
  }

  //
  // Internal helpers
  //

  private def getRGB(image: BufferedImage): List[List[Int]] = {
    val (inWidth, inHeight) = (image.getWidth, image.getHeight)

    (0 until inHeight).toList.map(row => {
      (0 until inWidth).toList.map(col => {
        image.getRGB(row, col)
      })
    })
  }
}
