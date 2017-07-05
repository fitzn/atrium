//
// ImageConversionsSpec.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.utils

import com.root81.atrium.core.{RGBRegion, YCCPixel}
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

  val COLOR_SPACE_ERROR_MARGIN = 0.5  // This is 0.5 of the full [0, 256) range for RGB and [8, 248) for YCC
  val COLOR_SPACE_EXTENDED_ERROR_MARGIN = COLOR_SPACE_ERROR_MARGIN * 2    // Values are close enough for error-correction to work.

  behavior of "ImageConversions"

  it should "produce red, green and blue byte values from a pixel" in {
    assert(ImageConversions.getRed(TEST_PIXEL) == TEST_RED)
    assert(ImageConversions.getGreen(TEST_PIXEL) == TEST_GREEN)
    assert(ImageConversions.getBlue(TEST_PIXEL) == TEST_BLUE)
  }

  it should "produce a pixel from red, green and blue byte values" in {
    val pixel = ImageConversions.makePixel(TEST_RED, TEST_GREEN, TEST_BLUE)
    assert(pixel == TEST_PIXEL)
  }

  it should "convert from RGB pixel to YCC pixel" in {
    val rgbPixel = ImageConversions.makePixel(139, 246, 118)
    val yccPixel = ImageConversions.toYCCPixel(rgbPixel)

    assert(math.abs(yccPixel.y - 199.387427) <= COLOR_SPACE_ERROR_MARGIN)
    assert(math.abs(yccPixel.cb - 82.0546547) <= COLOR_SPACE_ERROR_MARGIN)
    assert(math.abs(yccPixel.cr - 84.907936) <= COLOR_SPACE_ERROR_MARGIN)
  }

  it should "convert from YCC pixel to RGB pixel" in {
    val yccPixel = YCCPixel(199.387427, 82.0546547, 84.907936)
    val rgbPixel = ImageConversions.toRGBPixel(yccPixel)

    val expectedPixel = ImageConversions.makePixel(139, 246, 118)
    assert(rgbPixel == expectedPixel)
  }

  it should "convert from YCCPixel to RGBPixel and back again without correction" in {
    val originalYccPixel = YCCPixel(227.6,123.3,133.5)
    val rgbPixel = ImageConversions.toRGBPixel(originalYccPixel)
    val newYccPixel = ImageConversions.toYCCPixel(rgbPixel)

    assert(math.abs(originalYccPixel.y - newYccPixel.y) <= COLOR_SPACE_ERROR_MARGIN)
    assert(math.abs(originalYccPixel.cb - newYccPixel.cb) <= COLOR_SPACE_ERROR_MARGIN)
    assert(math.abs(originalYccPixel.cr - newYccPixel.cr) <= COLOR_SPACE_ERROR_MARGIN)
  }

  it should "convert from YCCPixel to RGBPixel and back again safely with correction" in {
    val originalYccPixel = YCCPixel(216.422605,149.7803575,98.46719999999999)
    val newYValue = 217.30825499999997

    val rgbPixel = ImageConversions.safeConvertYCCToRGB(originalYccPixel, newYValue)
    val newYccPixel = ImageConversions.toYCCPixel(rgbPixel)

    assert(math.abs(originalYccPixel.y - newYccPixel.y) <= COLOR_SPACE_EXTENDED_ERROR_MARGIN)
    assert(math.abs(originalYccPixel.cb - newYccPixel.cb) <= COLOR_SPACE_EXTENDED_ERROR_MARGIN)
    assert(math.abs(originalYccPixel.cr - newYccPixel.cr) <= COLOR_SPACE_EXTENDED_ERROR_MARGIN)
  }

  it should "convert from RGBRegion to YCCRegion and back again" in {
    val pixels = for (i <- (0 until (WIDTH * HEIGHT)).toList) yield {
      ImageConversions.makePixel(TEST_RED + i, TEST_GREEN + i, TEST_BLUE + i)
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
