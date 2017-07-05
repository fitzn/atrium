//
// ImageConversions.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.utils

import com.root81.atrium.core.{YCCPixel, YCCRegion, RGBRegion, RegionedImageRGB}
import java.awt.image.BufferedImage

object ImageConversions {

  val COLOR_ALPHA_CONSTANT = 255  // Assume colors are completely opaque in the getPixel method.
  val COLOR_RANGE_NORMALIZE_CONSTANT = 128D

  def getRed(pixel: Int): Int = (pixel >> 16) & 0xFF
  def getGreen(pixel: Int): Int = (pixel >> 8) & 0xFF
  def getBlue(pixel: Int): Int = (pixel >> 0) & 0xFF

  def extractPixel(pixel: Int): (Int, Int, Int) = (getRed(pixel), getGreen(pixel), getBlue(pixel))
  def makePixel(r: Int, g: Int, b: Int): Int = (COLOR_ALPHA_CONSTANT << 24) | ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | (b & 0xFF)
  def getPixelString(pixel: Int): String = "(" + getRed(pixel) + "," + getGreen(pixel) + "," + getBlue(pixel) + ")"

  def toYCCPixel(rgbPixel: Int): YCCPixel = {
    val (red, green, blue) = (getRed(rgbPixel), getGreen(rgbPixel), getBlue(rgbPixel))

    val y = (0.298839 * red) + (0.586811 * green) + (0.114350 * blue)
    val cb = (-0.1687367 * red) - (0.331264 * green) + (0.5 * blue) + COLOR_RANGE_NORMALIZE_CONSTANT
    val cr = (0.5 * red) - (0.418688 * green) - (0.081312 * blue) + COLOR_RANGE_NORMALIZE_CONSTANT

    YCCPixel(y, cb, cr)
  }

  def toRGBPixel(yccPixel: YCCPixel): Int = {
    val YCCPixel(y, originalCb, originalCr) = yccPixel
    val cb = originalCb - COLOR_RANGE_NORMALIZE_CONSTANT
    val cr = originalCr - COLOR_RANGE_NORMALIZE_CONSTANT

    val red = (0.99999999999914679361 * y) - (1.2188941887145875e-06 * cb) + (1.4019995886561440468 * cr)
    val green = (0.99999975910502514331 * y) - (0.34413567816504303521 * cb) - (0.71413649331646789076 * cr)
    val blue = (1.00000124040004623180 * y) + (1.77200006607230409200 * cb) + (2.1453384174593273e-06 * cr)

    makePixel(red.round.toInt, green.round.toInt, blue.round.toInt)
  }

  def safeConvertYCCToRGB(previousYCCPixel: YCCPixel, newYValue: Double): Int = {

    val newYCCPixel = previousYCCPixel.copy(y = newYValue)

    val (previousRed, previousGreen, previousBlue) = extractPixel(ImageConversions.toRGBPixel(previousYCCPixel))
    val (newRed, newGreen, newBlue) = extractPixel(ImageConversions.toRGBPixel(newYCCPixel))

    val actualPixel = if (previousYCCPixel.y < newYValue) {
      // Increased luminance - every color should have increased, but watch for the wrap-around at 255.

      val actualRed = clampForColorIncrease(previousRed, newRed)
      val actualGreen = clampForColorIncrease(previousGreen, newGreen)
      val actualBlue = clampForColorIncrease(previousBlue, newBlue)

      makePixel(actualRed, actualGreen, actualBlue)

    } else {
      // Decreased luminance - every color should be reduced, but watch for wrap-around at 0.

      val actualRed = clampForColorDecrease(previousRed, newRed)
      val actualGreen = clampForColorDecrease(previousGreen, newGreen)
      val actualBlue = clampForColorDecrease(previousBlue, newBlue)

      makePixel(actualRed, actualGreen, actualBlue)
    }

    actualPixel
  }

  def toRegionedImage(image: BufferedImage, regionWidth: Int, regionHeight: Int): RegionedImageRGB = {
    require(image.getType == BufferedImage.TYPE_3BYTE_BGR, s"Image Type (${image.getType}) must be RGB type: (${BufferedImage.TYPE_3BYTE_BGR}).")

    val (width, height) = (image.getWidth, image.getHeight)
    val rowRegions = (0 until height).grouped(regionHeight).map(_.toList).toList
    val columnRegions = (0 until width).grouped(regionWidth).map(_.toList).toList

    val regions = for {
      rowsInThisRegion <- rowRegions
      columnsInThisRegion <- columnRegions
    } yield {
      val topLeftX = columnsInThisRegion.head
      val topLeftY = rowsInThisRegion.head

      // Pair up the row and column indices within this region to produce each pixel coordinate.
      val pixels = for {
        row <- rowsInThisRegion
        column <- columnsInThisRegion
      } yield image.getRGB(column, row)

      RGBRegion(topLeftX, topLeftY, columnsInThisRegion.size, rowsInThisRegion.size, pixels)
    }

    RegionedImageRGB(width, height, regions)
  }

  def toBufferedImage(regionedImage: RegionedImageRGB): BufferedImage = {
    require(doRegionsCoverImage(regionedImage), "malformed image: regions have gaps, overlap or don't match width and height")

    val image = new BufferedImage(regionedImage.width, regionedImage.height, BufferedImage.TYPE_3BYTE_BGR)

    regionedImage.regions.foreach(region => {
      // Loop through every pixel in this region and write it into the image at the corresponding location based off of the top-left, (x, y).
      val (x, y) = (region.topLeftX, region.topLeftY)

      (0 until region.height).toList.foreach(row => {
        (0 until region.width).toList.foreach(col => {
          val pixelIndex = (row * region.width) + col
          val pixel = region.pixels(pixelIndex)
          image.setRGB(x + col, y + row, pixel)
        })
      })
    })

    image
  }

  def toYCCRegion(region: RGBRegion): YCCRegion = {
    val yccPixels = region.pixels.map(toYCCPixel)
    YCCRegion(region.topLeftX, region.topLeftY, region.width, region.height, yccPixels)
  }

  def toRGBRegion(region: YCCRegion): RGBRegion = {
    val rgbPixels = region.pixels.map(toRGBPixel)
    RGBRegion(region.topLeftX, region.topLeftY, region.width, region.height, rgbPixels)
  }

  //
  // Internal helpers
  //

  protected def doRegionsCoverImage(regionedImage: RegionedImageRGB): Boolean = {
    val regions = regionedImage.regions.sortBy(r => (r.topLeftY, r.topLeftX))

    val isContinuousWithoutOverlap = regions.indices.tail.toList.forall(i => {
      val previous = regions(i - 1)
      val current = regions(i)
      val previousTopRightX = previous.topLeftX + previous.width

      (previousTopRightX == current.topLeftX && previous.topLeftY == current.topLeftY) ||
        (previousTopRightX == regionedImage.width && current.topLeftX == 0 && previous.topLeftY + previous.height == current.topLeftY)
    })

    lazy val maxWidthIsWidth = regions.map(r => r.topLeftX + r.width).sorted.lastOption.contains(regionedImage.width)
    lazy val maxHeightIsHeight = regions.map(r => r.topLeftY + r.height).sorted.lastOption.contains(regionedImage.height)

    isContinuousWithoutOverlap && maxWidthIsWidth && maxHeightIsHeight
  }

  protected def clampForColorIncrease(original: Int, current: Int): Int = {
    if (original <= current) {
      current
    } else {
      255
    }
  }

  protected def clampForColorDecrease(original: Int, current: Int): Int = {
    if (original >= current) {
      current
    } else {
      0
    }
  }
}
