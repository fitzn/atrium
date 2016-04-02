//
// ImageConversions.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.utils

import com.root81.atrium.core.{YCCPixel, YCCRegion, RGBRegion, RegionedImage}
import java.awt.image.BufferedImage

object ImageConversions {

  val COLOR_ALPHA_CONSTANT = 255  // Assume colors are completely opaque in the getPixel method.
  val YCC_BIT_DEPTH = 8
  val YCC_BIT_DEPTH_CONSTANT = Math.pow(2, YCC_BIT_DEPTH - 1)

  def getRed(pixel: Int): Int = (pixel >> 16) & 0xFF
  def getGreen(pixel: Int): Int = (pixel >> 8) & 0xFF
  def getBlue(pixel: Int): Int = (pixel >> 0) & 0xFF
  def getPixel(r: Int, g: Int, b: Int): Int = (COLOR_ALPHA_CONSTANT << 24) | ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | (b & 0xFF)

  def toRegionedImage(image: BufferedImage, regionWidth: Int, regionHeight: Int): RegionedImage = {
    val (width, height) = (image.getWidth, image.getHeight)

    if (width % regionWidth != 0 || height % regionHeight != 0) {
      throw new UnsupportedOperationException("Region dimensions must be a multiple of the image's dimensions.")
    }

    if (image.getType != BufferedImage.TYPE_3BYTE_BGR) {
      throw new UnsupportedOperationException(s"Image Type (${image.getType}) is not supported. RGB type (${BufferedImage.TYPE_3BYTE_BGR}) only.")
    }

    val rowRegions = (0 until height).grouped(regionHeight).map(_.toList).toList
    val columnRegions = (0 until width).grouped(regionWidth).map(_.toList).toList

    val regions = for {
      rowsInThisRegion <- rowRegions
      columnsInThisRegion <- columnRegions
    } yield {
      // Pair up the row and column indices within this region to produce each pixel coordinate.
      val pixels = for {
        row <- rowsInThisRegion
        column <- columnsInThisRegion
      } yield image.getRGB(column, row)

      RGBRegion(regionWidth, regionHeight, pixels)
    }

    RegionedImage(width, height, regions)
  }

  def toBufferedImage(regionedImage: RegionedImage): BufferedImage = {
    val regions = regionedImage.regions
    val width = regionedImage.width
    val widthSum = regions.map(_.width).sum
    val height = regionedImage.height
    val heightSum = regions.map(_.height).sum

    require(widthSum % width == 0, "Malformed RegionedImage; sum of regions' width is not a multiple of image width")
    require(heightSum % height == 0, "Malformed RegionedImage; sum of regions' height is not a multiple of image height")

    var (x, y) = (0, 0)
    val image = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)

    regions.foreach(region => {

      // Loop through every pixel in this region and write it into the image at the corresponding location based off of the top-left, (x, y).
      (0 until region.height).toList.foreach(row => {
        (0 until region.width).toList.foreach(col => {
          val pixelIndex = (row * region.width) + col
          val pixel = region.pixels(pixelIndex)
          image.setRGB(x + col, y + row, pixel)
        })
      })

      // We've finished this region, so move the (x, y) to the top left of the next region.
      x = (x + region.width) % width
      y = if (x == 0) y + region.height else y
    })

    image
  }

  def toYCCRegion(region: RGBRegion): YCCRegion = {

    val yccPixels = region.pixels.map(rgbPixel => {
      val (r, g, b) = (getRed(rgbPixel), getGreen(rgbPixel), getBlue(rgbPixel))
      val y = (0.299 * r) + (0.587 * g) + (0.114 * b)
      val cb = (-0.1687 * r) - (0.3313 * g) + (0.5 * b) + YCC_BIT_DEPTH_CONSTANT
      val cr = (0.5 * r) - (0.4187 * g) - (0.0813 * b) + YCC_BIT_DEPTH_CONSTANT
      YCCPixel(y, cb, cr)
    })

    YCCRegion(region.width, region.height, yccPixels)
  }

  def toRGBRegion(region: YCCRegion): RGBRegion = {

    val rgbPixels = region.pixels.map(yccPixel => {
      val r = (yccPixel.y + 1.40200 * (yccPixel.cr - YCC_BIT_DEPTH_CONSTANT)).round.toInt
      val g = (yccPixel.y - 0.34414 * (yccPixel.cb - YCC_BIT_DEPTH_CONSTANT) - 0.71414 * (yccPixel.cr - YCC_BIT_DEPTH_CONSTANT)).round.toInt
      val b = (yccPixel.y + 1.77200 * (yccPixel.cb - YCC_BIT_DEPTH_CONSTANT)).round.toInt
      getPixel(r, g, b)
    })

    RGBRegion(region.width, region.height, rgbPixels)
  }
}
