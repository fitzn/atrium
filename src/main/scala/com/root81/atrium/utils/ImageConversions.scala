//
// ImageConversions.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.utils

import java.awt.image.BufferedImage

object ImageConversions {

  val YCC_BIT_DEPTH = 8
  val YCC_BIT_DEPTH_CONSTANT = Math.pow(2, YCC_BIT_DEPTH - 1)

  def getRed(pixel: Int): Int = (pixel >> 16) & 0xFF
  def getGreen(pixel: Int): Int = (pixel >> 8) & 0xFF
  def getBlue(pixel: Int): Int = (pixel >> 0) & 0xFF

  def toRegionedImage(image: BufferedImage, regionWidth: Int, regionHeight: Int): RegionedImage = {
    val (width, height) = (image.getWidth, image.getHeight)

    if (width % regionWidth != 0 || height % regionHeight != 0) {
      throw new UnsupportedOperationException("Region dimensions must be a multiple of the image's dimensions.")
    }

    if (image.getType != BufferedImage.TYPE_3BYTE_BGR) {
      throw new UnsupportedOperationException(s"Image Type (${image.getType}) is not supported. RGB type (${BufferedImage.TYPE_INT_RGB}) only.")
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
}
