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
  val YCC_BIT_DEPTH = 8
  val YCC_BIT_DEPTH_CONSTANT = Math.pow(2, YCC_BIT_DEPTH - 1)

  def getRed(pixel: Int): Int = (pixel >> 16) & 0xFF
  def getGreen(pixel: Int): Int = (pixel >> 8) & 0xFF
  def getBlue(pixel: Int): Int = (pixel >> 0) & 0xFF
  def getPixel(r: Int, g: Int, b: Int): Int = (COLOR_ALPHA_CONSTANT << 24) | ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | (b & 0xFF)

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

    val yccPixels = region.pixels.map(rgbPixel => {
      val (r, g, b) = (getRed(rgbPixel), getGreen(rgbPixel), getBlue(rgbPixel))
      val y = (0.299 * r) + (0.587 * g) + (0.114 * b)
      val cb = (-0.1687 * r) - (0.3313 * g) + (0.5 * b) + YCC_BIT_DEPTH_CONSTANT
      val cr = (0.5 * r) - (0.4187 * g) - (0.0813 * b) + YCC_BIT_DEPTH_CONSTANT
      YCCPixel(y, cb, cr)
    })

    YCCRegion(region.topLeftX, region.topLeftY, region.width, region.height, yccPixels)
  }

  def toRGBRegion(region: YCCRegion): RGBRegion = {

    val rgbPixels = region.pixels.map(yccPixel => {
      val r = (yccPixel.y + 1.40200 * (yccPixel.cr - YCC_BIT_DEPTH_CONSTANT)).round.toInt
      val g = (yccPixel.y - 0.34414 * (yccPixel.cb - YCC_BIT_DEPTH_CONSTANT) - 0.71414 * (yccPixel.cr - YCC_BIT_DEPTH_CONSTANT)).round.toInt
      val b = (yccPixel.y + 1.77200 * (yccPixel.cb - YCC_BIT_DEPTH_CONSTANT)).round.toInt
      getPixel(r, g, b)
    })

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
}
