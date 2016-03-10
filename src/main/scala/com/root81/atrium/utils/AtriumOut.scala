//
// AtriumOut.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.utils

import com.root81.atrium.core.{YCCRegion, RGBRegion}

object AtriumOut {

  import AtriumLogger._
  import ImageConversions._

  def print(region: RGBRegion): Unit = {
    val rows = region.pixels.grouped(region.width).toList
    if (rows.size != region.height) {
      warn(s"Malformed RGBRegion: w=${region.width}, h=${region.height}, pix=${region.pixels.size}")
    }

    rows.foreach(pixelIntRow => {
      // Map each pixel integer to RGB and write the 3-tuple of byte integers with 13 bytes
      val rgbRow = pixelIntRow.map(pixel => List(getRed(pixel), getGreen(pixel), getBlue(pixel)))
      val formattedRow = rgbRow.map(rgb => "(" + rgb.map(i => ("  " + i).takeRight(3)).mkString(",") + ")").mkString(" ")
      println(formattedRow)
    })
  }

  def print(region: YCCRegion): Unit = {
    val rows = region.pixels.grouped(region.width).toList
    if (rows.size != region.height) {
      warn(s"Malformed YCCRegion: w=${region.width}, h=${region.height}, pix=${region.pixels.size}")
    }

    rows.foreach(row => {
      // For now, just print the Y.
      val formattedRow = row.map(pixel => "(%.3f)".format(pixel.y)).mkString(" ")
      println(formattedRow)
    })
  }

  def print(matrix: Vector[Vector[Double]]): Unit = {
    matrix.foreach(row => {
      val formattedRow = row.map(d => "(%.3f)".format(d)).mkString(" ")
      println(formattedRow)
    })
  }
}
