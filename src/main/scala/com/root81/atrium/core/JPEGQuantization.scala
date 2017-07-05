//
// JPEGQuantization.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.core

import javax.imageio.plugins.jpeg.JPEGQTable

object JPEGQuantization {

  val DIMENSION = 8

  def getJavaLuminanceQuantizers(quality: Int): Vector[Vector[Int]] = {
    val scaleFactor = getScaleFactor(quality)
    JPEGQTable.K1Luminance.getScaledInstance(scaleFactor, true).getTable.grouped(DIMENSION).map(_.toVector).toVector
  }

  def getJavaChrominanceQuantizers(scaleFactor: Float): Vector[Vector[Int]] = {
    JPEGQTable.K2Chrominance.getScaledInstance(scaleFactor, true).getTable.grouped(DIMENSION).map(_.toVector).toVector
  }

  //
  // Internal helpers
  //

  protected def getScaleFactor(quality: Int): Float = {
    require(quality >= 0 && quality <= 100, s"Illegal JPEG quality value: ($quality) must be between 0 and 100.")

    if (quality < 50) {
      50 / quality.toFloat
    } else {
      2 - (quality.toFloat / 50)
    }
  }
}
