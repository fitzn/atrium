//
// JPEGQuantization.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.core

import javax.imageio.plugins.jpeg.JPEGQTable

object JPEGQuantization {

  private val DIMENSION = 8

  def getJavaLuminanceQuantizers(scaleFactor: Float): Vector[Vector[Int]] = {
    JPEGQTable.K1Luminance.getScaledInstance(scaleFactor, true).getTable.grouped(DIMENSION).map(_.toVector).toVector
  }

  def getJavaChrominanceQuantizers(scaleFactor: Float): Vector[Vector[Int]] = {
    JPEGQTable.K2Chrominance.getScaledInstance(scaleFactor, true).getTable.grouped(DIMENSION).map(_.toVector).toVector
  }

  def quantize(channel: Vector[Vector[Double]], quality: Int): QuantizedMatrix = {
    require(channel.size == DIMENSION && channel.head.size == DIMENSION, s"quantize() only works on ${DIMENSION}x$DIMENSION channels")

    val scaleFactor = getScaleFactor(quality)
    val quantizers = getJavaLuminanceQuantizers(scaleFactor)

    val coefficients = channel.zip(quantizers).map {
      case (dctRow, quantRow) => dctRow.zip(quantRow).map {
        case (coef, quant) => (coef / quant).round.toInt
      }
    }

    QuantizedMatrix(quality, coefficients)
  }

  def unquantize(matrix: QuantizedMatrix): Vector[Vector[Double]] = {
    require(
      matrix.coefficients.size == DIMENSION && matrix.coefficients.head.size == DIMENSION,
      s"Malformed QuantizedMatrix; it is not ${DIMENSION}x$DIMENSION"
    )

    val scaleFactor = getScaleFactor(matrix.quality)
    val quantizers = getJavaLuminanceQuantizers(scaleFactor)

    matrix.coefficients.zip(quantizers).map {
      case (coefRow, quantRow) => coefRow.zip(quantRow).map {
        case (coef, quant) => coef * quant.toDouble
      }
    }
  }

  //
  // Internal helpers
  //

  protected def getScaleFactor(quality: Int): Float = {
    require(quality >= 0 && quality <= 100, s"Illegal quality factor: ($quality) must be between 0 and 100.")

    if (quality < 50) {
      50 / quality.toFloat
    } else {
      2 - (quality.toFloat / 50)
    }
  }
}
