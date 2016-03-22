//
// JPEGQuantization.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.core

object JPEGQuantization {

  private val DIMENSION = 8

  val JPEG_QUANTIZERS_50 = Vector(
    Vector( 16,  11,  10,  16,  24,  40,  51,  61),
    Vector( 12,  12,  14,  19,  26,  58,  60,  55),
    Vector( 14,  13,  16,  24,  40,  57,  69,  56),
    Vector( 14,  17,  22,  29,  51,  87,  80,  62),
    Vector( 18,  22,  37,  56,  68, 109, 103,  77),
    Vector( 24,  35,  55,  64,  81, 104, 113,  92),
    Vector( 49,  64,  78,  87, 103, 121, 120, 101),
    Vector( 72,  92,  95,  98, 112, 100, 103,  99)
  )

  def quantize(channel: Vector[Vector[Double]], quality: Int): QuantizedMatrix = {
    require(channel.size == DIMENSION && channel.head.size == DIMENSION, s"quantize() only works on ${DIMENSION}x$DIMENSION channels")

    val scaleFactor = getScaleFactor(quality)

    val coefficients = channel.zip(JPEG_QUANTIZERS_50).map {
      case (dctRow, quantRow) => dctRow.zip(quantRow).map {
        case (coef, quant) => (coef / (quant * scaleFactor)).round.toInt
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

    matrix.coefficients.zip(JPEG_QUANTIZERS_50).map {
      case (coefRow, quantRow) => coefRow.zip(quantRow).map {
        case (coef, quant) => coef * quant * scaleFactor
      }
    }
  }

  //
  // Internal helpers
  //

  protected def getScaleFactor(quality: Int): Double = {
    require(quality >= 0 && quality <= 100, s"Illegal quality factor: ($quality) must be between 0 and 100.")

    if (quality < 50) {
      50D / quality
    } else {
      2 - (quality / 50D)
    }
  }
}
