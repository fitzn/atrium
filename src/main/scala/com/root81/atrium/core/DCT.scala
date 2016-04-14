//
// DCT.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.core

object DCT {

  private val RANGE_HALF = 128D

  def applyRegionDCT(region: YCCRegion): DCTRegion = {
    require(
      region.width * region.height == region.pixels.size,
      s"malformed region: width=${region.width} * height=${region.height} should equal pixels=${region.pixels.size}"
    )

    require(region.width == region.height, "Atrium DCT only works on square regions")

    def normalizeRangeValues(values: Vector[Double]): Vector[Vector[Double]] = values.map(_ - RANGE_HALF).grouped(region.width).toVector
    val pixels = region.pixels.toVector
    val yMat = normalizeRangeValues(pixels.map(_.y))
    val cbMat = normalizeRangeValues(pixels.map(_.cb))
    val crMat = normalizeRangeValues(pixels.map(_.cr))

    DCTRegion(region.topLeftX, region.topLeftY, region.width, region.height, applyDCT(yMat), applyDCT(cbMat), applyDCT(crMat))
  }

  def unapplyRegionDCT(region: DCTRegion): YCCRegion = {
    require(region.channel0.isEmpty || region.channel0.map(_.size).toSet.size == 1, "Region channel0 has rows with different number of columns")
    require(region.channel1.isEmpty || region.channel1.map(_.size).toSet.size == 1, "Region channel1 has rows with different number of columns")
    require(region.channel2.isEmpty || region.channel2.map(_.size).toSet.size == 1, "Region channel2 has rows with different number of columns")

    require(region.channel0.isEmpty || region.channel0.head.size == region.width, "Region channel0 width differs from region's declared width")
    require(region.channel1.isEmpty || region.channel1.head.size == region.width, "Region channel1 width differs from region's declared width")
    require(region.channel2.isEmpty || region.channel2.head.size == region.width, "Region channel2 width differs from region's declared width")

    require(region.channel0.size == region.height, "Region channel0 height differs from region's declared height")
    require(region.channel1.size == region.height, "Region channel1 height differs from region's declared height")
    require(region.channel2.size == region.height, "Region channel2 height differs from region's declared height")

    def denormalizeRangeValues(matrix: Vector[Vector[Double]]): Vector[Double] = matrix.flatMap(_.map(_ + RANGE_HALF))
    val yValues = denormalizeRangeValues(applyIDCT(region.channel0))
    val cbValues = denormalizeRangeValues(applyIDCT(region.channel1))
    val crValues = denormalizeRangeValues(applyIDCT(region.channel2))

    require(yValues.size == cbValues.size && cbValues.size == crValues.size, "DCTRegion produced channel value vectors of different lengths")

    val pixels = yValues.zip(cbValues).zip(crValues).map {
      case ((y, cb), cr) => YCCPixel(y, cb, cr)
    }

    YCCRegion(region.topLeftX, region.topLeftY, region.width, region.height, pixels.toList)
  }

  def applyDCT(matrix: Vector[Vector[Double]]): Vector[Vector[Double]] = {
    if (matrix.isEmpty) return Vector.empty[Vector[Double]]

    require(matrix.map(_.size).toSet.size == 1, "matrix has rows with different number of columns")
    require(matrix.size == matrix.head.size, "Atrium DCT only works on square matrices")

    val size = matrix.size
    val divFactor = 2D / size
    val cosDivisor = 2D * size
    val alpha = makeAlpha(size)
    val indices = (0 until size).toVector

    indices.map(u => {
      indices.map(v => {
        // Do the sum for each (u, v) index across all pixel values.
        val sum = indices.flatMap(x => {
          indices.map(y => {
            matrix(x)(y) * math.cos((((2 * x) + 1) * u * Math.PI) / cosDivisor) * math.cos((((2 * y) + 1) * v * Math.PI) / cosDivisor)
          })
        }).sum

        divFactor * alpha(u) * alpha(v) * sum
      })
    })
  }

  def applyIDCT(matrix: Vector[Vector[Double]]): Vector[Vector[Double]] = {
    if (matrix.isEmpty) return Vector.empty[Vector[Double]]

    require(matrix.map(_.size).toSet.size == 1, "matrix has rows with different number of columns")
    require(matrix.size == matrix.head.size, "Atrium DCT only works on square matrices")

    val size = matrix.size
    val divFactor = 2D / size
    val cosDivisor = 2D * size
    val alpha = makeAlpha(size)
    val indices = (0 until size).toVector

    indices.map(x => {
      indices.map(y => {
        val sum = indices.flatMap(u => {
          indices.map(v => {
            alpha(u) * alpha(v) * matrix(u)(v) * math.cos((((2 * x) + 1) * u * Math.PI) / cosDivisor) * math.cos((((2 * y) + 1) * v * Math.PI) / cosDivisor)
          })
        }).sum

        divFactor * sum
      })
    })

  }

  //
  // Internal helpers
  //

  protected def makeAlpha(count: Int): Vector[Double] = {
    if (count == 0) {
      Vector.empty[Double]
    } else {
      ((1D / Math.sqrt(2D)) :: List.fill(count - 1)(1D)).toVector
    }
  }

  protected def matMult(mat0: Vector[Vector[Double]], mat1: Vector[Vector[Double]]): Vector[Vector[Double]] = {
    require(mat0.map(_.size).toSet.size == 1, "mat0 has rows with different number of columns")
    require(mat1.map(_.size).toSet.size == 1, "mat1 has rows with different number of columns")
    require(mat0.isEmpty || mat0.head.size == mat1.size, "mat0 number of columns does not equal mat1 number of rows")

    // Convert mat1 to a column-based matrix for easier multiplication.
    val colMat1 = toColumnMat(mat1)

    mat0.map(row => {
      colMat1.map(col => {
        row.zip(col).map {
          case (r, c) => r * c
        }.sum
      })
    })
  }

  protected def toColumnMat(matrix: Vector[Vector[Double]]): Vector[Vector[Double]] = {
    require(matrix.map(_.size).toSet.size == 1, "matrix has rows with different number of columns")

    if (matrix.isEmpty) return Vector.empty[Vector[Double]]

    matrix.head.indices.toVector.map(col => {
      // Drop some number of columns within the row to get to the current column and take the head of the row.
      matrix.map(_.drop(col).head)
    })
  }
}