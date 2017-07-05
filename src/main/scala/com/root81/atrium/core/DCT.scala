//
// DCT.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.core

object DCT {

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
}