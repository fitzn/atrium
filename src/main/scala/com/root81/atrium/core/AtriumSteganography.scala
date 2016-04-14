//
// AtriumSteganography.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.core

object AtriumSteganography {

  // NOTE: This matches JPEGQuantization.DIMENSION.
  private val DIMENSION = 8

  /**
    * The (x, y) cell coordinates that will hold 1 of the byte's 8 bits, from highest-order bit to lowest-order bit.
    * We use these cells based on the zig-zag ordering of the JPEG standard and the way it uses the DCT.
    */
  val BIT_CELLS = Vector(
    (1, 1), // highest-order bit
    (2, 0),
    (3, 0),
    (2, 1),
    (1, 2),
    (0, 3),
    (0, 4),
    (1, 3)  // lowest-order bit
  )

  def encode(byte: Byte, matrix: QuantizedMatrix): QuantizedMatrix = {
    require(
      matrix.coefficients.size == DIMENSION && matrix.coefficients.head.size == DIMENSION,
      s"Malformed QuantizedMatrix; it is not ${DIMENSION}x$DIMENSION"
    )

    // Iterate through the bits in "byte" from highest-order to lowest and produce a Map with the coded bit value for specified bit cells.
    val coefficientsByCoordinates = (0 to 7).reverse.zip(BIT_CELLS).map {
      case (shift, (row, col)) => {
        val originalCoefficient = matrix.coefficients(row)(col)
        val coefficientIsEven = originalCoefficient % 2 == 0
        val coefficientShouldBeEven = ((byte >> shift) & 0x1) == 0

        // If the parity of the coefficient differs from the bit position's, then increment the coefficient to correct it.
        // NOTE: Another approach would be to check the coefficient's sign and when modifying it,
        //       move the value towards zero (when it's non-zero) rather than always adding.
        val addend = if (coefficientIsEven == coefficientShouldBeEven) 0 else 1
        val updatedCoefficient = originalCoefficient + addend

        ((row, col), updatedCoefficient)
      }
    }.toMap

    // Loop through the original matrix coefficients and overwrite the value at the 8 bit cell coordinates.
    val resultCoefficients =
      (0 until DIMENSION).toVector.map(row => {
        (0 until DIMENSION).toVector.map(col => {
          coefficientsByCoordinates.getOrElse((row, col), matrix.coefficients(row)(col))
        })
      })

    QuantizedMatrix(matrix.quality, resultCoefficients)
  }

  /**
    * This method reads a single byte from the 8 bit cells in the provided matrix coefficients.
    * This method does not "reset" the matrix since it cannot know the original coefficient.
    */
  def decode(matrix: QuantizedMatrix): Byte = {
    require(
      matrix.coefficients.size == DIMENSION && matrix.coefficients.head.size == DIMENSION,
      s"Malformed QuantizedMatrix; it is not ${DIMENSION}x$DIMENSION"
    )

    // The bit cell coordinates are highest-order to lowest, so loop through and OR in the coefficient parity bit.
    var result = 0
    BIT_CELLS.foreach {
      case (row, col) => result = (result << 1) | (matrix.coefficients(row)(col) & 0x1)
    }
    result.toByte
  }
}
