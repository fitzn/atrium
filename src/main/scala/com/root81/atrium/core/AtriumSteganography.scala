//
// AtriumSteganography.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.core

object AtriumSteganography {

  // NOTE: This matches JPEGQuantization.DIMENSION.
  private val DIMENSION = 8

  private val BITS_IN_BYTE = 8

  // Starting at the top left, where x coordinate moves left to right, and y coordinate moves top to bottom.
  // These coordinates are reversed from the normal (row, column) iteration; x is column, y is row.
  val JPEG_ZIG_ZAG_XY = Vector(
    (0,0), (1,0), (0,1), (0,2), (1,1), (2,0), (3,0), (2,1),
    (1,2), (0,3), (0,4), (1,3), (2,2), (3,1), (4,0), (5,0),
    (4,1), (3,2), (2,3), (1,4), (0,5), (0,6), (1,5), (2,4),
    (3,3), (4,2), (5,1), (6,0), (7,0), (6,1), (5,2), (4,3),
    (3,4), (2,5), (1,6), (0,7), (1,7), (2,6), (3,5), (4,4),
    (5,3), (6,2), (7,1), (7,2), (6,3), (5,4), (4,5), (3,6),
    (2,7), (3,7), (4,6), (5,5), (6,4), (7,3), (7,4), (6,5),
    (5,6), (4,7), (5,7), (6,6), (7,5), (7,6), (6,7), (7,7)
  )

  // The (x, y) cell coordinates that will hold 1 of the byte's 8 bits, from highest-order bit to lowest-order bit.
  // The starting position is selected so that it does not interfere with the low-frequency, high-impact bits, but
  // it's not so far into the expected-zero territory of the matrix that it would hurt compression.
  val ENCODE_INDEX_DEFAULT = 4

  def getBitCellsXY(encodePositionStart: Int): Vector[(Int, Int)] = {
    JPEG_ZIG_ZAG_XY.slice(encodePositionStart, encodePositionStart + BITS_IN_BYTE)
  }

  /**
    * Encodes the byte into the QuantizedMatrix's coefficients starting at the specified coefficient index.
    * NOTE: the decoder needs the same encode index, so it's not useful dynamically, but aids in testing.
    */
  def encode(byte: Byte, matrix: QuantizedMatrix, encodeStartIndex: Int = ENCODE_INDEX_DEFAULT): QuantizedMatrix = {
    require(
      matrix.coefficients.size == DIMENSION && matrix.coefficients.head.size == DIMENSION,
      s"Malformed QuantizedMatrix; it is not ${DIMENSION}x$DIMENSION"
    )

    require(encodeStartIndex >= 0 && encodeStartIndex < 64, s"invalid encoding start index $encodeStartIndex, must be 0 <= index < 64.")

    // Iterate through the bits in "byte" from highest-order to lowest and produce a Map with the coded bit value for specified bit cells.
    val bitCellsXY = getBitCellsXY(encodeStartIndex)
    val coefficientsByCoordinates = (0 to 7).reverse.zip(bitCellsXY).map {
      case (shift, (x, y)) => {
        val originalCoefficient = matrix.coefficients(y)(x)
        val coefficientIsEven = originalCoefficient % 2 == 0
        val coefficientShouldBeEven = ((byte >> shift) & 0x1) == 0

        // If the parity of the coefficient differs from the bit position's, then increment the coefficient to correct it.
        // NOTE: Another approach would be to check the coefficient's sign and when modifying it,
        //       move the value towards zero (when it's non-zero) rather than always adding.
        val addend = if (coefficientIsEven == coefficientShouldBeEven) 0 else 1
        val updatedCoefficient = originalCoefficient + addend

        ((y, x), updatedCoefficient)
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
  def decode(matrix: QuantizedMatrix, encodeStartIndex: Int = ENCODE_INDEX_DEFAULT): Byte = {
    require(
      matrix.coefficients.size == DIMENSION && matrix.coefficients.head.size == DIMENSION,
      s"Malformed QuantizedMatrix; it is not ${DIMENSION}x$DIMENSION"
    )

    require(encodeStartIndex >= 0 && encodeStartIndex < 64, s"invalid encoding start index $encodeStartIndex, must be 0 <= index < 64.")

    // The bit cell coordinates are highest-order to lowest, so loop through and OR in the coefficient parity bit.
    var result = 0
    val bitCellsXY = getBitCellsXY(encodeStartIndex)

    bitCellsXY.foreach {
      case (x, y) => result = (result << 1) | (matrix.coefficients(y)(x) & 0x1)
    }

    result.toByte
  }
}
