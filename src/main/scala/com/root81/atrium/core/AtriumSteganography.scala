//
// AtriumSteganography.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.core

object AtriumSteganography {

  private val DIMENSION = JPEGQuantization.DIMENSION
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
    * Encodes the byte into the channel by quantizing it and modulating its coefficients starting at the specified coefficient index.
    * NOTE: the decoder needs the same encode index, so it's not useful dynamically, but aids in testing.
    * NOTE: This method assumes the channel is the luminance channel.
    */
  def encode(byte: Byte, channelDCT: Vector[Vector[Double]], quality: Int, encodeStartIndex: Int = ENCODE_INDEX_DEFAULT): Vector[Vector[Double]] = {

    val quantizers = JPEGQuantization.getJavaLuminanceQuantizers(quality)

    require(channelDCT.size == DIMENSION && channelDCT.head.size == DIMENSION, s"Malformed DCTRegion channel0 - it is not ${DIMENSION}x$DIMENSION")
    require(encodeStartIndex >= 0 && encodeStartIndex < 64, s"invalid encoding start index $encodeStartIndex, must be 0 <= index < 64.")

    val bitCellsXY = getBitCellsXY(encodeStartIndex)
    val bitsToEncode = (0 to 7).toList.reverse.map(shift => (byte >> shift) & 0x1)   // high-order to low-order bit, set to 1 if odd.

    val encodedChannel = channelDCT.zipWithIndex.map {
      case (rowCoefficients, yIndex) => {
        rowCoefficients.zipWithIndex.map {
          case (coefficient, xIndex) => {
            val bitIndex = bitCellsXY.indexOf((xIndex, yIndex))   // Defined if these coordinates will store a bit.
            if (bitIndex != -1) {
              // Move the channel value up or down so that when it's quantized, it produces the same parity as the bit.
              val bit = bitsToEncode(bitIndex)
              val quantizer = quantizers(yIndex)(xIndex)
              adjustDCTCoefficientToQuantizedParity(coefficient, quantizer, bit)

            } else {
              // Copy the dct coefficient over directly since it's not storing a bit.
              coefficient
            }
          }
        }
      }
    }

    encodedChannel
  }

  /**
    * Reads a single byte from the 8 bit cells in the provided channel starting at the specified coefficient index.
    * NOTE: This method assumes the channel is the luminance channel.
    */
  def decode(channelDCT: Vector[Vector[Double]], quality: Int, encodeStartIndex: Int = ENCODE_INDEX_DEFAULT): Byte = {

    val quantizers = JPEGQuantization.getJavaLuminanceQuantizers(quality)

    require(channelDCT.size == DIMENSION && channelDCT.head.size == DIMENSION, s"Malformed channel dimensions - it is not ${DIMENSION}x$DIMENSION")
    require(encodeStartIndex >= 0 && encodeStartIndex < 64, s"invalid encoding start index $encodeStartIndex, must be 0 <= index < 64.")

    val bitCellsXY = getBitCellsXY(encodeStartIndex)
    var result = 0

    // Decode the bits in order, so we can shift the result left for each new bit.
    bitCellsXY.foreach {
      case (x, y) => {
        val dctCoefficient = channelDCT(y)(x)
        val quantizer = quantizers(y)(x)
        val quantizedValue = quantizeCoefficient(dctCoefficient, quantizer)

        result = (result << 1) | (quantizedValue & 0x1)
      }
    }

    result.toByte
  }

  //
  // Internal helpers
  //

  protected def adjustDCTCoefficientToQuantizedParity(dctCoefficient: Double, quantizer: Int, parity: Int): Double = {
    val quantizedValue = quantizeCoefficient(dctCoefficient, quantizer)

    if ((quantizedValue & 0x1) == (parity & 0x1)) {
      // Coefficient already produces the desired parity when quantized and rounded.
      // However, if it's right on the edge of flipping the parity when quantized, move it more squarely within the quantized range.
      val correctionFactor = getQuantizerCorrectionFactor(quantizer)
      val higherCoefficient = dctCoefficient + correctionFactor
      val lowerCoefficient = dctCoefficient - correctionFactor

      if (quantizeCoefficient(higherCoefficient, quantizer) != quantizedValue) {
        lowerCoefficient
      } else if (quantizeCoefficient(lowerCoefficient, quantizer) != quantizedValue) {
        higherCoefficient
      } else {
        dctCoefficient
      }
    } else {
      // The coefficient *rounds* to the wrong parity, so move the coefficient to the next-closest multiple of the quantizer.
      val roundedQuantizerMultiple = quantizedValue * quantizer.toDouble

      val quantizedValueAddend = if (dctCoefficient < roundedQuantizerMultiple) {
        // Coefficient rounded up to quantizedValue, but that was wrong parity.
        // Next-closest parity is the floor (rather than rounding), so subtract one from the quantized value.
        -1
      } else {
        // Coefficient rounded down to quantizedValue, but that was wrong parity.
        // Next-closest parity is the ceiling, so add one to the quantized value.
        1
      }

      // Adjust the quantized value in the correct direction and then unquantize it.
      (quantizedValue + quantizedValueAddend) * quantizer.toDouble
    }
  }

  protected def quantizeCoefficient(dctCoefficient: Double, quantizer: Int): Int = {
    (dctCoefficient / quantizer).round.toInt
  }

  protected def getQuantizerCorrectionFactor(quantizer: Int): Double = {
    quantizer / 16D
  }
}
