//
// AtriumSteganographySpec.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.core

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AtriumSteganographySpec extends FlatSpec {

  private val INDICES = (0 to 7).toVector
  private val ENCODE_INDEX_START = 4
  private val BIT_CELLS_XY = AtriumSteganography.getBitCellsXY(ENCODE_INDEX_START)
  private val BIT_CELLS_XY_SET = BIT_CELLS_XY.toSet

  private val EXAMPLE_MATRIX = QuantizedMatrix(
    quality = 75,
    coefficients = Vector(
      Vector( -16, -79,   6,   7,   1,   0,   1,   0),
      Vector( -24,  11,  27,   8,  -2,  -1,   0,   0),
      Vector(   7,  10,   1,  -6,  -2,   1,   0,   0),
      Vector(  -3,  -1,  -1,  -1,   1,   1,   0,   0),
      Vector(   1,   2,   1,   0,   0,   0,   0,   0),
      Vector(   0,   0,   0,   0,   0,   0,   0,   0),
      Vector(   0,   0,   0,   0,   0,   0,   0,   0),
      Vector(   0,   0,   0,   0,   0,   0,   0,   0)
    )
  )

  private val EXAMPLE_MATRIX_2 = QuantizedMatrix(
    quality = 75,
    coefficients = Vector(
      Vector( -16, -79,   0,   0,   1,   0,   1,   0),
      Vector( -24,   0,   1,   8,  -2,  -1,   0,   0),
      Vector(   0,   1,   1,  -6,  -2,   1,   0,   0),
      Vector(   1,  -1,  -1,  -1,   1,   1,   0,   0),
      Vector(   1,   2,   1,   0,   0,   0,   0,   0),
      Vector(   0,   0,   0,   0,   0,   0,   0,   0),
      Vector(   0,   0,   0,   0,   0,   0,   0,   0),
      Vector(   0,   0,   0,   0,   0,   0,   0,   0)
    )
  )

  behavior of "AtriumSteganography"

  it should "leave all non-bit cells unchanged" in {
    val inputMatrix = EXAMPLE_MATRIX.coefficients
    val outputMatrix = AtriumSteganography.encode(0.toByte, EXAMPLE_MATRIX, ENCODE_INDEX_START).coefficients

    INDICES.foreach(row => {
      INDICES.foreach(col => {
        // Remember, row is y, col is x, so we need to reverse these when looking-up in an (x,y) set.
        if (!BIT_CELLS_XY_SET.contains((col, row))) {
          assert(inputMatrix(row)(col) == outputMatrix(row)(col))
        }
      })
    })

    // Try it with another byte value.
    val outputMatrix2 = AtriumSteganography.encode(0xFF.toByte, EXAMPLE_MATRIX, ENCODE_INDEX_START).coefficients

    INDICES.foreach(row => {
      INDICES.foreach(col => {
        if (!BIT_CELLS_XY_SET.contains((col, row))) {
          assert(inputMatrix(row)(col) == outputMatrix2(row)(col))
        }
      })
    })
  }

  it should "encode the bit cells parity to all even on a zero byte" in {
    val outputMatrix = AtriumSteganography.encode(0.toByte, EXAMPLE_MATRIX, ENCODE_INDEX_START).coefficients

    // Remember (x, y) corresponds to (col, row) from top left.
    BIT_CELLS_XY_SET.foreach {
      case (x, y) => assert((outputMatrix(y)(x) & 0x1) == 0)
    }
  }

  it should "encode the bit cells parity to all odd on the 0xFF byte" in {
    val outputMatrix = AtriumSteganography.encode(0xFF.toByte, EXAMPLE_MATRIX, ENCODE_INDEX_START).coefficients

    // Remember (x, y) corresponds to (col, row) from top left.
    BIT_CELLS_XY_SET.foreach {
      case (x, y) => assert((outputMatrix(y)(x) & 0x1) == 1)
    }
  }

  it should "encode the bit cells parity correctly on an arbitrary byte" in {
    val outputMatrix = AtriumSteganography.encode(0x55.toByte, EXAMPLE_MATRIX, ENCODE_INDEX_START).coefficients

    INDICES.foreach(index => {
      val (x, y) = BIT_CELLS_XY(index)
      assert((outputMatrix(y)(x) & 0x1) == (index & 0x1))
    })
  }

  it should "decode the bit cells parity correctly on an arbitrary byte" in {
    val byte = AtriumSteganography.decode(EXAMPLE_MATRIX, ENCODE_INDEX_START)
    assert(byte == 0xB7.toByte)

    val byte2 = AtriumSteganography.decode(EXAMPLE_MATRIX_2, ENCODE_INDEX_START)
    assert(byte2 == 0x1F.toByte)
  }
}
