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
  private val BIT_CELLS_SET = AtriumSteganography.BIT_CELLS.toSet

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
    val outputMatrix = AtriumSteganography.encode(0.toByte, EXAMPLE_MATRIX).coefficients

    INDICES.foreach(row => {
      INDICES.foreach(col => {
        if (!BIT_CELLS_SET.contains((row, col))) {
          assert(inputMatrix(row)(col) == outputMatrix(row)(col))
        }
      })
    })

    // Try it with another byte value.
    val outputMatrix2 = AtriumSteganography.encode(0xFF.toByte, EXAMPLE_MATRIX).coefficients

    INDICES.foreach(row => {
      INDICES.foreach(col => {
        if (!BIT_CELLS_SET.contains((row, col))) {
          assert(inputMatrix(row)(col) == outputMatrix2(row)(col))
        }
      })
    })
  }

  it should "encode the bit cells parity to all even on a zero byte" in {
    val outputMatrix = AtriumSteganography.encode(0.toByte, EXAMPLE_MATRIX).coefficients

    BIT_CELLS_SET.foreach {
      case (row, col) => {
        assert((outputMatrix(row)(col) & 0x1) == 0)
      }
    }
  }

  it should "encode the bit cells parity to all odd on the 0xFF byte" in {
    val outputMatrix = AtriumSteganography.encode(0xFF.toByte, EXAMPLE_MATRIX).coefficients

    BIT_CELLS_SET.foreach {
      case (row, col) => assert((outputMatrix(row)(col) & 0x1) == 1)
    }
  }

  it should "encode the bit cells parity correctly on an arbitrary byte" in {
    val outputMatrix = AtriumSteganography.encode(0x55.toByte, EXAMPLE_MATRIX).coefficients

    INDICES.foreach(index => {
      val (row, col) = AtriumSteganography.BIT_CELLS(index)
      assert((outputMatrix(row)(col) & 0x1) == (index & 0x1))
    })
  }

  it should "decode the bit cells parity correctly on an arbitrary bytes" in {
    val byte = AtriumSteganography.decode(EXAMPLE_MATRIX)
    assert(byte == 0xDB.toByte)

    val byte2 = AtriumSteganography.decode(EXAMPLE_MATRIX_2)
    assert(byte2 == 0x0F.toByte)
  }
}
