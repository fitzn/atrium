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

  private val ZERO_BYTE = 0.toByte
  private val ONE_BYTE = (-1).toByte
  private val QUALITY = 75
  private val ENCODE_INDEX_START = 4

  private val EXAMPLE_CHANNEL_DCT = Vector(
    Vector(534.8,  24.4, -20.4,  15.6, -11.6,  20.3, -0.1, -0.5),
    Vector(-41.9, -53.8,  -6.3,   9.8,  13.0, -28.1, -0.5, -0.1),
    Vector(-34.5,  34.7, -15.7,   0.1,  -0.6,   0.2, -0.3,  0.0),
    Vector( 27.9, -44.8, -21.6,  14.7,   0.2,   0.0, -0.1,  0.1),
    Vector(-36.1, -11.4,  -0.1, -27.1,  -0.4,   0.3,  0.1,  0.0),
    Vector(-12.4,  17.7,  -0.3,  -0.3,  -0.5,  -0.0,  0.1,  0.3),
    Vector(  0.2,   0.1,   0.8,  -0.1,   0.7,   0.0, -0.0, -0.1),
    Vector(  0.2,  -0.3,  -0.4,   0.3,  -0.9,   0.1,  0.0,  0.4)
  )

  behavior of "AtriumSteganography"

  it should "encode and decode a zero byte into a DCT channel matrix" in {

    val encodedChannel = AtriumSteganography.encode(ZERO_BYTE, EXAMPLE_CHANNEL_DCT, QUALITY, ENCODE_INDEX_START)
    val decodedByte = AtriumSteganography.decode(encodedChannel, QUALITY, ENCODE_INDEX_START)

    assert(decodedByte == ZERO_BYTE)
  }

  it should "encode and decode a byte of all 1's into a DCT channel matrix" in {

    val encodedChannel = AtriumSteganography.encode(ONE_BYTE, EXAMPLE_CHANNEL_DCT, QUALITY, ENCODE_INDEX_START)
    val decodedByte = AtriumSteganography.decode(encodedChannel, QUALITY, ENCODE_INDEX_START)

    assert(decodedByte == ONE_BYTE)
  }
}
