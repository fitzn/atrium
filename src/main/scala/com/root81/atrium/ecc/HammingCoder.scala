//
// HammingCoder.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.ecc

trait HammingCoder {
  protected val hamming = new HammingCoderImpl()
}

protected[ecc] class HammingCoderImpl {

  // Pre-compute the 4-bits to codeword lookup table.
  private val codewordBy4Bits = (0 to 15).toList.
    map(b => (b, getCodeword(b.toByte))).toMap

  /**
   * Network order (high 4 bits are in the first byte, low 4 bits in the second).
   */
  def toHamming84(bytes: Array[Byte]): Array[Byte] = {
    bytes.flatMap(b => {
      val (lowBits, highBits) = (b & 0xf, (b & 0xf0) >> 4)

      List(codewordBy4Bits(highBits), codewordBy4Bits(lowBits))
    })
  }

  //
  // Internal helpers
  //

  protected def getCodeword(bits: Byte): Byte = {
    require((bits & 0xf0) == 0, "Class only supports lowest 4 bits set: " + bits)

    val d4 = bits & 0x1
    val d3 = (bits >> 1) & 0x1
    val d2 = (bits >> 2) & 0x1
    val d1 = (bits >> 3) & 0x1

    val p1 = d1 ^ d2 ^ d4
    val p2 = d1 ^ d3 ^ d4
    val p3 = d2 ^ d3 ^ d4
    val p4 = d1 ^ d2 ^ d3 ^ d4 ^ p1 ^ p2 ^ p3

    // Order the bits left to right and OR them into the final byte.
    List(p1, p2, d1, p3, d2, d3, d4, p4).fold(0) {
      case (b, v) => (b << 1) | v
    }.toByte
  }
}

