//
// HammingCoder.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.ecc

import com.root81.atrium.utils.AtriumLogger
import java.util

// This is a class so that we can test the internal methods more easily
// since Scala doesn't allow inheritance from objects.
class HammingCoder {

  // Pre-compute the 4-bits to codeword lookup table and the reverse.
  private val codewordBy4Bits = (0 to 15).map(_.toByte).toList.
    map(b => (b, getCodeword(b))).toMap

  private val byteByCodeword = codewordBy4Bits.map(_.swap)

  /**
   * Network order (high 4 bits are in the first byte, low 4 bits in the second).
   */
  def toHamming84(bytes: Array[Byte]): Array[Byte] = {
    AtriumLogger.debug(s"HammingCoder: Encoding ${bytes.length} input bytes")

    val codedBytes = bytes.flatMap(b => {
      val (highBits, lowBits) = (((b & 0xf0) >> 4).toByte, (b & 0xf).toByte)

      val (highCodeword, lowCodeword) = (codewordBy4Bits(highBits), codewordBy4Bits(lowBits))

      AtriumLogger.debug(s"HammingCoder: Encoded '${new String(Array(b), "UTF-8")}' - ${byteStr(b)} to (${byteStr(highCodeword)},${byteStr(lowCodeword)})")

      List(highCodeword, lowCodeword)
    })

    AtriumLogger.debug(s"HammingCoder: Encoded ${bytes.length} input bytes to ${codedBytes.length} output bytes")
    codedBytes
  }

  def fromHamming84(bytes: Array[Byte], withCorrection: Boolean = false): Array[Byte] = {
    if (bytes.length % 2 != 0) {
      throw new InvalidLengthException("There must be an even number of encoded bytes: " + bytes.length)
    }

    AtriumLogger.debug(s"HammingCoder: Decoding ${bytes.length} bytes, withCorrection=$withCorrection")

    val decodedBytes = bytes.grouped(2).map(pair =>
      decodeBytePair(pair.head, pair.last, withCorrection)
    ).toArray

    decodedBytes
  }

  //
  // Internal helpers
  //

  protected def decodeBytePair(b0: Byte, b1: Byte, withCorrection: Boolean): Byte = {
    val dataByte0 = decodeByte(b0, withCorrection)
    val dataByte1 = decodeByte(b1, withCorrection)

    // Combine the 4-bit bytes into a single byte.
    val hiBits = dataByte0 << 4    // This is an Int but that's okay.
    val resultByte = (hiBits | dataByte1).toByte

    AtriumLogger.debug(s"HammingCoder: Decoded (${byteStr(b0)},${byteStr(b1)}) to ${byteStr(resultByte)} - '${new String(Array(resultByte), "UTF-8")}'")

    resultByte
  }

  /**
   * Decodes the byte, optionally performing error-correction.
   * If errors are detected (or unrecoverable), then it throws.
   */
  protected def decodeByte(byte: Byte, withCorrection: Boolean): Byte = {
    // If it's a known codeword, the lookup succeeds and we return the 4-bits byte.
    val decodedByte = byteByCodeword.getOrElse(byte, {
      val codewords = byteByCodeword.keys.toList
      val (code, distance) = codewords.map(code => (code, getHammingDistance(byte, code))).minBy(_._2)

      // If correction is desired and distance is 1, use the closest code. Otherwise, throw.
      if (withCorrection && distance == 1) {

        AtriumLogger.debug(s"HammingCoder: correcting '${byteStr(byte)}' -> '${byteStr(code)}'.")

        byteByCodeword(code)    // Safe since the value came from the map's keys.
      } else {
        throw ByteCorruptionException(distance, s"Coded byte ${byteStr(byte)} was corrupted by $distance bits")
      }
    })

    decodedByte
  }

  protected def getHammingDistance(b0: Byte, b1: Byte): Int = {
    val (bits0, bits1) = (getBits(b0), getBits(b1))
    bits0.xor(bits1)
    bits0.cardinality
  }

  protected def getBits(byte: Byte): util.BitSet = {
    val bits = new util.BitSet
    (0 to 7).foreach(shift => bits.set(shift, ((byte >> shift) & 0x1) != 0))
    bits
  }

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

  protected def byteStr(byte: Byte): String = {
    (0 to 7).toList.reverse.map(shift => ((byte >> shift) & 0x1).toString).reduce(_ + _)
  }
}

