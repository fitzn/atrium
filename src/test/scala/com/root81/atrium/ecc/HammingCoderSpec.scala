//
// HammingCoderSpec.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.ecc

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class HammingCoderSpec extends FlatSpec {

  object TestHammingCoder extends HammingCoderImpl {
    override def getCodeword(bits: Byte): Byte = super.getCodeword(bits)
  }

  val CODEWORD_BY_BITS = Map(
    0x0.toByte -> 0x0.toByte,
    0x8.toByte -> 0xe1.toByte,
    0x4.toByte -> 0x99.toByte,
    0xc.toByte -> 0x78.toByte,
    0x2.toByte -> 0x55.toByte,
    0xa.toByte -> 0xb4.toByte,
    0x6.toByte -> 0xcc.toByte,
    0xe.toByte -> 0x2d.toByte,
    0x1.toByte -> 0xd2.toByte,
    0x9.toByte -> 0x33.toByte,
    0x5.toByte -> 0x4b.toByte,
    0xd.toByte -> 0xaa.toByte,
    0x3.toByte -> 0x87.toByte,
    0xb.toByte -> 0x66.toByte,
    0x7.toByte -> 0x1e.toByte,
    0xf.toByte -> 0xff.toByte
  )

  behavior of "The Hamming coder"

  it should "encode 4 bits to a byte " in {
    CODEWORD_BY_BITS.foreach {
      case (bits, code) => assert(TestHammingCoder.getCodeword(bits) == code)
    }
  }

  it should "encode a byte to 2 bytes in Hamming84 " in {
    CODEWORD_BY_BITS.foreach {
      case (byte, code) => {
        // Hamming encodes in network-order.
        val expected = List(CODEWORD_BY_BITS(0x0.toByte), code)
        val actual = TestHammingCoder.toHamming84(Array(byte)).toList

        assert(actual == expected)
      }
    }
  }

  it should "encode positive and negative byte values correctly " in {
    val bytes = Array(1, 128, 200).map(_.toByte)  // 128 and 200 become -128 and -56.
    val expected = List(0, 1, 8, 0, 12, 8).map(_.toByte).map(CODEWORD_BY_BITS)
    val actual = TestHammingCoder.toHamming84(bytes).toList

    assert(actual == expected)
  }
}
