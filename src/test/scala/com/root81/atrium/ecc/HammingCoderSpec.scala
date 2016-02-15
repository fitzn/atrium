//
// HammingCoderSpec.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.ecc

import java.util
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class HammingCoderSpec extends FlatSpec {

  object TestHammingCoder extends HammingCoderImpl {
    override def decodeBytePair(b0: Byte, b1: Byte, withCorrection: Boolean): Byte = super.decodeBytePair(b0, b1, withCorrection)
    override def decodeByte(byte: Byte, withCorrection: Boolean): Byte = super.decodeByte(byte, withCorrection)
    override def getHammingDistance(b0: Byte, b1: Byte): Int = super.getHammingDistance(b0, b1)
    override def getBits(byte: Byte): util.BitSet = super.getBits(byte)
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

  it should "return the bitSet for a byte " in {
    val bitSet0 = TestHammingCoder.getBits(0x88.toByte)
    assert(bitSet0.get(7))
    assert(bitSet0.get(3))
    assert(bitSet0.cardinality == 2)

    val bitSet1 = TestHammingCoder.getBits(0xf7.toByte)
    assert(!bitSet1.get(3))
    assert(bitSet1.cardinality == 7)
  }

  it should "measure the hamming distance between bytes " in {
    assert(TestHammingCoder.getHammingDistance(0x80.toByte, 0x81.toByte) == 1)
    assert(TestHammingCoder.getHammingDistance(0x8f.toByte, 0x81.toByte) == 3)
    assert(TestHammingCoder.getHammingDistance(0x77.toByte, 0x88.toByte) == 8)
    assert(TestHammingCoder.getHammingDistance(0xf7.toByte, 0x7f.toByte) == 2)
    assert(TestHammingCoder.getHammingDistance(0xf0.toByte, 0xff.toByte) == 4)
  }

  it should "decode a valid codeword " in {
    CODEWORD_BY_BITS.foreach {
      case (bits, code) => assert(TestHammingCoder.decodeByte(code, withCorrection = false) == bits)
    }
  }

  it should "throw an exception on a one bit error in codeword without correction " in {
    // 0x1 and 0xfe are 1-bit errors on valid codewords.
    for (err <- List(0x1.toByte, 0xfe.toByte)) {
      val thrown = intercept[ByteCorruptionException] {
        TestHammingCoder.decodeByte(err, withCorrection = false)
      }
      assert(thrown.distance == 1)
    }
  }

  it should "decode a one bit error with correction " in {
    // 0x1 and 0xfe are 1-bit errors on valid codewords.
    val err0 = 0x1.toByte
    val err1 = 0xfe.toByte

    assert(TestHammingCoder.decodeByte(err0, withCorrection = true) == 0x0.toByte)
    assert(TestHammingCoder.decodeByte(err1, withCorrection = true) == 0xf.toByte)
  }

  it should "throw an exception on a two bit error in a codeword " in {
    // 0x3 and 0xfc are 2-bit errors on valid codewords.
    for (err <- List(0x3.toByte, 0xfc.toByte)) {
      // First, test without correction.
      val thrown0 = intercept[ByteCorruptionException] {
        TestHammingCoder.decodeByte(err, withCorrection = false)
      }
      assert(thrown0.distance == 2)

      // Then, test with correction and verify that it still fails.
      val thrown1 = intercept[ByteCorruptionException] {
        TestHammingCoder.decodeByte(err, withCorrection = true)
      }
      assert(thrown1.distance == 2)
    }
  }

  it should "decode a byte pair with no errors " in {
    val data0 = TestHammingCoder.decodeBytePair(0x0.toByte, 0xff.toByte, withCorrection = false)
    assert(data0 == 0xf.toByte)

    val data1 = TestHammingCoder.decodeBytePair(0xff.toByte, 0x0.toByte, withCorrection = false)
    assert(data1 == 0xf0.toByte)
  }

  it should "decode a byte pair with one error and correction " in {
    val data0 = TestHammingCoder.decodeBytePair(0x1.toByte, 0xf7.toByte, withCorrection = true)
    assert(data0 == 0xf.toByte)

    val data1 = TestHammingCoder.decodeBytePair(0xfe.toByte, 0x1.toByte, withCorrection = true)
    assert(data1 == 0xf0.toByte)
  }

  it should "throw an exception on two bit errors in byte pair regardless of correction " in {
    val thrown0 = intercept[ByteCorruptionException] {
      TestHammingCoder.decodeBytePair(0x3.toByte, 0xf3.toByte, withCorrection = false)
    }
    assert(thrown0.distance == 2)

    val thrown1 = intercept[ByteCorruptionException] {
      TestHammingCoder.decodeBytePair(0xfc.toByte, 0x7.toByte, withCorrection = true)
    }
    assert(thrown1.distance == 2)
  }

  it should "decode a byte array " in {
    val str = "abc"
    val bytes = str.getBytes("UTF-8")
    val encoded = TestHammingCoder.toHamming84(bytes)
    val decoded = TestHammingCoder.fromHamming84(encoded)
    val output = new String(decoded, "UTF-8")
    assert(output == str)
  }

  it should "throw an exception on a byte array with one bit errors without correction " in {
    val thrown = intercept[ByteCorruptionException] {
      TestHammingCoder.fromHamming84(Array(0x1.toByte, 0xf7.toByte), withCorrection = false)
    }
    assert(thrown.distance == 1)
  }

  it should "decode a byte array with one bit errors " in {
    val data = TestHammingCoder.fromHamming84(Array(0x1.toByte, 0xf7.toByte), withCorrection = true)
    assert(data.head == 0xf.toByte)
  }

  it should "throw an exception on a byte array with 2-bit errors " in {
    val thrown0 = intercept[ByteCorruptionException] {
      TestHammingCoder.fromHamming84(Array(0x3.toByte, 0xf3.toByte), withCorrection = false)
    }
    assert(thrown0.distance == 2)

    val thrown1 = intercept[ByteCorruptionException] {
      TestHammingCoder.fromHamming84(Array(0xfc.toByte, 0x7.toByte), withCorrection = true)
    }
    assert(thrown1.distance == 2)
  }
}
