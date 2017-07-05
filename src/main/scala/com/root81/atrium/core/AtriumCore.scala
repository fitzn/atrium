//
// AtriumCore.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.core

import com.root81.atrium.ecc.HammingCoder
import com.root81.atrium.utils.ImageConversions
import java.awt.image.BufferedImage

object AtriumCore {

  val DEFAULT_ENCODE_FILE_SUFFIX = "-atrium"
  val REGION_DIMENSION = JPEGQuantization.DIMENSION
  val UTF8 = "UTF-8"

  val COLOR_RANGE_NORMALIZE_CONSTANT = ImageConversions.COLOR_RANGE_NORMALIZE_CONSTANT
  val MINIMUM_LUMINANCE_VALUE = 7.85
  val MAXIMUM_LUMINANCE_VALUE = 247.75

  private val hamming = new HammingCoder()

  def encodeMessageIntoRegions(regions: List[RGBRegion], message: String, quality: Int): List[RGBRegion] = {

    val codedBytes = encodeMessage(message)
    var bytesToEmbed = codedBytes.toList

    val numFullRegions = countFullRegions(regions)
    if (numFullRegions < bytesToEmbed.length) {
      // Bytes to embed is one to one with a full region.
      throw ImageTooSmallException(s"Image can carry bytes in $numFullRegions regions but ${bytesToEmbed.length} are needed.")
    }

    regions.map(rgbRegion => {

      if (bytesToEmbed.nonEmpty) {
        // We have a byte to encode into this region.
        val byte = bytesToEmbed.head
        bytesToEmbed = bytesToEmbed.tail
        writeByteIntoRegion(byte, rgbRegion, quality)
      } else {
        // No more bytes to encode, so just return the region.
        rgbRegion
      }
    })
  }

  def encodeMessage(message: String): Array[Byte] = {
    val userData = message.getBytes(UTF8)
    val controlByte = getUnusedControlByte(userData)
    val messageBytes = Array(controlByte) ++ userData ++ Array(controlByte)
    hamming.toHamming84(messageBytes)
  }

  def getRegionedImage(image: BufferedImage): RegionedImageRGB = {
    ImageConversions.toRegionedImage(image, REGION_DIMENSION, REGION_DIMENSION)
  }

  def writeByteIntoRegion(byte: Byte, region: RGBRegion, quality: Int): RGBRegion = {

    // Convert RGB to YCC and normalize the y values (luminance) in the region.
    val yccRegion = ImageConversions.toYCCRegion(region)
    val yccPixels = yccRegion.pixels.toVector

    val luminanceChannel = normalizeLuminanceChannel(yccPixels, yccRegion.width)

    // Calculate the DCT on the luminance channel and embed the byte into the region.
    val luminanceDCT = DCT.applyDCT(luminanceChannel)

    val encodedDCT = AtriumSteganography.encode(byte, luminanceDCT, quality)

    val encodedLuminanceChannel = DCT.applyIDCT(encodedDCT)

    // De-normalize the y values and reconstitute the ycc region with the encoded y values and convert to RGB.
    val denormalizedEncodedChannel = encodedLuminanceChannel.flatMap(_.map(_ + COLOR_RANGE_NORMALIZE_CONSTANT))

    assert(yccPixels.size == denormalizedEncodedChannel.size)
    val encodedRGBPixels = yccPixels.zip(denormalizedEncodedChannel).toList.map {
      case (yccPixel, yValue) => ImageConversions.safeConvertYCCToRGB(yccPixel, yValue)
    }

    region.copy(pixels = encodedRGBPixels)
  }

  def decodeMessageFromRegions(regions: List[RGBRegion], quality: Int): String = {

    // Each original data byte becomes 2 coded bytes after Hamming Coding.
    // Thus, with each region being one coded byte, we take the regions in pairs to decode a single data byte.
    // With 2 control data bytes, we need at least 3 regionPairs for any user data at all.
    val regionPairs = regions.grouped(2).toList
    if (regionPairs.size < 3) {
      throw new UndecodableImageException("message unrecoverable due to lack of encoded bytes")
    }

    // First data byte (i.e., first two coded bytes) is the control byte.
    val controlRegionPair = regionPairs.head
    val controlBytePair = Array(
      extractByteFromRegion(controlRegionPair.head, quality),
      extractByteFromRegion(controlRegionPair.last, quality)
    )

    val controlByte = hamming.fromHamming84(controlBytePair, withCorrection = true).head

    // Stream the remaining pairs so that we only decode regions until the end of the message, as indicated by seeing the control byte.
    val decodedBytes = regionPairs.drop(1).toStream.map(regionPair => {
      val bytePair = Array(extractByteFromRegion(regionPair.head, quality), extractByteFromRegion(regionPair.last, quality))
      hamming.fromHamming84(bytePair, withCorrection = true).head
    }).takeWhile(_ != controlByte).toArray

    new String(decodedBytes, UTF8)
  }

  def extractByteFromRegion(region: RGBRegion, quality: Int): Byte = {

    // Convert RGB to YCC and normalize the y values (luminance) in the region.
    val yccRegion = ImageConversions.toYCCRegion(region)
    val yccPixels = yccRegion.pixels.toVector

    val luminanceChannel = normalizeLuminanceChannel(yccPixels, yccRegion.width)

    // Calculate the DCT on the luminance channel and extract the byte from the region.
    val luminanceDCT = DCT.applyDCT(luminanceChannel)

    AtriumSteganography.decode(luminanceDCT, quality)
  }

  def getDefaultOutputPathFromInputPath(path: String): String = {
    val lastDot = path.lastIndexOf('.')
    val prefix = path.substring(0, lastDot)
    val suffix = path.substring(lastDot)
    prefix + DEFAULT_ENCODE_FILE_SUFFIX + suffix
  }

  //
  // Internal helpers
  //

  protected def getUnusedControlByte(data: Array[Byte]): Byte = {
    // Walk down from 255 to 0 and return the first byte value that does not appear in the application's data.
    val uniqueDataBytes = data.toSet
    (0 to 255).map(_.toByte).reverse.find(!uniqueDataBytes.contains(_)).getOrElse {
      // Note: We could do something like COBS encoding in this case.
      throw new MissingControlByteException("input data uses all 256 byte values, leaving no available control byte")
    }
  }

  protected def countFullRegions(regions: List[RGBRegion]): Int = {
    regions.count(region => region.width == REGION_DIMENSION && region.height == REGION_DIMENSION)
  }

  protected def normalizeLuminanceChannel(yccPixels: Vector[YCCPixel], width: Int): Vector[Vector[Double]] = {
    yccPixels.map(yccPixel => {
      yccPixel.y - COLOR_RANGE_NORMALIZE_CONSTANT
    }).grouped(width).toVector
  }
}
