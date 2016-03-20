//
// AtriumHelpers.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.app

import com.root81.atrium.core._
import com.root81.atrium.ecc.HammingCoder
import com.root81.atrium.utils.ImageConversions
import java.awt.image.BufferedImage
import java.io.File

object AtriumHelper {

  val DEFAULT_ENCODE_FILE_SUFFIX = "-atrium"
  val REGION_DIMENSION = 8
  val UTF8 = "UTF-8"

  private val hamming = new HammingCoder()

  def getExistingJPGFile(path: String): File = {

    if (!path.toLowerCase.endsWith(".jpg")) {
      exit(1, Some("atrium: Only .jpg files are supported"))
    }

    val file = new File(path)

    if (!file.exists()) {
      exit(1, Some(s"atrium: '${file.getName}' does not exist"))
    }

    file
  }

  def safeGetRegionedImage(image: BufferedImage, minRegionsNeeded: Option[Int] = None): RegionedImage = {

    val tryRegionedImage = try {
      Some(ImageConversions.toRegionedImage(image, REGION_DIMENSION, REGION_DIMENSION))
    } catch {
      case uoe: UnsupportedOperationException => {
        exit(3, Some(s"atrium: error breaking image into ${REGION_DIMENSION}x$REGION_DIMENSION pixel regions - " + uoe.getMessage))
        None
      }
    }

    // If we're here, then the option is defined.
    val regionedImage = tryRegionedImage.get

    if (minRegionsNeeded.exists(regionedImage.regions.size < _)) {
      exit(3, Some(s"atrium: image is too small; it has ${regionedImage.regions.size} regions, but the message requires ${minRegionsNeeded.get}."))
    }

    regionedImage
  }

  def wrapUserMessage(message: String): Array[Byte] = {
    val userData = message.getBytes(UTF8)
    val controlByte = getUnusedControlByte(userData)
    val messageBytes = Array(controlByte) ++ userData ++ Array(controlByte)
    hamming.toHamming84(messageBytes)
  }

  def decodeUserMessageFromRegions(regions: List[RGBRegion], quality: Int): String = {

    // Each original data byte becomes 2 coded bytes after Hamming Coding.
    // Thus, with each region being one coded byte, we take the regions in pairs to decode a single data byte.
    val regionPairs = regions.grouped(2).toList

    // With 2 control data bytes, we need at least 3 regionPairs for any user data at all.
    if (regionPairs.size < 3) {
      exit(3, Some(s"atrium: message unrecoverable due to lack of encoded bytes"))
    }

    // First data byte is the control byte.
    val controlRegionPair = regionPairs.head
    val controlBytePair = Array(
      getByteFromRegion(controlRegionPair.head, quality),
      getByteFromRegion(controlRegionPair.last, quality)
    )

    val tryControlByte = try {
      Some(hamming.fromHamming84(controlBytePair, withCorrection = true))
    } catch {
      case e: Exception => {
        exit(3, Some("atrium: decoding failed due to corrupted image - " + e.getMessage))
        None
      }
    }

    // If we're here, then we have a single byte in the Array.
    val controlByte = tryControlByte.get.head

    // Stream the remaining pairs so that we only decode regions until the end of the message, as indicated by seeing the control byte.
    val decodedBytes = regionPairs.drop(1).toStream.map(regionPair => {
      Array(getByteFromRegion(regionPair.head, quality), getByteFromRegion(regionPair.last, quality))
    }).map(bytePair => {
      val tryByte = try {
        Some(hamming.fromHamming84(bytePair, withCorrection = true))
      } catch {
        case e: Exception => {
          exit(3, Some("atrium: decoding failed due to corrupted image - " + e.getMessage))
          None
        }
      }
      tryByte.get.head
    }).takeWhile(_ != controlByte).toArray

    new String(decodedBytes, UTF8)
  }

  def getByteFromRegion(region: RGBRegion, quality: Int): Byte = {
    val yccRegion = ImageConversions.toYCCRegion(region)
    val dctRegion = DCT.applyRegionDCT(yccRegion)
    val quantizedYChannel = JPEGQuantization.quantize(dctRegion.channel0, quality)
    AtriumSteganography.decode(quantizedYChannel)
  }

  def getUnusedControlByte(data: Array[Byte]): Byte = {
    // Walk down from 255 to 0 and return the first byte value that does not appear in the application's data.
    val uniqueDataBytes = data.toSet
    val availableValue = (0 to 255).reverse.find(x => !uniqueDataBytes.contains(x.toByte))

    if (availableValue.isEmpty) {
      // Note: We could do something like COBS encoding in this case.
      exit(2, Some("atrium: input data cannot use all 256 byte values since 1 byte is needed as the control byte"))
    }

    availableValue.get.toByte
  }

  def makeAtriumOutputFilename(path: String): String = {
    val lastDot = path.lastIndexOf('.')
    val prefix = path.substring(0, lastDot)
    val suffix = path.substring(lastDot)
    prefix + DEFAULT_ENCODE_FILE_SUFFIX + suffix
  }

  def exit(code: Int, message: Option[String]): Unit = {
    message.foreach(System.err.println)
    System.exit(code)
  }
}
