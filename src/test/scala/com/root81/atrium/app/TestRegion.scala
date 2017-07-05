package com.root81.atrium.app

import com.root81.atrium.core._
import com.root81.atrium.utils.{AtriumLogger, AtriumOut, ImageLoader}

/**
  * Test encoding a specific byte into a specific region in an image.
  * This is used for debugging a problematic byte+region combination, so there is little error handling.
  */
object TestRegion {
  import AtriumCore._

  private val QUALITY = 75

  def main(args: Array[String]): Unit = {

    if (args.length < 2) {
      System.err.println("usage: TestRegion <file path> <region index> [byte]")
      System.exit(1)
    }

    val filePath = args(0)
    val regionIndex = args(1).toInt
    val byte = args.drop(2).headOption.map(_.toInt).getOrElse(-1).toByte

    AtriumLogger.info("Input file: " + filePath)
    AtriumLogger.info("Region Index: " + regionIndex)
    AtriumLogger.info("Byte value: " + byte)
    AtriumLogger.info("Quality: " + QUALITY)

    val inputImage = ImageLoader.loadJPGImage(filePath)
    val regionedImage = getRegionedImage(inputImage)
    val inputRgbRegion = regionedImage.regions(regionIndex)

    AtriumLogger.info("Input Byte: " + AtriumOut.byteStr(byte))

    val outputRgbRegion = AtriumCore.writeByteIntoRegion(byte, inputRgbRegion, QUALITY)
    val outputByte = AtriumCore.extractByteFromRegion(outputRgbRegion, QUALITY)

    AtriumLogger.info("Output Byte: " + AtriumOut.byteStr(outputByte))

    AtriumLogger.info("Input match Output? " + (byte == outputByte))
  }
}
