//
// Atrium.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.app

import com.root81.atrium.core.{AtriumSteganography, DCT, JPEGQuantization}
import com.root81.atrium.utils.{ImageConversions, ImageLoader}

sealed trait AtriumArgs
case class DecodeArgs(path: String, quality: Int) extends AtriumArgs
case class EncodeArgs(inPath: String, quality: Int, message: String, outPath: Option[String] = None) extends AtriumArgs
case class ExitArgs(code: Int, errMsg: Option[String] = None) extends AtriumArgs

object Atrium {
  import AtriumHelper._

  private val COMMAND_ENCODE = "encode"
  private val COMMAND_DECODE = "decode"

  private val OPT_ENCODE_OUTPUT = "--out"

  private val OPT_HELP = "--help"
  private val OPTS_HELP = Set("help", OPT_HELP)

  val USAGE =
    s"""
      |usage: atrium [$OPT_HELP] <command> [<args>]
      |
      |Commands:
      |\tatrium $COMMAND_ENCODE [--out <output file>] <jpeg file> <quality> <message>
      |\tatrium $COMMAND_DECODE <jpeg file> <quality>
    """.stripMargin

  def main(args: Array[String]): Unit = {

    // Validate the args and perform the requested action.
    validateArgs(args) match {
      case args: EncodeArgs => handleEncode(args)
      case args: DecodeArgs => handleDecode(args)
      case ExitArgs(code, errMsg) => exit(code, errMsg)
    }
  }

  //
  // Commands
  //

  protected def handleEncode(args: EncodeArgs): Unit = {

    val codedBytes = wrapUserMessage(args.message)

    val inputFile = getExistingJPGFile(args.inPath)
    val inputImage = ImageLoader.loadImageFile(inputFile)
    val regionedImage = safeGetRegionedImage(inputImage, Some(codedBytes.length))

    var bytestoEmbed = codedBytes.toList
    val codedRGBRegions = regionedImage.regions.map(rgbRegion => {

      if (bytestoEmbed.nonEmpty) {
        // We have a byte to encode into this region.
        val byte = bytestoEmbed.head
        bytestoEmbed = bytestoEmbed.tail

        // Drill down to the quantized matrix and embed the byte.
        val yccRegion = ImageConversions.toYCCRegion(rgbRegion)
        val dctRegion = DCT.applyRegionDCT(yccRegion)
        val quantizedYChannel = JPEGQuantization.quantize(dctRegion.channel0, args.quality)
        val channelWithByte = AtriumSteganography.encode(byte, quantizedYChannel)

        // Reverse the process back to an RGBRegion.
        val yMatrixWithByte = JPEGQuantization.unquantize(channelWithByte)
        val dctRegionWithByte = dctRegion.copy(channel0 = yMatrixWithByte)
        val yccRegionWithByte = DCT.unapplyRegionDCT(dctRegionWithByte)
        ImageConversions.toRGBRegion(yccRegionWithByte)

      } else {
        // No more bytes to encode, so just return the region.
        rgbRegion
      }
    })

    // Write out the encoded regions into a new image.
    val regionedImageWithBytes = regionedImage.copy(regions = codedRGBRegions)
    val outputImage = ImageConversions.toBufferedImage(regionedImageWithBytes)
    val outputPath = args.outPath.getOrElse(makeAtriumOutputFilename(args.inPath))

    ImageLoader.writeImageToJPGFile(outputPath, outputImage, args.quality)
  }

  protected def handleDecode(args: DecodeArgs): Unit = {

    val inputFile = getExistingJPGFile(args.path)
    val inputImage = ImageLoader.loadImageFile(inputFile)
    val regionedImage = safeGetRegionedImage(inputImage)

    val message = decodeUserMessageFromRegions(regionedImage.regions, args.quality)
    println(message)
  }

  //
  // Arguments parser
  //

  private def validateArgs(args: Array[String]): AtriumArgs = {

    args.headOption match {
      case Some(COMMAND_ENCODE) => {
        args.drop(1).toList match {
          case OPT_ENCODE_OUTPUT :: outputPath :: inputPath :: quality :: message :: Nil => EncodeArgs(inputPath, quality.toInt, message, Some(outputPath))
          case inputPath :: quality :: phrase :: Nil => EncodeArgs(inputPath, quality.toInt, phrase)
          case _ => ExitArgs(1, Some(s"atrium: 'encode' requires a jpg image path, a quality on [0, 100], and a message."))
        }
      }
      case Some(COMMAND_DECODE) => {
        args.drop(1).toList match {
          case path :: quality :: Nil => DecodeArgs(path, quality.toInt)
          case _ => ExitArgs(1, Some(s"atrium: 'decode' requires a jpg image path and a quality on [0, 100]."))
        }
      }
      case Some(x) if !OPTS_HELP.contains(x) => {
        ExitArgs(1, Some(s"atrium: '$x' is not an atrium command.  See 'atrium $OPT_HELP'."))
      }
      case _ => {
        println(USAGE)
        ExitArgs(0)
      }
    }
  }
}

