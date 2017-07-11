//
// Atrium.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.app

import com.root81.atrium.core._
import com.root81.atrium.utils._
import java.io.File
import javax.imageio.ImageIO
import scala.util.Try

//
// Application argument types
//

sealed trait AtriumArgs

case class DecodeArgs(
  path: String
) extends AtriumArgs

case class EncodeArgs(
  inPath: String,
  message: String,
  outPath: Option[String] = None,
  quality: Option[Int] = None
) extends AtriumArgs

case class InfoArgs(
  path: String
) extends AtriumArgs

case class ExitArgs(
  code: Int,
  errMsg: Option[String] = None
) extends AtriumArgs

//
// Application
//

object Atrium {
  import AtriumCore._

  private val COMMAND_ENCODE = "encode"
  private val COMMAND_DECODE = "decode"
  private val COMMAND_INFO = "info"

  private val OPTION_PREFIX = "--"
  private val OPT_ENCODE_OUTPUT = OPTION_PREFIX + "out"
  private val OPT_ENCODE_QUALITY = OPTION_PREFIX + "quality"

  private val OPT_HELP = "--help"
  private val OPTS_HELP = Set("help", OPT_HELP)

  val USAGE =
    s"""
      |usage: atrium [$OPT_HELP] <command> [<args>]
      |
      |Commands:
      |\tatrium $COMMAND_ENCODE [--out <filename>] [--quality <int>] <jpg file> <message>
      |\tatrium $COMMAND_DECODE <jpg file>
      |\tatrium $COMMAND_INFO <jpg file>
    """.stripMargin

  def main(args: Array[String]): Unit = {

    try {

      // Validate the args and perform the requested action.
      getAtriumCommandArgs(args) match {
        case args: EncodeArgs => handleEncode(args)
        case args: DecodeArgs => handleDecode(args)
        case args: InfoArgs => handleInfo(args)
        case ExitArgs(code, errMsg) => exit(code, errMsg)
      }

    } catch {
      case e: Exception => {
        exit(1, Some("atrium: " + e.getLocalizedMessage))
      }
    }
  }

  //
  // Commands
  //

  protected def handleEncode(args: EncodeArgs): Unit = {
    AtriumLogger.debug("Atrium: ENCODE")

    val inputImage = ImageLoader.loadJPGImage(args.inPath)
    val inputImageInfo = getJPEGInfoForImagePath(args.inPath)

    val regionedImage = getRegionedImage(inputImage)
    val quality = args.quality.getOrElse(inputImageInfo.quality)

    AtriumLogger.debug(s"Atrium: $regionedImage")
    AtriumLogger.debug(s"Atrium: input message ${args.message.length} characters - '${args.message}'")

    val regionsWithEncodedBytes = encodeMessageIntoRegions(regionedImage.regions, args.message, quality)

    // Write out the encoded regions into a new image.
    val regionedImageWithBytes = regionedImage.copy(regions = regionsWithEncodedBytes)
    val outputImage = ImageConversions.toBufferedImage(regionedImageWithBytes)
    val outputPath = args.outPath.getOrElse(getDefaultOutputPathFromInputPath(args.inPath))

    ImageLoader.writeImageToJPGFile(outputPath, outputImage, quality)

    AtriumLogger.debug(s"Atrium: Encoded image written to: $outputPath")
  }

  protected def handleDecode(args: DecodeArgs): Unit = {
    AtriumLogger.debug("Atrium: DECODE")

    val inputImage = ImageLoader.loadJPGImage(args.path)
    val inputImageInfo = getJPEGInfoForImagePath(args.path)

    val regionedImage = getRegionedImage(inputImage)

    AtriumLogger.debug(s"Atrium: $regionedImage")

    val message = decodeMessageFromRegions(regionedImage.regions, inputImageInfo.quality)

    AtriumLogger.debug(s"Atrium: decoded ${message.length} message characters")

    println(message)
  }

  protected def handleInfo(args: InfoArgs): Unit = {
    AtriumLogger.debug("Atrium: INFO")

    val inputImage = ImageLoader.loadJPGImage(args.path)
    val (width, height) = (inputImage.getWidth, inputImage.getHeight)

    val jpegInfo = getJPEGInfoForImagePath(args.path)

    println(s"Path: ${args.path}")
    println(s"Size: ${width}x$height")
    println(s"Quality: ${jpegInfo.quality}")
    AtriumOut.print(jpegInfo.quantizationTables)
  }

  //
  // Utilities
  //

  protected def getJPEGInfoForImagePath(path: String): JPEGInfo = {
    val inputImageStream = ImageIO.createImageInputStream(new File(path))
    JPEGReader.getJPEGInfo(inputImageStream)
  }

  //
  // Args methods
  //

  private def getAtriumCommandArgs(args: Array[String]): AtriumArgs = {

    args.headOption match {
      case Some(COMMAND_ENCODE) => {
        val encodeArgs = args.drop(1).toList

        val atriumArgs = argOptionParser(encodeArgs) match {
          case Left(exitArgs) => exitArgs
          case Right((remainingArgs, optionMap)) => {
            // Requires an input path and a message.

            if (remainingArgs.size < 2) {
              ExitArgs(1, Some(s"atrium: 'encode' requires a jpg image path and a message."))

            } else {
              val (inputPath, message) = (remainingArgs.head, remainingArgs(1))

              val outputPath = optionMap.get(OPT_ENCODE_OUTPUT)
              val quality = optionMap.get(OPT_ENCODE_QUALITY).flatMap(v => Try(v.toInt).toOption)

              EncodeArgs(inputPath, message, outputPath, quality)
            }
          }
        }

        atriumArgs
      }
      case Some(COMMAND_DECODE) => {
        args.drop(1).toList match {
          case path :: Nil => DecodeArgs(path)
          case _ => ExitArgs(1, Some(s"atrium: 'decode' requires a jpg image path."))
        }
      }
      case Some(COMMAND_INFO) => {
        args.drop(1).toList match {
          case path :: Nil => InfoArgs(path)
          case _ => ExitArgs(1, Some(s"atrium: 'info' requires a jpg image path."))
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

  private def argOptionParser(
    args: List[String],
    options: Map[String, String] = Map.empty[String, String]
  ): Either[ExitArgs, (List[String], Map[String, String])] = {

    if (args.headOption.exists(_.startsWith(OPTION_PREFIX))) {
      val option = args.head
      if (args.tail.isEmpty) {
        return Left(ExitArgs(1, Some(s"atrium: option '$option' requires a value")))
      }

      val optionValue = args.tail.head
      val newOptions = options ++ Map(option -> optionValue)

      return argOptionParser(args.tail.tail, newOptions)
    }

    Right(args, options)
  }

  private def exit(code: Int, message: Option[String] = None): Unit = {
    message.foreach(System.err.println)
    System.exit(code)
  }
}

