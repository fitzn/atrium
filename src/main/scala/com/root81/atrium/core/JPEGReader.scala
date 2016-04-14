//
// JPEGReader.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.core

import com.root81.atrium.utils.AtriumLogger
import java.io.{ByteArrayInputStream, DataInputStream}
import javax.imageio.stream.ImageInputStream
import scala.annotation.tailrec
import scala.util.Try

//
// This object and class is adapted from ImageMagick's coders/jpeg.c file.
//
object JPEGReader {

  val MARKER_CONSTANT = 0xFF
  val MARKER_SOI = 0xFFD8   // Start of image
  val MARKER_EOI = 0xFFD9   // End of image
  val MARKER_SOS = 0xFFDA   // Scan segment
  val MARKER_DQT = 0xFFDB   // Discrete quantization table
  val TERMINATION_MARKERS = Set(MARKER_EOI, MARKER_SOS)   // These markers indicate no further DQTs are to be read.

  val NUM_DCT_QUANTIZERS = 64

  private val HASH_FOR_LUMINANCE_AND_CHROMINANCE = Vector(
    1020, 1015,  932,  848,  780,  735,  702,  679,  660,  645,
     632,  623,  613,  607,  600,  594,  589,  585,  581,  571,
     555,  542,  529,  514,  494,  474,  457,  439,  424,  410,
     397,  386,  373,  364,  351,  341,  334,  324,  317,  309,
     299,  294,  287,  279,  274,  267,  262,  257,  251,  247,
     243,  237,  232,  227,  222,  217,  213,  207,  202,  198,
     192,  188,  183,  177,  173,  168,  163,  157,  153,  148,
     143,  139,  132,  128,  125,  119,  115,  108,  104,   99,
      94,   90,   84,   79,   74,   70,   64,   59,   55,   49,
      45,   40,   34,   30,   25,   20,   15,   11,    6,    4,
       0
  )

  private val HASH_FOR_LUMINANCE_ONLY = Vector(
    510,  505,  422,  380,  355,  338,  326,  318,  311,  305,
    300,  297,  293,  291,  288,  286,  284,  283,  281,  280,
    279,  278,  277,  273,  262,  251,  243,  233,  225,  218,
    211,  205,  198,  193,  186,  181,  177,  172,  168,  164,
    158,  156,  152,  148,  145,  142,  139,  136,  133,  131,
    129,  126,  123,  120,  118,  115,  113,  110,  107,  105,
    102,  100,   97,   94,   92,   89,   87,   83,   81,   79,
    76,   74,   70,   68,   66,   63,   61,   57,   55,   52,
    50,   48,   44,   42,   39,   37,   34,   31,   29,   26,
    24,   21,   18,   16,   13,   11,    8,    6,    3,    2,
    0
  )

  private val SUMS_FOR_LUMINANCE_AND_CHROMINANCE = Vector(
    32640, 32635, 32266, 31495, 30665, 29804, 29146, 28599, 28104,
    27670, 27225, 26725, 26210, 25716, 25240, 24789, 24373, 23946,
    23572, 22846, 21801, 20842, 19949, 19121, 18386, 17651, 16998,
    16349, 15800, 15247, 14783, 14321, 13859, 13535, 13081, 12702,
    12423, 12056, 11779, 11513, 11135, 10955, 10676, 10392, 10208,
     9928,  9747,  9564,  9369,  9193,  9017,  8822,  8639,  8458,
     8270,  8084,  7896,  7710,  7527,  7347,  7156,  6977,  6788,
     6607,  6422,  6236,  6054,  5867,  5684,  5495,  5305,  5128,
     4945,  4751,  4638,  4442,  4248,  4065,  3888,  3698,  3509,
     3326,  3139,  2957,  2775,  2586,  2405,  2216,  2037,  1846,
     1666,  1483,  1297,  1109,   927,   735,   554,   375,   201,
      128,     0
  )

  private val SUMS_FOR_LUMINANCE_ONLY = Vector(
    16320, 16315, 15946, 15277, 14655, 14073, 13623, 13230, 12859,
    12560, 12240, 11861, 11456, 11081, 10714, 10360, 10027,  9679,
     9368,  9056,  8680,  8331,  7995,  7668,  7376,  7084,  6823,
     6562,  6345,  6125,  5939,  5756,  5571,  5421,  5240,  5086,
     4976,  4829,  4719,  4616,  4463,  4393,  4280,  4166,  4092,
     3980,  3909,  3835,  3755,  3688,  3621,  3541,  3467,  3396,
     3323,  3247,  3170,  3096,  3021,  2952,  2874,  2804,  2727,
     2657,  2583,  2509,  2437,  2362,  2290,  2211,  2136,  2068,
     1996,  1915,  1858,  1773,  1692,  1620,  1552,  1477,  1398,
     1326,  1251,  1179,  1109,  1031,   961,   884,   814,   736,
      667,   592,   518,   441,   369,   292,   221,   151,    86,
       64,     0
  )

  def getJPEGInfo(inputImageStream: ImageInputStream): JPEGInfo = {
    val dqtSegments = JPEGReader.readDQTSegmentsFromJPEG(inputImageStream)
    val quantizationTables = JPEGReader.getQuantizationTables(dqtSegments)
    val quality = JPEGReader.getJPEGQuality(quantizationTables)
    JPEGInfo(quality, quantizationTables)
  }

  def getJPEGQuality(tables: JPEGQuantizationTables): Int = {
    require(tables.tableLuminance.nonEmpty, s"JPEGReader: cannot read JPEGInformation; tables has no 'tableLuminance' value (${tables.toString})")

    val quantizersSum = tables.tableLuminance.sum + tables.tableChrominance.sum + tables.table2.sum + tables.table3.sum

    AtriumLogger.debug(s"JPEGReader: image quantizers sum to $quantizersSum")

    // We've already checked that "tableLuminance" is non-empty.
    val (hash, sums, qualityKeySum) = if (tables.tableChrominance.nonEmpty) {
      val qualityValue = tables.tableLuminance(2) + tables.tableLuminance(53) + tables.tableChrominance(0) + tables.tableChrominance(NUM_DCT_QUANTIZERS - 1)
      (HASH_FOR_LUMINANCE_AND_CHROMINANCE, SUMS_FOR_LUMINANCE_AND_CHROMINANCE, qualityValue)
    } else {
      val qualityValue = tables.tableLuminance(2) + tables.tableLuminance(53)
      (HASH_FOR_LUMINANCE_ONLY, SUMS_FOR_LUMINANCE_ONLY, qualityValue)
    }

    val qualityIndices = (0 until 100).toList
    val selectedQualityIndex = qualityIndices.find(qIdx => {
      (qualityKeySum >= hash(qIdx) || quantizersSum >= sums(qIdx)) &&
      ((qualityKeySum <= hash(qIdx) && quantizersSum <= sums(qIdx)) || qIdx >= 50)
    }).getOrElse {
      throw new FailedQualityException(s"JPEGReader: cannot calculate quality for image, quantizersSum=$quantizersSum, qualityKeySum=$qualityKeySum")
    }

    val quality = selectedQualityIndex + 1

    val matchType = if (qualityKeySum <= hash(selectedQualityIndex) && quantizersSum <= sums(selectedQualityIndex)) "exact" else "approximate"
    AtriumLogger.debug(s"JPEGReader: image quality calculated at $quality, match is $matchType")

    quality
  }

  def getQuantizationTables(segments: List[DQTSegment]): JPEGQuantizationTables = {
    require(segments.nonEmpty, "JPEGReader: cannot create JPEGQuantTables without any segments")
    require(segments.size <= 4, s"JPEGReader: too many DQTSegments ${segments.size} to create JPEGQuantTables")

    AtriumLogger.debug(s"JPEGReader: producing quantization tables for ${segments.size} segments")

    val tablesByIndex = segments.flatMap(extractQuantizationIndicesAndTables).toMap

    AtriumLogger.debug(s"JPEGReader: quantization table indices: ${tablesByIndex.keys.toList.sorted.mkString("[", ", ", "]")}")

    JPEGQuantizationTables(
      tableLuminance = tablesByIndex.getOrElse(0, Vector.empty[Int]),
      tableChrominance = tablesByIndex.getOrElse(1, Vector.empty[Int]),
      table2 = tablesByIndex.getOrElse(2, Vector.empty[Int]),
      table3 = tablesByIndex.getOrElse(3, Vector.empty[Int])
    )
  }

  def readDQTSegmentsFromJPEG(imageStream: ImageInputStream): List[DQTSegment] = {
    // Ensure the image stream is a JPEG formatted image stream and at the correct image stream start position.
    var marker = imageStream.readUnsignedShort()
    if (marker != MARKER_SOI) {
      throw new InvalidJPEGFormatException(s"JPEGReader: image stream has invalid marker byte, '$marker', should be '$MARKER_SOI'")
    }

    val segments = List.empty[DQTSegment].toBuffer
    do {
      marker = getSegmentMarker(imageStream)
      val segmentLength = imageStream.readUnsignedShort()   // Includes the 2 bytes for the length field itself.
      val dataLength = segmentLength - 2

      if (marker == MARKER_DQT) {
        AtriumLogger.debug(s"JPEGReader: DQT segment length $segmentLength bytes")

        // Read the DQT data.
        val segmentData = for (_ <- 0 until dataLength) yield imageStream.readByte()
        segments.append(DQTSegment(segmentData.toArray))

      } else {
        // Skip data for all other segments.
        imageStream.skipBytes(dataLength)
      }
    } while (!TERMINATION_MARKERS.contains(marker))

    segments.toList
  }

  //
  // Internal helpers
  //

  protected def extractQuantizationIndicesAndTables(segment: DQTSegment): List[(Int, Vector[Int])] = {
    if (segment.data.length < NUM_DCT_QUANTIZERS + 1) {
      throw new InvalidJPEGFormatException(s"JPEGReader: segment data must be at least $NUM_DCT_QUANTIZERS bytes")
    }

    val dataInputStream = new DataInputStream(new ByteArrayInputStream(segment.data))

    // More than one table can be stored in a single DQT segment, so loop until the segment's data is exhausted.
    var oControl: Option[Int] = None
    val indicesAndTables = List.empty[(Int, Vector[Int])].toBuffer

    do {
      oControl = Try(dataInputStream.readUnsignedByte()).toOption   // This throws EOFException at the end of the data, hence the Try.

      oControl.foreach(control => {
        val tableIndex = control & 0xf
        val bitMode = (control >> 4) & 0xf

        if (tableIndex < 0 || tableIndex > 3) {
          throw new InvalidJPEGFormatException(s"JPEGReader: invalid tableIndex value '$tableIndex' must be between 0 and 3")
        }
        if (bitMode < 0 || bitMode > 1) {
          throw new InvalidJPEGFormatException(s"JPEGReader: invalid bitMode value '$bitMode' must be between 0 and 1")
        }

        AtriumLogger.debug(s"JPEGReader: segment control byte, tableIndex=$tableIndex, bitMode=$bitMode")

        val table = (for (_ <- 0 until NUM_DCT_QUANTIZERS) yield {
          if (bitMode == 0) {
            dataInputStream.readUnsignedByte()
          } else {
            dataInputStream.readUnsignedShort()
          }
        }).toVector

        indicesAndTables.append((tableIndex, table))
      })
    } while (oControl.isDefined)

    indicesAndTables.toList
  }

  protected def getSegmentMarker(stream: ImageInputStream): Int = {
    val highByte = readUntil(stream, _ == MARKER_CONSTANT)
    val lowByte = readUntil(stream, _ != MARKER_CONSTANT)
    highByte << 8 | lowByte
  }

  @tailrec private def readUntil(stream: ImageInputStream, stopFunc: (Int) => Boolean): Int = {
    val value = stream.readUnsignedByte()
    if (stopFunc(value)) {
      value
    } else {
      readUntil(stream, stopFunc)
    }
  }
}
