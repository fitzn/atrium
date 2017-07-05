//
// AtriumTypes.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.core

//
// Exceptions
//

case class FailedQualityException(msg: String) extends Exception(msg)
case class ImageTooSmallException(msg: String) extends Exception(msg)
case class ImageWriteException(msg: String) extends Exception(msg)
case class InvalidJPEGFormatException(msg: String) extends Exception(msg)
case class MissingControlByteException(msg: String) extends Exception(msg)
case class UndecodableImageException(msg: String) extends Exception(msg)

//
// Data Types
//

case class RGBRegion(
  topLeftX: Int,
  topLeftY: Int,
  width: Int,
  height: Int,
  pixels: List[Int]
)

case class YCCRegion(
  topLeftX: Int,
  topLeftY: Int,
  width: Int,
  height: Int,
  pixels: List[YCCPixel]
)

case class YCCPixel(
  y: Double,
  cb: Double,
  cr: Double
)

case class RegionedImageRGB(
  width: Int,
  height: Int,
  regions: List[RGBRegion]
) {
  override def toString: String = {
    s"RegionedImage: (${width}x$height) ${regions.size} regions"
  }
}

case class DQTSegment(
  data: Array[Byte]
)

case class JPEGInfo(
  quality: Int,
  quantizationTables: JPEGQuantizationTables
)

case class JPEGQuantizationTables(
  tableLuminance: Vector[Int],
  tableChrominance: Vector[Int],
  table2: Vector[Int],
  table3: Vector[Int]
)
