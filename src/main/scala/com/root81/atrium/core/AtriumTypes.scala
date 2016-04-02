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
case class InvalidJPEGFormatException(msg: String) extends Exception(msg)
case class UnsupportedDimensionsException(width: Int, height: Int, msg: String) extends Exception(msg)

//
// Data Types
//

case class QuantizedMatrix(
  quality: Int,
  coefficients: Vector[Vector[Int]]
)

case class DCTRegion(
  width: Int,
  height: Int,
  channel0: Vector[Vector[Double]],
  channel1: Vector[Vector[Double]],
  channel2: Vector[Vector[Double]]
)

case class RGBRegion(
  width: Int,
  height: Int,
  pixels: List[Int]
)

case class YCCRegion(
  width: Int,
  height: Int,
  pixels: List[YCCPixel]
)

case class YCCPixel(
  y: Double,
  cb: Double,
  cr: Double
)

case class RegionedImage(
  width: Int,
  height: Int,
  regions: List[RGBRegion]
)

case class DQTSegment(
  data: Array[Byte]
)

case class JPEGQuantizationTables(
  tableLuminance: Vector[Int],
  tableChrominance: Vector[Int],
  table2: Vector[Int],
  table3: Vector[Int]
)
