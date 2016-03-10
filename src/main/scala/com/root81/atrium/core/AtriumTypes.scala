//
// AtriumTypes.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.core

//
// Exceptions
//

case class UnsupportedDimensionsException(width: Int, height: Int, msg: String) extends Exception(msg)

//
// Data Types
//

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
