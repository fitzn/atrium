//
// AtriumOut.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.utils

import com.root81.atrium.core._

object AtriumOut {

  import AtriumLogger._
  import ImageConversions._

  def byteStr(byte: Byte): String = {
    (0 to 7).toList.reverse.map(shift => ((byte >> shift) & 0x1).toString).reduce(_ + _)
  }

  def print(byte: Byte): Unit = {
    println(byteStr(byte))
  }

  def print(region: RGBRegion): Unit = {
    val rows = region.pixels.grouped(region.width).toList
    if (rows.size != region.height) {
      warn(s"Malformed RGBRegion: w=${region.width}, h=${region.height}, pix=${region.pixels.size}")
    }

    rows.foreach(pixelIntRow => {
      // Map each pixel integer to RGB and write the 3-tuple of byte integers with 13 bytes
      val rgbRow = pixelIntRow.map(pixel => List(getRed(pixel), getGreen(pixel), getBlue(pixel)))
      val formattedRow = rgbRow.map(rgb => "(" + rgb.map(i => ("  " + i).takeRight(3)).mkString(",") + ")").mkString(" ")
      println(formattedRow)
    })
  }

  def print(region: YCCRegion): Unit = {
    val rows = region.pixels.grouped(region.width).toList
    if (rows.size != region.height) {
      warn(s"Malformed YCCRegion: w=${region.width}, h=${region.height}, pix=${region.pixels.size}")
    }

    rows.foreach(row => {
      // For now, just print the Y.
      val formattedRow = row.map(pixel => "(%.1f,%.1f,%.1f)".format(pixel.y, pixel.cb, pixel.cr)).mkString(" ")
      println(formattedRow)
    })
  }

  def print(tables: JPEGQuantizationTables): Unit = {
    if (tables.tableLuminance.isEmpty && tables.tableChrominance.isEmpty && tables.table2.isEmpty && tables.table3.isEmpty) {
      println("(empty quantization tables)")
    } else {
      if (tables.tableLuminance.nonEmpty) {
        println("Table Luminance")
        print(tables.tableLuminance.grouped(8).toVector)
      }
      if (tables.tableChrominance.nonEmpty) {
        println("Table Chrominance")
        print(tables.tableChrominance.grouped(8).toVector)
      }
      if (tables.table2.nonEmpty) {
        println("Table2")
        print(tables.table2.grouped(8).toVector)
      }
      if (tables.table3.nonEmpty) {
        println("Table3")
        print(tables.table3.grouped(8).toVector)
      }
    }
  }

  def print(matrix: Vector[Vector[Int]]): Unit = {
    val widest = matrix.flatten.toList.map(_.toString.length).sorted.last
    val prefix = " " * widest
    matrix.foreach(row => {
      val formattedRow = row.map(x => (prefix + x.toString).takeRight(widest + 1)).mkString("")
      println(formattedRow)
    })
  }

  def printD(matrix: Vector[Vector[Double]]): Unit = {
    val formattedNumbers = matrix.flatten.toList.map(d => "%.1f".format(d))
    val widest = formattedNumbers.sortBy(_.length).last.length
    val prefix = " " * widest
    matrix.foreach(row => {
      val formattedRow = row.map(x => (prefix + "%.1f".format(x)).takeRight(widest + 1)).mkString("")
      println(formattedRow)
    })
  }
}
