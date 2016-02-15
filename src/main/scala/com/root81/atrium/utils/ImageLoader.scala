//
// ImageLoader.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.utils

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object ImageLoader {

  def loadImage(path: String): BufferedImage = {
    val file = new File(path)
    loadImageFile(file)
  }

  def loadImageFile(file: File): BufferedImage = {
    ImageIO.read(file)
  }

  def loadImageDirectory(path: String): Map[String, BufferedImage] = {
    val directory = new File(path)
    (for (file <- directory.listFiles()) yield {
      val filename = file.getName
      (filename, loadImageFile(file))
    }).toMap
  }
}