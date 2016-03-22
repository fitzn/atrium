//
// ImageLoader.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.utils

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.{IIOImage, ImageIO, ImageWriteParam}

object ImageLoader {

  def loadJPGImage(path: String): BufferedImage = {
    if (!path.toLowerCase.endsWith(".jpg")) {
      throw new IllegalArgumentException(s"ImageLoader - file path must end in .jpg: $path")
    }

    loadImage(path)
  }

  def loadImage(path: String): BufferedImage = {
    val file = new File(path)

    if (!file.exists()) {
      throw new IllegalArgumentException(s"ImageLoader - file path not found: $path")
    }

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

  def writeImageToJPGFile(path: String, image: BufferedImage, quality: Int): Unit = {
    require(0 <= quality && quality <= 100, "JPG quality must be between 0 and 100")

    val outputStream =  ImageIO.createImageOutputStream(new File(path))
    val jpgWriters = ImageIO.getImageWritersByFormatName("jpg")

    if (!jpgWriters.hasNext) {
      throw new RuntimeException("Could not find a JPG ImageWriter on this system")
    }

    val writer = jpgWriters.next()
    val writeParams = writer.getDefaultWriteParam
    writeParams.setCompressionMode(ImageWriteParam.MODE_EXPLICIT)
    writeParams.setCompressionQuality(quality / 100f)

    writer.setOutput(outputStream)
    writer.write(null, new IIOImage(image, null, null), writeParams)
    writer.dispose()
  }
}