//
// ImageLoader.scala
//
// Copyright (c) 2016 MF Nowlan
//

package com.root81.atrium.utils

import com.root81.atrium.core.ImageWriteException
import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File}
import javax.imageio.stream.ImageOutputStream
import javax.imageio.{IIOImage, ImageIO, ImageWriteParam}

object ImageLoader {

  def loadJPGImage(path: String): BufferedImage = {
    if (!path.toLowerCase.endsWith(".jpg")) {
      throw new IllegalArgumentException(s"ImageLoader: file path must end in .jpg: $path")
    }

    loadImage(path)
  }

  def loadImage(path: String): BufferedImage = {
    val file = new File(path)

    if (!file.exists()) {
      throw new IllegalArgumentException(s"ImageLoader: file path not found: $path")
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

    val outputFile = new File(path)
    if (outputFile.exists() && !outputFile.delete()) {
      throw new ImageWriteException(s"ImageLoader: could not delete pre-existing file $path")
    }

    val outputStream = ImageIO.createImageOutputStream(outputFile)
    writeJPGImage(image, quality, outputStream)
  }

  def writeImageToNewImageJPG(image: BufferedImage, quality: Int): BufferedImage = {
    require(0 <= quality && quality <= 100, "JPG quality must be between 0 and 100")

    // Write the image into the byte array stream.
    val byteArrayOutputStream = new ByteArrayOutputStream()
    val imageOutputStream = ImageIO.createImageOutputStream(byteArrayOutputStream)
    writeJPGImage(image, quality, imageOutputStream)

    // Read the image from the byte array into a new image.
    val bytes = byteArrayOutputStream.toByteArray
    val byteArrayInputStream = new ByteArrayInputStream(bytes)
    ImageIO.read(byteArrayInputStream)
  }

  protected def writeJPGImage(image: BufferedImage, quality: Int, outputStream: ImageOutputStream): Unit = {
    val jpgWriters = ImageIO.getImageWritersByFormatName("jpg")
    if (!jpgWriters.hasNext) {
      throw new RuntimeException("Could not find a JPEG ImageWriter on this system")
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