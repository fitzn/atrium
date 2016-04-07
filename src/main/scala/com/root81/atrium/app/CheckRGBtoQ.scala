package com.root81.atrium.app

import com.root81.atrium.core._
import com.root81.atrium.utils.AtriumOut._
import com.root81.atrium.utils.ImageConversions._
import com.root81.atrium.utils.ImageLoader._

object CheckRGBtoQ {

  private def getTempFile(index: Int): String = s"temp$index.jpg"
  private val QUALITY = 75

  def main(args: Array[String]): Unit = {

    val allImages = loadImageDirectory("src/test/resources/images/").headOption

    allImages.foreach {
      case (name, image) => {
        val regions = toRegionedImage(image, 8, 8).regions
        println(s"$name - ${regions.size} regions")

        // Write each region to a file, then read/write it several times.
        regions.zipWithIndex.foreach {
          case (reg1, index) => {
            val dct1 = DCT.applyRegionDCT(toYCCRegion(reg1))
            print(dct1)
            val qMatrix1 = JPEGQuantization.quantize(dct1.channel0, QUALITY)
            val buf1 = toBufferedImage(RegionedImage(8, 8, List(reg1)))
            writeImageToJPGFile(getTempFile(1), buf1, QUALITY)

            val qMatrix2 = loadQuantAndWriteOneRegionTempImage(1)
            val qMatrix3 = loadQuantAndWriteOneRegionTempImage(2)
            val qMatrix4 = loadQuantAndWriteOneRegionTempImage(3)

            print(qMatrix1)
            print(qMatrix2)
            print(qMatrix3)
            print(qMatrix4)

            // Now compare the quantized matrices.
            val b1 = if (qMatrix1 == qMatrix2) 1 else 0
            val b2 = if (qMatrix2 == qMatrix3) 1 else 0
            val b3 = if (qMatrix3 == qMatrix4) 1 else 0
            println(s"$index: $b1 $b2 $b3")
          }
        }
      }
    }
  }

  private def loadQuantAndWriteOneRegionTempImage(index: Int): QuantizedMatrix = {
    val image = loadJPGImage(getTempFile(index))
    val region = toRegionedImage(image, 8, 8).regions.head
    val dct = DCT.applyRegionDCT(toYCCRegion(region))
    print(dct)
    val qMatrix = JPEGQuantization.quantize(dct.channel0, QUALITY)
    val bufferedImage = toBufferedImage(RegionedImage(8, 8, List(region)))
    writeImageToJPGFile(getTempFile(index + 1), bufferedImage, QUALITY)
    qMatrix
  }

}
