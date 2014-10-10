package services

import java.awt.image.BufferedImage
import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, IOException }
import javax.imageio.ImageIO

import org.imgscalr.Scalr
import play.api.Logger

/**
 * User: Björn Reimer
 * Date: 10.10.14
 * Time: 11:28
 */
object ImageScaler {

  def scale(input: Array[Byte], size: Int, cutSquare: Boolean): Option[Array[Byte]] = {
    val bais: ByteArrayInputStream = new ByteArrayInputStream(input)
    try {
      val image: BufferedImage = ImageIO.read(bais)
      val scaleMode = if (image.getHeight < image.getWidth) Scalr.Mode.FIT_TO_HEIGHT else Scalr.Mode.FIT_TO_WIDTH

      val targetSize = Math.min(size, Math.min(image.getHeight, image.getWidth))
      val thumb: BufferedImage = Scalr.resize(image, Scalr.Method.QUALITY, scaleMode, targetSize, Scalr.OP_ANTIALIAS)
      val result = if(cutSquare) doCutSquare(thumb) else thumb

      val baos = new ByteArrayOutputStream()
      ImageIO.write(result, "png", baos)
      Some(baos.toByteArray)

    } catch {
      // return a blank image if something goes wrong
      case e: IOException          => None
      case e: NullPointerException => None
    }
  }

  def doCutSquare(image: BufferedImage): BufferedImage = {
    val width = image.getWidth
    val height = image.getHeight

    // horizontal image
    if (width > height) {
      val offset = (width - height) / 2
      Scalr.crop(image, offset, 0, height, height)
    }
    // vertical image
    else if (width < height) {
      val offset = (height - width) / 2
      Scalr.crop(image, 0, offset, width, width)
    }
    // image is already a square
    else {
      image
    }
  }

}
