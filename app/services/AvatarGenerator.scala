package services

import org.w3c.dom.{ Element, Document, DOMImplementation }
import org.apache.batik.dom.GenericDOMImplementation
import org.apache.batik.svggen.SVGGraphics2D
import java.awt._
import java.io._
import play.api.{ Play, Logger }
import org.apache.batik.transcoder.{ TranscoderException, TranscoderOutput, TranscoderInput }
import org.apache.batik.transcoder.image.PNGTranscoder
import javax.swing.border.StrokeBorder
import org.apache.batik.dom.svg.SVGDOMImplementation
import java.security.MessageDigest
import helper.Utils
import play.api.Play.current
import magick.{ImageInfo, MagickImage}
import org.apache.batik.dom.util.DOMUtilities

/**
 * User: Björn Reimer
 * Date: 05.05.14
 * Time: 11:52
 */
object AvatarGenerator {

  case class AvatarDescription(raster: Seq[Seq[Boolean]], color: Color)

  val colors: Seq[Color] =
    Seq(
      "#02BED2",
      "#FF6600",
      "#47EBFC",
      "#F9CB13",
      "#03A6B8",
      "#CC0000",
      "#66CC00",
      "#026A75",
      "#D5AA00",
      "#8F0101",
      "#E06618",
      "#8F7301",
      "#660000",
      "#B05213",
      "#486718",
      "#090055"
    ).map { str =>
        val r = Integer.parseInt(str.substring(1, 3), 16)
        val b = Integer.parseInt(str.substring(3, 5), 16)
        val g = Integer.parseInt(str.substring(5, 7), 16)
        new Color(r, b, g)
      }

  def generate() {

    // Get a DOMImplementation.
    val domImpl: DOMImplementation = GenericDOMImplementation.getDOMImplementation

    // Create empty document
    val svgNS = SVGDOMImplementation.SVG_NAMESPACE_URI
    val empty = domImpl.createDocument(svgNS, "svg", null)

    // create avatar description from username
    val desc = createAvatarDesc("huuuphuup2")

    // render avatar into empty document
    val withAvatar = drawAvatar(empty, desc)

    // write to file
    val png = saveToPng(withAvatar)

  }

  private def createAvatarDesc(seed: String): AvatarDescription = {

    // seed random generator with seed, so we will always get the same avatar for each seed
    val random = new scala.util.Random
    random.setSeed(Utils.md5Long(seed))

    // color
    val color = colors(random.nextInt(colors.length))

    // generate first half of columns
    val size: Int = Play.configuration.getInt("avatar.generator.raster").get
    val halfSize = Math.round(size.toFloat / 2f)
    val half = Seq.fill(halfSize)(Seq.fill(size)(random.nextBoolean()))

    // fill second half by mirroring the first
    def appendOuterColumns[A](columns: Seq[A]): Seq[A] = {
      columns.size match {
        case 0 => Seq()
        case 1 => columns
        case _ => columns.head +: appendOuterColumns(columns.tail) :+ columns.head
      }
    }
    val all = appendOuterColumns(half)
    new AvatarDescription(all, color)
  }

  private def drawAvatar(document: Document, desc: AvatarDescription): Document = {

    // Create an instance of the SVG Generator.
    val svgGenerator: SVGGraphics2D = new SVGGraphics2D(document)

    // Set dimension
    val rasterNum = desc.raster.size
    val rasterSize = 100
    val dimension = new Dimension(rasterNum * rasterSize, rasterNum * rasterSize)
    svgGenerator.setSVGCanvasSize(dimension)

    // fill background
    svgGenerator.setPaint(Color.white)
    svgGenerator.fill(new Rectangle(dimension))

    // set color
    svgGenerator.setPaint(desc.color)

    // draw boxes
    desc.raster.zipWithIndex.seq.foreach {
      case (column, i) =>
        column.zipWithIndex.seq.foreach {
          case (false, j) =>
          case (true, j) =>
            val rect = new Rectangle(i * rasterSize, j * rasterSize, rasterSize, rasterSize)
            svgGenerator.fill(rect)
        }
    }

    // Populate the document with the generated SVG content.
    val root = document.getDocumentElement
    svgGenerator.getRoot(root)

    document
  }


  private def saveToPng(svg: Document): File = {
    val pngFile = File.createTempFile("fromSVG", ".png")
    try {
      val pngTranscoder = new PNGTranscoder()
      val input = new TranscoderInput(svg)
      val ostream = new FileOutputStream(pngFile)
      val output = new TranscoderOutput(ostream)
      pngTranscoder.transcode(input, output)
    } catch {
      case e: TranscoderException => Logger.error("Error transcoding svg to png", e)
    }
    pngFile
  }

}
