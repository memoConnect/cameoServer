package services

import scala.concurrent.{Future, ExecutionContext}
import ExecutionContext.Implicits.global
import org.w3c.dom.{ Document, DOMImplementation }
import org.apache.batik.dom.GenericDOMImplementation
import org.apache.batik.svggen.SVGGraphics2D
import java.awt._
import java.io._
import play.api.{ Play, Logger }
import org.apache.batik.transcoder._
import org.apache.batik.transcoder.image.PNGTranscoder
import org.apache.batik.dom.svg.SVGDOMImplementation
import helper.Utils
import play.api.Play.current
import sun.misc.BASE64Encoder
import models.{ ChunkMeta, FileMeta, FileChunk, Identity }

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

  def generate(identity: Identity): Future[Boolean] = {

    // Get a DOMImplementation.
    val domImpl: DOMImplementation = GenericDOMImplementation.getDOMImplementation

    // Create empty document
    val svgNS = SVGDOMImplementation.SVG_NAMESPACE_URI
    val empty = domImpl.createDocument(svgNS, "svg", null)

    // create avatar description from username
    val desc = createAvatarDesc(identity.cameoId)

    // render avatar into empty document
    val withAvatar = drawAvatar(empty, desc)

    // get png from generated svg
    val png: Array[Byte] = getPng(withAvatar)

    // save to db
    saveAvatar(png, identity)
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

  private def getPng(svg: Document): Array[Byte] = {
    val pngTranscoder = new PNGTranscoder()
    val input = new TranscoderInput(svg)

    val baos = new ByteArrayOutputStream()
    val output = new TranscoderOutput(baos)

    val imageSize: Int = Play.configuration.getInt("avatar.generator.png.size").get
    pngTranscoder.addTranscodingHint(SVGAbstractTranscoder.KEY_HEIGHT, imageSize.toFloat)
    //    pngTranscoder.addTranscodingHint(SVGAbstractTranscoder.KEY_WIDTH, imageSize)
    pngTranscoder.transcode(input, output)

    baos.toByteArray
  }

  private def saveAvatar(png: Array[Byte], identity: Identity): Future[Boolean] = {

    val prefix = "data:image/png;base64,"
    val base64: String = new BASE64Encoder().encode(png).replace("\n", "")

    // Create Chunk and MetaData
    val chunk = FileChunk.create(prefix + base64)
    val chunkMeta = ChunkMeta.createFromChunk(0, chunk)
    val fileMeta = FileMeta.create(Seq(chunkMeta), "avatar.png", 1, chunkMeta.chunkSize, "image/png")

    // write to db and add to identity
    for {
      chunk <- FileChunk.col.insert(chunk)
      meta <- FileMeta.col.insert(fileMeta)
      setAvatar <- identity.setAvatar(fileMeta.id)
    } yield {
      Logger.info("avatar generated for id: " + identity.id)
      chunk.ok && meta.ok && setAvatar
    }
  }
}
