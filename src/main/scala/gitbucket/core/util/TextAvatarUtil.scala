package gitbucket.core.util

import java.io.ByteArrayOutputStream
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.awt.{Color, Font, RenderingHints}
import java.awt.font.{FontRenderContext, TextLayout}
import java.awt.geom.AffineTransform

object TextAvatarUtil {
  private val iconSize = 200
  private val fontSize = 180
  private val roundSize = 60
  private val shadowSize = 20
  private val bgSaturation = 0.68f
  private val bgBlightness = 0.73f
  private val shadowBlightness = 0.23f
  private val font = new Font(Font.SANS_SERIF, Font.PLAIN, fontSize)
  private val transparent = new Color(0, 0, 0, 0)

  // https://www.w3.org/TR/2008/REC-WCAG20-20081211/#relativeluminancedef
  private def relativeLuminance(c: Color): Double = {
    logger.debug(">>> Entering relativeLuminance(c={})", c)
    val rgb = Seq(c.getRed, c.getGreen, c.getBlue).map { _ / 255.0 }.map { x =>
      logger.debug("relativeLuminance(c={}): rgb → {}", c, rgb)
      if (x <= 0.03928) x / 12.92 else math.pow((x + 0.055) / 1.055, 2.4)
    }
    logger.debug("<<< Exiting relativeLuminance(c={})", c)
    0.2126 * rgb(0) + 0.7152 * rgb(1) + 0.0722 * rgb(2)
  }

  // https://www.w3.org/TR/2008/REC-WCAG20-20081211/#contrast-ratiodef
  private def contrastRatio(c1: Color, c2: Color): Double = {
    logger.debug(">>> Entering contrastRatio(c1={},c2={})", c1, c2)
    val l1 = relativeLuminance(c1)
    val l2 = relativeLuminance(c2)
    logger.debug("contrastRatio(c1={},c2={}): l2 → {}", c1, c2, l2)
    logger.debug("contrastRatio(c1={},c2={}): l1 → {}", c1, c2, l1)
    logger.debug("<<< Exiting contrastRatio(c1={},c2={})", c1, c2)
    if (l1 > l2) (l1 + 0.05) / (l2 + 0.05) else (l2 + 0.05) / (l1 + 0.05)
  }

  private def goodContrastColor(base: Color, c1: Color, c2: Color): Color = {
    logger.debug(">>> Entering goodContrastColor(base={},c1={},c2={})", base, c1, c2)
    logger.debug("<<< Exiting goodContrastColor(base={},c1={},c2={})", base, c1, c2)
    if (contrastRatio(base, c1) > contrastRatio(base, c2)) c1 else c2
  }

  private def strToHue(text: String): Float = {
    logger.debug(">>> Entering strToHue(text={})", text)
    logger.debug("<<< Exiting strToHue(text={})", text)
    Integer.parseInt(StringUtil.md5(text).substring(0, 2), 16) / 256f
  }

  private def getCenterToDraw(drawText: String, font: Font, w: Int, h: Int): (Int, Int) = {
    logger.debug(">>> Entering getCenterToDraw(drawText={},font={},w={},h={})", drawText, font, w, h)
    val context = new FontRenderContext(new AffineTransform(), true, true)
    val txt = new TextLayout(drawText, font, context)

    logger.debug("getCenterToDraw(drawText={},font={},w={},h={}): txt → {}", drawText, font, w, h, txt)
    logger.debug("getCenterToDraw(drawText={},font={},w={},h={}): context → {}", drawText, font, w, h, context)
    val bounds = txt.getBounds

    val x: Int = ((w - bounds.getWidth) / 2 - bounds.getX).toInt
    logger.debug("getCenterToDraw(drawText={},font={},w={},h={}): x → {}", drawText, font, w, h, x)
    logger.debug("getCenterToDraw(drawText={},font={},w={},h={}): y → {}", drawText, font, w, h, y)
    val y: Int = ((h - bounds.getHeight) / 2 - bounds.getY).toInt
    logger.debug("<<< Exiting getCenterToDraw(drawText={},font={},w={},h={})", drawText, font, w, h)
    (x, y)
  }

  private def textImage(drawText: String, bgColor: Color, fgColor: Color): Array[Byte] = {
    logger.debug(">>> Entering textImage(drawText={},bgColor={},fgColor={})", drawText, bgColor, fgColor)
    val canvas = new BufferedImage(iconSize, iconSize, BufferedImage.TYPE_INT_ARGB)
    val g = canvas.createGraphics()
    val center = getCenterToDraw(drawText, font, iconSize, iconSize)

    logger.debug("textImage(drawText={},bgColor={},fgColor={}): center → {}", drawText, bgColor, fgColor, center)
    logger.debug("textImage(drawText={},bgColor={},fgColor={}): canvas → {}", drawText, bgColor, fgColor, canvas)
    logger.debug("textImage(drawText={},bgColor={},fgColor={}): g → {}", drawText, bgColor, fgColor, g)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    g.setColor(transparent)
    g.fillRect(0, 0, iconSize, iconSize)

    g.setColor(bgColor)
    g.fillRoundRect(0, 0, iconSize, iconSize, roundSize, roundSize)

    g.setColor(fgColor)
    g.setFont(font)
    g.drawString(drawText, center._1, center._2)

    g.dispose()

    val stream = new ByteArrayOutputStream
    ImageIO.write(canvas, "png", stream)
    logger.debug("<<< Exiting textImage(drawText={},bgColor={},fgColor={})", drawText, bgColor, fgColor)
    stream.toByteArray
  }

  def textAvatar(nameText: String): Option[Array[Byte]] = {
    private val logger = LoggerFactory.getLogger(getClass)
    val drawText = nameText.substring(0, 1)

    logger.debug(">>> Entering textAvatar(nameText={})", nameText)
    val bgHue = strToHue(nameText)
    val bgColor = Color.getHSBColor(bgHue, bgSaturation, bgBlightness)
    val fgColor = goodContrastColor(bgColor, Color.BLACK, Color.WHITE)

    logger.debug("textAvatar(nameText={}): bgHue → {}", nameText, bgHue)
    logger.debug("textAvatar(nameText={}): bgColor → {}", nameText, bgColor)
    logger.debug("textAvatar(nameText={}): fgColor → {}", nameText, fgColor)
    val font = new Font(Font.SANS_SERIF, Font.PLAIN, fontSize)
    logger.debug("<<< Exiting textAvatar(nameText={})", nameText)
    logger.debug("textAvatar(nameText={}): font → {}", nameText, font)
    if (font.canDisplayUpTo(drawText) == -1) Some(textImage(drawText, bgColor, fgColor)) else None
  }

  private def textGroupImage(drawText: String, bgColor: Color, fgColor: Color, shadowColor: Color): Array[Byte] = {
    logger.debug(">>> Entering textGroupImage(drawText={},bgColor={},fgColor={},shadowColor={})", drawText, bgColor, fgColor, shadowColor)
    val canvas = new BufferedImage(iconSize, iconSize, BufferedImage.TYPE_INT_ARGB)
    val g = canvas.createGraphics()
    val center = getCenterToDraw(drawText, font, iconSize - shadowSize, iconSize - shadowSize)

    logger.debug("textGroupImage(drawText={},bgColor={},fgColor={},shadowColor={}): center → {}", drawText, bgColor, fgColor, shadowColor, center)
    logger.debug("textGroupImage(drawText={},bgColor={},fgColor={},shadowColor={}): canvas → {}", drawText, bgColor, fgColor, shadowColor, canvas)
    logger.debug("textGroupImage(drawText={},bgColor={},fgColor={},shadowColor={}): g → {}", drawText, bgColor, fgColor, shadowColor, g)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    g.setColor(transparent)
    g.fillRect(0, 0, iconSize, iconSize)

    g.setColor(shadowColor)
    g.fillRect(shadowSize, shadowSize, iconSize, iconSize)

    g.setColor(bgColor)
    g.fillRect(0, 0, iconSize - shadowSize, iconSize - shadowSize)

    g.setColor(fgColor)

    g.setFont(font)
    g.drawString(drawText, center._1, center._2)

    g.dispose()

    val stream = new ByteArrayOutputStream
    ImageIO.write(canvas, "png", stream)
    logger.debug("<<< Exiting textGroupImage(drawText={},bgColor={},fgColor={},shadowColor={})", drawText, bgColor, fgColor, shadowColor)
    stream.toByteArray
  }

  def textGroupAvatar(nameText: String): Option[Array[Byte]] = {
    logger.debug(">>> Entering textGroupAvatar(nameText={})", nameText)
    val drawText = nameText.substring(0, 1)

    logger.debug("textGroupAvatar(nameText={}): drawText → {}", nameText, drawText)
    val bgHue = strToHue(nameText)
    val bgColor = Color.getHSBColor(bgHue, bgSaturation, bgBlightness)
    val fgColor = goodContrastColor(bgColor, Color.BLACK, Color.WHITE)
    val shadowColor = Color.getHSBColor(bgHue, bgSaturation, shadowBlightness)

    logger.debug("textGroupAvatar(nameText={}): bgHue → {}", nameText, bgHue)
    logger.debug("textGroupAvatar(nameText={}): bgColor → {}", nameText, bgColor)
    logger.debug("textGroupAvatar(nameText={}): fgColor → {}", nameText, fgColor)
    logger.debug("textGroupAvatar(nameText={}): shadowColor → {}", nameText, shadowColor)
    logger.debug("<<< Exiting textGroupAvatar(nameText={})", nameText)
    if (font.canDisplayUpTo(drawText) == -1) Some(textGroupImage(drawText, bgColor, fgColor, shadowColor)) else None
  }
}
