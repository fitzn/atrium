
// External types
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import org.joda.time.DateTime
import org.joda.time.LocalDate

// Internal types
import com.root81.atrium.ecc._
import com.root81.atrium.core._
import com.root81.atrium.utils._
import com.root81.atrium.utils.ImageConversions._

AtriumLogger.info("Loading Atrium...")

// Object definitions
val hamming = new HammingCoder()

val images = ImageLoader.loadImageDirectory("src/test/resources/test-images/")
val filename = "keyboard.jpg"
val image = images(filename)
AtriumLogger.info(s"Loaded $filename into 'image'")

val regionedImage = toRegionedImage(image, 8, 8)
val rgbRegion = regionedImage.regions.head
val yccRegion = toYCCRegion(rgbRegion)
