
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

val images = ImageLoader.loadImageDirectory("src/test/resources/images/")
AtriumLogger.info(s"Loaded ${images.size} images into 'images'")

