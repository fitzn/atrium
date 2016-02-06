
// External types
import org.joda.time.DateTime
import org.joda.time.LocalDate

// Internal types
import com.root81.atrium.ecc._

// Object definitions
val hamming = {
  class GetHammingCoder extends HammingCoder {
    def getIt = hamming
  }
  (new GetHammingCoder).getIt
}

