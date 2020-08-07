package autoquiz

import java.io.File
import cats.data.{ NonEmptyList => NEL }

/**
 * A bundle of simple factual statements with supporting images.
 *
 * @param statements The collection of possibly-backed facts
 * @param otherImageFiles Collection of additional {@code TeX} support, 
 *    (files) not tied to a specific statement in the bundle.
 * @param otherTexFiles Collection of additional image support, 
 *    (files) not tied to a specific statement in the bundle
 */
final case class FactsBundle(
  statements: NEL[MaybeSupportedStatement], 
  otherImageFiles: List[File], otherTexFiles: List[File])
