package autoquiz

import java.io.File

/**
 * A factual statement perhaps backed by {@code LaTeX} proof and/or image(s).
 *
 * @param statement The simple statement of fact
 * @param texFiles Paths to files with additional description/clarification 
 *    and/or proof
 * @param imageFiles Paths to files with supporting images
 * @author Vince Reuter
 */
case class MaybeSupportedStatement(
  statement: String, texFiles: List[File], imageFiles: List[File]) {
  final def hasSupport: Boolean = texFiles.nonEmpty || imageFiles.nonEmpty
  final def supported: Boolean = hasSupport

}

/** Helpers for working with backed statements. */
object MaybeSupportedStatement {
  /** Create an unsupported instance. */
  def apply(statement: String): MaybeSupportedStatement =  
    new MaybeSupportedStatement(statement, List(), List())
}
