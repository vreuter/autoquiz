package autoquiz

/**
 * Q-and-A JSON parsing-related functionality.
 *
 * @author Vince Reuter
 */
object Parsing {
  import scala.io.Source
  import java.io.File
  import io.circe._, io.circe.parser.decode
  /**
   * Read the given file into a collection of questions and answers.
   *
   * @param f Path to the JSON file to parse
   * @return Either a {@code Left} wrapping a parsing error, or a {@code Right} 
   *         wrapping a collection of Q-and-A instances
   */
  def readFile(f: File): Either[Error, List[TexQA]] = 
    decode[List[TexQA]](Source.fromFile(f).mkString)
}
