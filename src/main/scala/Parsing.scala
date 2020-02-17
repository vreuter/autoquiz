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
   * From a collection of (possibly, anyway) {@code TeX}-formatted words, strip away the formatting directives.
   *
   * @param words collection of words from which {@code TeX} formatting directives are to be removed
   * @return collection of "cleaned" words
   */
  def parseCleanWords(words: List[String]): List[String] = {
    import cats.instances.string._, cats.syntax.eq._
    import mouse.boolean._
    def rmPfx(s: String): String = {
      val trimmed = s.stripPrefix("\\textbf{").stripPrefix("\\textit{")
      (trimmed === s).fold(trimmed, rmPfx(trimmed))
    }
    def rmSfx(s: String): String = s.split("\\}").toList match {
      case res :: Nil => res
      case _ => throw new Exception(s"Suffix removal failed: $s")
    }
    words map { (rmPfx _) andThen { (s: String) => s.startsWith("\\text").fold(s, rmSfx(s)) } }
  }

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
