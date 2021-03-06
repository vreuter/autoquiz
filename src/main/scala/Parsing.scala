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
   * @return "cleaned word"
   */
  def removeTexFmt: String => String = {
    import cats.instances.char._, cats.instances.string._, cats.syntax.eq._
    import mouse.boolean._
    import scala.annotation.tailrec
    def rmPfx(s: String): String = {
      @tailrec
      def go(ss: String): String = {
        val trimmed = ss.stripPrefix("\\textbf{")
                        .stripPrefix("\\textit{")
                        .stripPrefix("\\underline{")
                        .stripPrefix("\\item{")
                        .stripPrefix("\\enumerate{")
                        .stripPrefix("\\begin{")
                        .stripPrefix("\\end{")
        if (trimmed === ss) ss else go(trimmed)
      }
      go(rmHeadSpaces(s))
    }
    def rmSfx(s: String): String = s.reverse.dropWhile(_ === '}').reverse.mkString("")
    (s: String) => rmSfx(rmPfx(s))
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
