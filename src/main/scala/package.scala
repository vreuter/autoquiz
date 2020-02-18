/**
 * Self-quizzing and representation of knowledge as question-and-answer.
 *
 * @author Vince Reuter
 */
package object autoquiz {
  
  import cats.instances.char._, cats.instances.string._, cats.syntax.eq._

  def texFmtDirectiveKeyowrds = List("enumerate", "align*", "itemize", "QandA")

  /**
   * Determine whether the given string is just a formatting directive.
   */
  def allTexFmtDir(s: String): Boolean = {
    val dirs = texFmtDirectiveKeyowrds flatMap { kw => List(s"\\begin{$kw}", s"\\end{$kw}") }
    dirs contains rmHeadSpaces(s)
  }

  /**
   * Determine whether the given string appears to start with a {@code Tex} formatting directive.
   *
   * @param s The string to test
   * @return Whether it appears the given string starts with a {@code Tex} formatting directive
   */
  def hasTexFmtDir(s: String): Boolean = 
    s.startsWith("\\begin") || s.startsWith("\\end") || s.startsWith("\\item")

  /**
   * Remove leading spaces.
   *
   * @param s The string from which to remove leading spaces
   * @return Input but with leading spaces removed
   */
  def rmHeadSpaces(s: String): String = s dropWhile { _ === ' ' }

}
