package autoquiz

import java.io.File

sealed trait TeXt[A]

sealed trait TexFmt extends TeXt[String] {
  def raw: String
  def tex: String
}

case class Image(get: File) extends TeXt[File]

case class Bold(raw: String) extends TexFmt {
  def tex: String = s"\\textbf{$raw}"
}

case class Ital(raw: String) extends TexFmt {
  def tex: String = s"\\textit{$raw}"
}

case class Uline(raw: String) extends TexFmt {
  def tex: String = s"\\underline{$raw}"
}

/**
 * Implicits and ancillary functionality for working with {@code TeXt} types.
 *
 * @author Vince Reuter
 */
object TeXt {
  import cats.Show
  type Phrase = List[TexFmt]
  implicit val showPhrase: Show[Phrase] = 
    Show.show((p: Phrase) => p.map(_.tex) mkString " ")
}
