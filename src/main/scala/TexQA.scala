package autoquiz

/**
 * A simple {@code TeX}-friendly question-and-answer.
 *
 * @param q The question being asked
 * @param a Answer(s) to the question
 * @param tags Topic tags with which this instance is associated
 * @param refs References to things (like URLs) relevant to this instance
 * @author Vince Reuter
 * @return A newly minted instance
 */
final case class TexQA(q: String, a: ValidAsAnswer, tags: List[String], refs: List[String]) {
  import mouse.boolean._
  
  /** Produce a plaintext version of the question and answer(s). */
  def plain: (String, List[String]) = renderPlain(Parsing.removeTexFmt)
  
  /**
   * Product a plaintext version of what may otherwise contain {@code TeX} formatting stuff.
   *
   * @param rmTex How to remove the possible {@code TeX} formatting from question and answer(s)
   * @param rmLeadSpaces Whether leading spaces should be cut out
   */
  def renderPlain(rmTex: String => String, rmLeadSpaces: Boolean = true): (String, List[String]) = {
    // Go word-by-word, removing LaTeX formatting.
    def onWords(f: String => String): String => String = s => {
      val words = s.split(" ")
      rmLeadSpaces.fold(words.dropWhile(_.isEmpty), words).map(f).mkString(" ")
    }
    onWords(rmTex)(q) -> a.toList.map(onWords(rmTex)).filterNot {
      s => s.isEmpty || texFmtDirectiveKeyowrds.contains(s) }
  }
}


/**
 * Question-and-answer ancillary functions
 *
 * @author Vince Reuter
 */
object TexQA {
  import io.circe._, io.circe.syntax._
  
  /** Simplest constructor; single-answer, no tags/refs */
  def apply(q: String, a: String): TexQA = apply(q, List(a))

  /** Simple constructor; no tags/refs */
  def apply(q: String, a: List[String]) = new TexQA(q, AnswerListSimple(a), List(), List())

  /** Encode a {@code TeX}-friendly Q-and-A instance as JSON. */
  implicit val qaEncoder: Encoder[TexQA] = Encoder.instance {
    case TexQA(q, a, tags, refs) => {
      val j = Json.obj("Q" -> q.asJson, "A" -> a.asJson)
      (tags, refs) match {
        case (Nil, Nil) => j
        case (_, Nil) => j deepMerge Json.obj("tags" -> tags.asJson)
        case (Nil, _) => j deepMerge Json.obj("refs" -> refs.asJson)
        case _ => j deepMerge Json.obj("tags" -> tags.asJson, "refs" -> refs.asJson)
      }
    }
  }
  
  /** Parse JSON into a {@code TeX}-friendly Q-and-A instance. */
  implicit val qaDecoder: Decoder[TexQA] = new Decoder[TexQA] {
    import ValidAsAnswer._
    def apply(c: HCursor): Decoder.Result[TexQA] = for {
      q <- c.get[String]("Q")
      a <- c.get[ValidAsAnswer]("A")
      tags <- c.get[Option[List[String]]]("tags")
      refs <- c.get[Option[List[String]]]("refs")
      qa = TexQA(q, a, tags getOrElse List.empty[String], refs getOrElse List.empty[String])
    } yield qa
  }
}
