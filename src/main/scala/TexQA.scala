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
final case class TexQA(q: String, a: List[String], tags: List[String], refs: List[String]) {
  def plain: (String, List[String]) = renderPlain(Parsing.removeTexFmt)
  def renderPlain(rmTex: String => String): (String, List[String]) = 
    q.split(" ").map(rmTex).mkString(" ") -> a.map(rmTex).filter(_.nonEmpty)
}

/**
 * Question-and-answer ancillary functions
 *
 * @author Vince Reuter
 */
object TexQA {
  import io.circe._, io.circe.generic.semiauto._, io.circe.parser._, io.circe.syntax._
  
  /** Simplest constructor; single-answer, no tags/refs */
  def apply(q: String, a: String): TexQA = apply(q, List(a))
  
  /** Simple constructor; no tags/refs */
  def apply(q: String, a: List[String]) = new TexQA(q, a, List(), List())
  
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
    def apply(c: HCursor): Decoder.Result[TexQA] = for {
      q <- c.get[String]("Q")
      a <- c.get[String]("A").fold(_ => c.get[List[String]]("A"), ans => Right(List(ans)))
      tags <- c.get[Option[List[String]]]("tags")
      refs <- c.get[Option[List[String]]]("refs")
      qa = TexQA(q, a, tags getOrElse List.empty[String], refs getOrElse List.empty[String])
    } yield qa
  }
}
