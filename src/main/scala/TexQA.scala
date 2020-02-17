package autoquiz

final case class TexQA(q: String, a: List[String], tags: List[String], refs: List[String]) {
  def renderTex(offset: Int)(implicit render: RenderQA): String = {
    require(offset >= 0, s"Negative line space offset: $offset")
    List(render ask q, render, render.headA) ::: (a :+ render.footA) mkString "\n"
  }
}

object TexQA {
  import io.circe._, io.circe.generic.semiauto._, io.circe.parser._, io.circe.syntax._
  def apply(q: String, a: String): TexQA = apply(q, List(a))
  def apply(q: String, a: List[String]) = new TexQA(q, a, List(), List())
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
