package autoquiz

/** A type that's allowed as an answer in a {@code TexQA} */
private[autoquiz] sealed trait ValidAsAnswer { def toList: List[String] }

/** Simple text answer is a list of strings. */
final case class AnswerListSimple(get: List[String]) extends ValidAsAnswer {
  def toList: List[String] = get
}

/** Some answers have structure and map a keyword/keyphrase to an explanation or example, e.g. */
final case class AnswerMapSimple(get: Map[String, Option[String]]) extends ValidAsAnswer {
  def toList: List[String] = get.toList map { 
    case (k, None) => k
    case (k, v) => s"${k}: ${v}"
  }
}

/** Implicits and helpers for working with allowed answer types */
object ValidAsAnswer {
  import io.circe._, io.circe.syntax._
  implicit val validAnswerCodec: Codec[ValidAsAnswer] = new Codec[ValidAsAnswer] {
    def apply(c: HCursor): Decoder.Result[ValidAsAnswer] = {
      val readStr = implicitly[Decoder[String]]
      val readList = implicitly[Decoder[List[String]]]
      val readMap = implicitly[Decoder[Map[String, Option[String]]]]
      val dec: Decoder[Either[String, Either[List[String], Map[String, Option[String]]]]] = 
        readStr.either(readList.either(readMap))
      dec.apply(c).map(_.fold(
        ans => AnswerListSimple(List(ans)), 
        _.fold(ans => AnswerListSimple(ans), ans => AnswerMapSimple(ans))))
    }
    def apply(a: ValidAsAnswer): Json = a match {
      case AnswerListSimple(answers) => answers.asJson
      case AnswerMapSimple(answers) => answers.asJson
    }
  }
}
