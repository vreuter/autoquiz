package autoquiz

object Conceptual {
  import cats.data.{ NonEmptyList => NEL }
  import io.circe._, io.circe.parser._, io.circe.syntax._
  import Support.SupportSourceLike
  
  sealed trait ShortTextLike
  
  final case object ShortNull extends ShortTextLike
  final case class ShortText(get: String) extends ShortTextLike
  
  object ShortTextLike {
    def nothing: ShortTextLike = ShortNull
    implicit val shortTextLikeCodec: Codec[ShortTextLike] = new Codec[ShortTextLike] {
      def apply(t: ShortTextLike): Json = t match {
        case ShortNull => Json.Null
        case ShortText(t) => t.asJson
      }
      def apply(c: HCursor): Decoder.Result[ShortTextLike] = c.as[String].map(t => ShortText(t))
    }
  }
  
  sealed trait LongTextLike
  
  final case object LongNull extends LongTextLike
  final case class LongText(get: String) extends LongTextLike
  
  object LongTextLike {
    def nothing: LongTextLike = LongNull
    implicit val longTextLikeCodec: Codec[LongTextLike] = new Codec[LongTextLike] {
      def apply(t: LongTextLike): Json = t match {
        case LongNull => Json.Null
        case LongText(t) => t.asJson
      }
      def apply(c: HCursor): Decoder.Result[LongTextLike] = c.as[String].map(t => LongText(t))
    }
  }

  final case class Concept(
    key: String, name: String, formula: String, statements: NEL[String], 
    shortText: ShortTextLike, longText: LongTextLike, supports: List[SupportSourceLike])

  object Concept {
    def apply(name: String, formula: String, statement: String): Concept = 
      apply(name, name, formula, NEL(statement, List()), ShortTextLike.nothing, LongTextLike.nothing, List())
    def apply(key: String, name: String, formula: String, statement: String): Concept = 
      new Concept(key, name, formula, NEL(statement, List()), ShortTextLike.nothing, LongTextLike.nothing, List())
    def apply(name: String, formula: String, statement: String, support: SupportSourceLike): Concept = 
      apply(name, name, formula, NEL(statement, List()), ShortTextLike.nothing, LongTextLike.nothing, List(support))
    def apply(key: String, name: String, formula: String, statement: String, support: SupportSourceLike): Concept = 
      new Concept(key, name, formula, NEL(statement, List()), ShortTextLike.nothing, LongTextLike.nothing, List(support))
    /*implicit val conceptCodec: Codec[Concept] = new Codec[Concept] {
      def apply(c: Concept): Json = {
        Json("key" -> c.key.asJson, fo)
      }
      def apply(c: HCursor): Decoder.Result[Concept] = ???
    }
    */
  }

}
