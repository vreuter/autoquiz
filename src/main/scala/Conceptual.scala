package autoquiz

object Conceptual {
  import cats.data.{ NonEmptyList => NEL }, cats.syntax.list._
  import io.circe._, io.circe.parser._, io.circe.syntax._
  import Contextual.StatementContext, Support.SupportSourceLike
  
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
      def apply(c: HCursor): Decoder.Result[ShortTextLike] = {
        import mouse.boolean._
        c.as[String].fold(
          _ => c.value.isNull.either(DecodingFailure("ShortText decoder got neither null nor String", Nil), ShortNull), 
          s => Right(ShortText(s)))
      }
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
      def apply(c: HCursor): Decoder.Result[LongTextLike] = {
        import mouse.boolean._
        c.as[String].fold(
          _ => c.value.isNull.either(DecodingFailure("LongText decoder got neither null nor String", Nil), LongNull), 
          s => Right(LongText(s)))
      }
    }
  }

  final case class Concept(
    key: String, name: String, formula: String, statements: NEL[String], 
    shortText: ShortTextLike, longText: LongTextLike, supports: List[SupportSourceLike], context: Option[StatementContext])

  object Concept {
    def apply(key: String, state: String, formula: String, support: SupportSourceLike, ctx: StatementContext): Concept = {
      val name = if (state.length < 80) state else key
      new Concept(
        key = key, name = name, formula = formula, statements = NEL(state, Nil), 
        shortText = ShortTextLike.nothing, longText = LongTextLike.nothing, supports = List(support), context = Some(ctx))
    }
    def apply(name: String, formula: String, statement: String): Concept = 
      apply(name, name, formula, NEL(statement, List()), ShortTextLike.nothing, LongTextLike.nothing, List(), None)
    def apply(key: String, name: String, formula: String, statement: String): Concept = 
      new Concept(key, name, formula, NEL(statement, List()), ShortTextLike.nothing, LongTextLike.nothing, List(), None)
    def apply(name: String, formula: String, statement: String, support: SupportSourceLike): Concept = 
      apply(name, name, formula, NEL(statement, List()), ShortTextLike.nothing, LongTextLike.nothing, List(support), None)
    def apply(key: String, name: String, formula: String, statement: String, support: SupportSourceLike): Concept = 
      new Concept(key, name, formula, NEL(statement, List()), ShortTextLike.nothing, LongTextLike.nothing, List(support), None)
    implicit val conceptCodec: Codec[Concept] = new Codec[Concept] {
      import mouse.boolean._
      import LongTextLike._, ShortTextLike._, StatementContext._, Support.SupportSourceLike._
      def apply(c: Concept): Json = Json.obj(
        "key" -> c.key.asJson, 
        "name" -> c.name.asJson, 
        "formula" -> c.formula.asJson, 
        "statements" -> c.statements.toList.asJson, 
        "shortText" -> c.shortText.asJson, 
        "longText" -> c.longText.asJson, 
        "supports" -> c.supports.asJson, 
        "context" -> c.context.fold(Json.Null)(_.asJson) )
      def apply(c: HCursor): Decoder.Result[Concept] = for {
        k <- c.get[String]("key")
        n <- c.get[String]("name")
        form <- c.get[String]("formula")
        states <- c.get[List[String]]("statements").flatMap(_.toNel.toRight(DecodingFailure(s"Empty statements for key $k", Nil)))
        short <- c.get[ShortTextLike]("shortText")
        long <- c.get[LongTextLike]("longText")
        supps <- c.get[List[SupportSourceLike]]("supports")
        context <- c.get[StatementContext]("context").fold(
          _ => c.value.isNull.either(DecodingFailure("Concept decoder got neither null nor valid context", Nil), None), 
          ctx => Right(Some(ctx)))
      } yield Concept(k, n, form, states, short, long, supps, context)
    }
  }

}
