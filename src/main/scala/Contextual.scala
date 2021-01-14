package autoquiz

/**
 * Statement contexts
 *
 * @author Vince Reuter
 */
object Contextual {
  import cats.data.{ NonEmptyList => NEL }, cats.syntax.list._
  
  sealed trait StatementContext
  
  final case class NamedAssumption(name: String) extends StatementContext {
    final def get: String = name
  }
  
  final case class AssumptionCollection(get: NEL[String]) extends StatementContext
  
  object StatementContext {
    import io.circe._, io.circe.syntax._
    implicit val contextCodec: Codec[StatementContext] = new Codec[StatementContext] {
      def apply(c: StatementContext): Json = c match {
        case NamedAssumption(n) => n.asJson
        case AssumptionCollection(as) => as.toList.asJson
      }
      def apply(c: HCursor): Decoder.Result[StatementContext] = c.as[String].fold(
        _ => c.as[List[String]].flatMap(
          _.toNel.toRight(DecodingFailure("Empty assumptions list", Nil)).map(
            as => AssumptionCollection(as))), 
        s => Right(NamedAssumption(s)))
    }
  }
}
