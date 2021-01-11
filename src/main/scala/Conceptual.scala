package autoquiz

object Conceptual {
  import cats.data.{ NonEmptyList => NEL }
  import Support.SupportSourceLike
  
  sealed trait ShortTextLike
  
  final case object ShortNull extends ShortTextLike
  final case class ShortText(get: String) extends ShortTextLike
  
  object ShortTextLike {
    def nothing: ShortTextLike = ShortNull
  }
  
  sealed trait LongTextLike
  
  final case object LongNull extends LongTextLike
  final case class LongText(get: String) extends LongTextLike
  
  object LongTextLike {
    def nothing: LongTextLike = LongNull
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
  }

}
