package autoquiz.entities

import cats.data.{ NonEmptyList => NEL }

trait Contextualized[-A] {
  def context: A => String
}

final case class Context(get: String) extends TextableEntity {
  def entityText: String = get
}

object Conditions {
  implicit object ContextualizedForContext extends Contextualized[Context] {
    def context: Context => String = _.get
  }
}
