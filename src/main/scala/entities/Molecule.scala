package autoquiz.entities

trait MoleculeLike[-A] {
  def molName: A => String
}

object MoleculeLike {
  implicit object MoleculeLikeForProtein extends MoleculeLike[Protein] {
    def molName: Protein => String = _.name
  }
}

final case class Protein(name: String) extends TextableEntity {
  def entityText: String = name
}
