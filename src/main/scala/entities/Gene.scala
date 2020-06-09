package autoquiz.entities

trait GeneLike[-A] {
  def geneName: A => String
}

final case class Gene(name: String) extends TextableEntity {
  def entityText = name
}

object Gene {
  implicit object GeneLikeForGene extends GeneLike[Gene] {
    def geneName: Gene => String = _.name
  }
}