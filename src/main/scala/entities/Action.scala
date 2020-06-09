package autoquiz.entities

trait ActionLike[-A] { def actionName: A => String }

final case class Action(name: String) extends TextableEntity {
  def entityText: String = name
}

object Action {
  implicit object ActionLikeForAction extends ActionLike[Action] {
    def actionName: Action => String = _.name
  }
}
