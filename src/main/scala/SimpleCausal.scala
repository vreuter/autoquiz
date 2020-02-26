package autoquiz

import entities._

final case class SimpleCausal[A](
  a: TextableEntity, action: A, b: TextableEntity, 
  reasons: List[String])(implicit actEv: ActionLike[A])

object SimpleCausal {
  def apply[A](a: TextableEntity, action: A, b: TextableEntity)(implicit ev: ActionLike[A]) = 
    new SimpleCausal(a, action, b, List())
  def apply[A](a: TextableEntity, action: A, b: TextableEntity, reason: String)(implicit ev: ActionLike[A]) = 
    new SimpleCausal(a, action, b, List(reason))
}
