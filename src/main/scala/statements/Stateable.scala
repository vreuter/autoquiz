package autoquiz.statements

trait Stateable[-A] {
  def makeStatement: A => String
}

object Stateable {
  implicit class StatementOps[A](a: A)(implicit ev: Stateable[A]) {
    def statement: String = ev.makeStatement(a)
  }
}
