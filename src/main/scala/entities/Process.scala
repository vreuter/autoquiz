package autoquiz.entities

trait ProcessLike[-A] {
  def processName: A => String
}

final case class Process(name: String) extends TextableEntity {
  def entityText: String = name
}

object ProcessLike {
  implicit object ProcessLikeForProcess extends ProcessLike[Process] {
    def processName: Process => String = _.name
  }
  implicit class ProcessLikeOps[A](a: A)(implicit ev: ProcessLike[A]) {
    def processName: String = ev.processName(a)
  }
}
