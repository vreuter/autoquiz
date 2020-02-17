package autoquiz

trait RenderQA {
  import cats.data.{ NonEmptyList => NEL }
  def env: Option[String]
  def ask(q: String): String
  def headA: String
  def footA: String
  def asChunkWriter: (String, NEL[TexQA]) => String = {
    val addEnv: List[String] => List[String] = 
      parts => env.fold(parts){ e => s"\\begin{$e}" :: (parts :+ s"\\end{$e}") }
    (sectHead: String, qas: NEL[TexQA]) => {
      val init = qas.toList flatMap { qa => ask(qa.q) :: headA :: (qa.a :+ footA) }
      (sectHead :: addEnv(init)) mkString "\n"
    }
  }
}

object RenderQA {
  def apply(presentQuestion: String => String, envname: String, aHead: String, aFoot: String): RenderQA = 
    new RenderQA {
      def env = Some(envname)
      def ask(q: String): String = presentQuestion(q)
      def headA = aHead
      def footA = aFoot
    }
}
