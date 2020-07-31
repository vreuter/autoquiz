package autoquiz

/**
 * Entity with which to produce {@code TeX} for a question-and-answer.
 *
 * @author Vince Reuter
 */
trait RenderQA {
  import cats.data.{ NonEmptyList => NEL }
  import mouse.boolean._
  
  /** Name for {@code TeX} environment for question-and-answer section */
  def env: Option[String]
  
  /** How to present the given question, e.g. by prefixing with {@code \item} */
  def ask(q: String): String
  
  /** The header for beginning of an answer */
  def headA: String
  
  /** The footer for end of an answer */
  def footA: String
  
  /** Transform this instance into a 2-arg function mapping QA topic and instances to text block. */
  def asChunkWriter: (String, NEL[TexQA]) => String = {
    val addEnv: List[String] => List[String] = 
      parts => env.fold(parts){ e => s"\\begin{$e}" :: (parts :+ s"\\end{$e}") }
    (sectHead: String, qas: NEL[TexQA]) => {
      val nLeftPad = 2
      val qText: TexQA => String = qa => s"${Seq.fill(nLeftPad)(" ") mkString ""}${ask(qa.q)}"
      def aText(nIdentStep: Int): String => String = s => s"${Seq.fill(nLeftPad * nIdentStep)(" ") mkString ""}$s"
      val step2Text = aText(2)
      val init = qas.toList flatMap { qa => {
        val aList = qa.a.toList match {
          case Nil => throw new Exception(s"No answer for question in section $sectHead: ${qa.q}")
          case a :: Nil => List(aText(2)(a))
          case as => {
            val itemText: String => String = 
              a => aText(3)(hasTexFmtDir(rmHeadSpaces(a)).fold(a, s"\\item{$a}"))
            step2Text("\\begin{itemize}") :: (as.map(itemText) :+ step2Text("\\end{itemize}"))
          }
        }
        qText(qa) :: step2Text(headA) :: (aList :+ step2Text(footA))
      } }
      (s"\\section{$sectHead}" :: addEnv(init)) mkString "\n"
    }
  }
}

/**
 * Question-and-answer rendering helpers
 *
 * @author Vince Reuter
 */
object RenderQA {
  /** Create an instance by proving the question presentation function, environment name, and answer head/foot. */
  def apply(presentQuestion: String => String, envname: String, aHead: String, aFoot: String): RenderQA = 
    new RenderQA {
      def env = Some(envname)
      def ask(q: String): String = presentQuestion(q)
      def headA = aHead
      def footA = aFoot
    }
}
