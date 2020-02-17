package autoquiz

trait RenderQA {
  def ask(q: String): String
  def headA: String
  def footA: String
}

object RenderQA {
  def apply(presentQuestion: String => String, head: String, foot: String): RenderQA = 
    new RenderQA {
      def ask(q: String): String = presentQuestion(q)
      def headA = head
      def footA = foot
    }
}
