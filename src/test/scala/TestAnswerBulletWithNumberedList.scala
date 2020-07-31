package autoquiz

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers


/**
 * Tests for a particular case of an answer list structure.
 *
 * Specifically, in the case used here, one of the bulleted answer items has a numbered list.
 *
 * @author Vince Reuter
 */
class TestAnswerBulletWithNumberedList extends AnyFunSuite with Matchers {
  import io.circe.parser.decode

  val qText = "What are the main validation questions to address in a ChIP-PCR experiment?"
  
  val ansTextParts = List(
    "Interest is to compare control amplicon vs. target amplicon in input DNA (no/nonspecific antibody) for primer bias, and to compare control amplicon vs. target in treated DNA to assess antibody efficacy.", 
    "Antibody efficacy: If the antibody effectively binds the protein/complex/modification of interest, the treatment samples' target region should be enriched (lower cycle threshold) relative to those samples' control region. This suggests that the antibody's sensitive and specific.", 
    "Primer efficacy/bias: targeting a control locus/amplicon helps to establish a baseline for amplification of the respective regions. Then we can compare the change in CT for a given sample. This facilitates comparison of the target to control in the treated sample, which is ultimately what we're after." , 
    "Example:", 
    "In input DNA, both the control amplicon and the target amplicon show CT of 27.", 
    "In the IP sample(s), the control amplicon has CT of 30 while treated (specific Ab) samples have CT of 27. This implies $8x = 2^{3}x$ enrichment of the target.", 
    "We can use the raw difference in the CT between the regions in the treated sample because the baseline difference is $0 = 27 - 27$ between the amplicons in the input DNA.", 
  )
  
  val ansBullNum = """{
  "Q": "What are the \\textit{main \\textbf{validation} questions} to address in a \\textbf{ChIP-PCR} experiment?", 
  "A": [
    "Interest is to compare control amplicon vs. target amplicon in \\textit{\\textbf{input} DNA} (no/nonspecific antibody) for \\textbf{\\textit{primer bias,}} and to compare control amplicon vs. target in \\textit{\\textbf{treated} DNA} to assess \\textbf{\\textit{antibody efficacy.}}", 
    "\\textbf{\\textit{Antibody} efficacy:} If the antibody effectively binds the protein/complex/modification of interest, the treatment samples' target region should be enriched (lower cycle threshold) relative to those samples' control region. This suggests that the antibody's sensitive and specific.", 
    "\\textbf{\\textit{Primer} efficacy/bias:} targeting a \\textbf{\\textit{control} locus/amplicon} helps to \\textit{establish a \\textbf{baseline}} for amplification of the respective regions. Then we can \\textit{compare the \\textbf{change in CT}} for a given sample. This facilitates comparison of the target to control in the treated sample, which is ultimately what we're after." , 
    "\\underline{Example:}", 
    "\\begin{enumerate}", 
    "  \\item{In input DNA, both the control amplicon and the target amplicon show CT of 27.}", 
    "  \\item{In the IP sample(s), the control amplicon has CT of 30 while treated (specific Ab) samples have CT of 27. This implies $8x = 2^{3}x$ enrichment of the target.}", 
    "  \\item{We can use the raw difference in the CT between the regions in the treated sample because the baseline difference is $0 = 27 - 27$ between the amplicons in the input DNA.}", 
    "\\end{enumerate}"
  ]
}"""

  implicit val myRender = RenderQA((q: String) => s"\\item{$q}", "QandA", "\\begin{answered}", "\\end{answered}")
  
  test("An answer bullet with numbered list should render properly as TeX") {
    import cats.data.{ NonEmptyList => NEL }, cats.syntax.list._
    val tqa = decode[TexQA](ansBullNum)
    val expLines = List(
      "\\section{ChIP}", 
      "\\begin{QandA}",
      "  \\item{What are the \\textit{main \\textbf{validation} questions} to address in a \\textbf{ChIP-PCR} experiment?}", 
      "    \\begin{answered}",
      "    \\begin{itemize}", 
      "      \\item{Interest is to compare control amplicon vs. target amplicon in \\textit{\\textbf{input} DNA} (no/nonspecific antibody) for \\textbf{\\textit{primer bias,}} and to compare control amplicon vs. target in \\textit{\\textbf{treated} DNA} to assess \\textbf{\\textit{antibody efficacy.}}}",
      "      \\item{\\textbf{\\textit{Antibody} efficacy:} If the antibody effectively binds the protein/complex/modification of interest, the treatment samples' target region should be enriched (lower cycle threshold) relative to those samples' control region. This suggests that the antibody's sensitive and specific.}", 
      "      \\item{\\textbf{\\textit{Primer} efficacy/bias:} targeting a \\textbf{\\textit{control} locus/amplicon} helps to \\textit{establish a \\textbf{baseline}} for amplification of the respective regions. Then we can \\textit{compare the \\textbf{change in CT}} for a given sample. This facilitates comparison of the target to control in the treated sample, which is ultimately what we're after.}",
      "      \\item{\\underline{Example:}}",
      "      \\begin{enumerate}", 
      "        \\item{In input DNA, both the control amplicon and the target amplicon show CT of 27.}", 
      "        \\item{In the IP sample(s), the control amplicon has CT of 30 while treated (specific Ab) samples have CT of 27. This implies $8x = 2^{3}x$ enrichment of the target.}", 
      "        \\item{We can use the raw difference in the CT between the regions in the treated sample because the baseline difference is $0 = 27 - 27$ between the amplicons in the input DNA.}", 
      "      \\end{enumerate}", 
      "    \\end{itemize}", 
      "    \\end{answered}", 
      "\\end{QandA}"
    )
    val sectName = "ChIP"
    val qas = NEL(tqa.toOption.get, List())
    val obsLines = myRender.asChunkWriter("ChIP", qas).split("\n")
    expLines shouldEqual obsLines
    /* Alternate formulation for debugging
    expLines.size shouldEqual obsLines.size
    expLines zip obsLines filterNot { case (exp, obs) => obs == exp } match {
      case Nil => succeed
      case mismatched => {
        println(mismatched.head match { case (l1, l2) => l1.toList.zip(l2.toList).zipWithIndex.find(t => t._1._1 != t._1._2) })
        fail(s"${mismatched.size} mismatches:\n${mismatched map { case (l1, l2) => s"$l1\n$l2" } mkString "\n"}")
      }
    }
    */
  }

  test("An answer bullet with numbered list should render properly as plain") {
    val tqa = decode[TexQA](ansBullNum)
    tqa.isRight shouldBe true
    val (q, as) = tqa.toOption.get.plain
    q shouldEqual qText
    println(as mkString "\n")
    println(ansTextParts mkString "\n")
    as shouldEqual ansTextParts
    /* Alternate formulation for debugging
    as.size shouldEqual ansTextParts.size
    as zip ansTextParts filterNot { case (obs, exp) => obs == exp } match {
      case Nil => succeed
      case mismatched => fail(s"${mismatched.size} mismatches: ${mismatched}")
    }
    */
  }

}
