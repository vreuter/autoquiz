package autoquiz

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

/**
 * Test the rendering/formatting and raw versions of phrases.
 *
 * @author Vince Reuter
 */
class TestPhrases extends AnyFunSuite with Matchers with ScalaCheckDrivenPropertyChecks {
  import org.scalacheck.{ Arbitrary, Gen }
  
  // Randomly choose a builder for one of the typed formatting instances.
  def genTeXtBuilder: Gen[String => TexFmt] = Gen.oneOf(
    (s: String) => Bold(s),
    (s: String) => Ital(s), 
    (s: String) => Uline(s)
  )

  // Generate a pair of raw text and a typed formatting instance.
  def genRawTexPair: Gen[(String, TexFmt)] = 
    Gen.zip(Arbitrary.arbitrary[String], genTeXtBuilder) map { case (s, f) => s -> f(s) }
  
  test("Single bold item renders properly") {
    forAll { s: String => Bold(s).tex shouldEqual s"\\textbf{$s}" }
  }
  
  test("Tex format raw is identity") {
    forAll(genRawTexPair) { 
      rawStrTeXtPair: (String, TexFmt) => rawStrTeXtPair._2.raw shouldEqual rawStrTeXtPair._1 }
  }
}
