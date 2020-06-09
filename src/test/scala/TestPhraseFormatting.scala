package autoquiz

import org.scalatest.{ FunSuite, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

/**
 * Test the rendering/formatting and raw versions of phrases.
 *
 * @author Vince Reuter
 */
class TestPhrases extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {
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
