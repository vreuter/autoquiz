/**
 * Testing out the notion and implementation of conceptuals
 *
 * @author Vince Reuter
 */
object InteractiveConceptuals {

  import java.io.{ BufferedWriter =>  BW, File, FileWriter => FW }
  import java.nio.file.Paths
  interp.load.ivy("org.typelevel" %% "cats-core" % "2.3.0")
  import cats.data.{ NonEmptyList => NEL }
  import cats.syntax.list._
  interp.load.ivy("org.typelevel" % "mouse_2.13" % "0.25")
  import mouse.boolean._
  interp.load.ivy("io.circe" %% "circe-core" % "0.13.0")
  import io.circe._, io.circe.syntax._

  // Attempt to find the path to the CODE folder.
  def findCodeSafe(envVar: String = "CODE"): Either[String, File] = {
    val envVarVal = System.getenv(envVar)
    (envVarVal != null && envVarVal != "").either(
      s"No value for env var: ${envVar}", envVarVal ) flatMap {
        v => { val d = new File(v); d.isDirectory.either(s"Code path isn't a directory: $d", d) } }
  }

  // Find path to CODE folder on this computer.
  def findCodeUnsafe(envVar: String = "CODE"): File = 
    findCodeSafe(envVar).fold(msg => throw new Exception(msg), identity _)

  // Load this library.
  val libVersion = "0.2.0-SNAPSHOT"
  val libname = "autoquiz"
  val codePath: File = findCodeUnsafe()
  val repoPath: File = new File(codePath, libname)
  val libPath: File = new File(Paths.get(
    repoPath.getPath, "target", "scala-2.13", s"${libname}_v${libVersion}.jar").toString)
  if (!libPath.isFile) { throw new Exception(s"Missing library JAR: ${libPath}") }
  interp.load.cp(ammonite.ops.Path(libPath.getPath))

  val ldErosionProofFile = {
    import java.nio.file.Paths
    new File(Paths.get(
      System.getenv("CODE"), 
      "popgen", 
      "LynchWalsh1-Genetics-Analysis-Quantitative-Traits", 
      "05-Sources-of-Genetic-Variation", 
      "LD-erosion.proof.src.tex").toString)
  }

  val effPopMaxTruePopProofFile = new File(Paths.get(
    System.getenv("CODE"), 
    "popgen", 
    "Effective-Population-Size-Sex-Balance-Attains-Full-Pop.proof.src.tex").toString)

  import autoquiz.Conceptual._, autoquiz.Support._

  val linkDiseq = Concept(
    key = "LD", 
    name = "linkage disequilibrium", 
    formula = "$D \\triangleq P_{A,B} - p_A p_B$", 
    statement = "Linkage disequilibrium is the difference between observed allelic joint occurrence, and joint occurrence under independence.")

  val ldErosion = Concept(
    name = "LD erosion", 
    formula = "$D(t) = (1-c)^t D(0) \\text{, where } c \\text{ is the \\textit{recombination rate}}$", 
    statement = "The recombination rate is the rate of linkage equilibrium erosion.",
    support = LatexFile(ldErosionProofFile))

  val effPopMaxAtN = Concept(
    key = "sex-balance-max-eff-pop-at-N", 
    name = "Sex balance maximizes effective population size at true population size.", 
    formula = "$N_f = N_m \\implies N_e = N$", 
    statement = "Sex balance maximizes effective population size at true population size.", 
    support = LatexFile(effPopMaxTruePopProofFile))

  val effPopInvProbIBD = Concept(
    key = "eff-pop-inv-IBD-prob", 
    name = "Effective population size is inverse of indirect IBD probability.", 
    formula = "\\frac{1}{N_e} = P(2 \\text{ copies of a grandparent allele } | \\text{ parents are half-sibs }) \\cdot P(\\text{parents are half-sibs})",
    support = ProofLines(NEL("N_e &\\triangleq \\frac{1}{4}\\big(1/N_f + 1/N_m\\big)", List(
      "&= \\Big(\\frac{1}{2}\\Big)^2\\big(1/N_f + 1/N_m\\big)", 
      "&= P(\\text{particular grandparental allele})^2 \\cdot \\big(P(\\text{parents share mother}) + P(\\text{parents share father})\\big)", 
      "&= P(2 \\text{ copies of partiular grandparent alleles}) \\cdot P(\\text{parents are half-sibs})"))), 
    statement = "\\textbf{\\textit{Effective} population} size is inverse of \\textbf{\\textit{indirect} IBD} probability.")
  
  val effPopRatioCouplings = Concept(
    key = "eff-pop-ratio-couplings", 
    name = "\\textbf{\\textit{Effective} population} size is true size scaled by a ratio of couplings.", 
    statement = "\\textbf{\\textit{Effective} population} size is the reduction, via scaling by the ratio between \\textit{actual} number of couplings available to number available under sex parity (assuming \\textit{random mating}), of true population size.", 
    formula = "N_e = r \\cdot N \\text{, where } r \\text{ is a ratio of counts of couplings}", 
    support = ProofLines(NEL(
      "r \\triangleq \\frac{N_f \\cdot N_m}{N/2 \\cdot N/2} = \\frac{N_f \\cdot N_m}{N^2/4} = \\frac{4 N_f \\cdot \\cdot N_m}{N^2}", 
      List("\\implies r \\cdot N = \\frac{4 \\cdot N_f \\cdot N_m}{N^2}N = \\frac{4 N_f N_m}{N_f + N_m} = N_e")))
  )

  val jsons = List(linkDiseq.asJson, ldErosion.asJson, effPopMaxAtN.asJson, effPopInvProbIBD.asJson, effPopRatioCouplings.asJson)
  val tempfile = new File(new File(System.getenv("HOME")), "temptest-concepts.json")
  val tempW = new BW(new FW(tempfile))
  try {
    tempW.write(jsons.asJson.spaces2)
    tempW.newLine()
  } finally { tempW.close() }
  
  val maybeParse: Either[String, List[Concept]] = JsonUtil.readJsonFile[List[Concept]](tempfile)

}