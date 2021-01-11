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

  import autoquiz.Conceptual._, autoquiz.Support._

  val linkDiseq = Concept(
    key = "LD", 
    name = "linkage disequilibrium", 
    formula = "$D \\triangleq P_{A,B} - p_A p_B$", 
    statement = "Linkage disequilibrium is the difference between observed allelic joint occurrence, and joint occurrence under independence.")
  val ldErosionProofFile = {
    import java.nio.file.Paths
    new File(Paths.get(
      System.getenv("CODE"), 
      "popgen", 
      "LynchWalsh1-Genetics-Analysis-Quantitative-Traits", 
      "05-Sources-of-Genetic-Variation", 
      "LD-erosion.proof.src.tex").toString)
  }
  val ldErosion = Concept(
    name = "LD erosion", 
    formula = "$D(t) = (1-c)^t D(0) \\text{, where } c \\text{ is the \\textit{recombination rate}}$", 
    statement = "The recombination rate is the rate of linkage equilibrium erosion.",
    support = ldErosionProofFile)

}