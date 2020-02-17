object InteractiveTesting {
  import java.io.File
  val libpath = "/home/vr/code/autoquiz/target/scala-2.12/autoquiz_v0.0.1-SNAPSHOT.jar"
  interp.load.cp(ammonite.ops.Path(libpath))
  import autoquiz._
  interp.load.ivy("org.typelevel" %% "cats-core" % "2.1.0")
  import cats.data.{ NonEmptyList => NEL }
  import cats.syntax.list._
  interp.load.ivy("io.circe" %% "circe-core" % "0.12.3")
  import io.circe._
  def parse2Nel: Either[Error, List[TexQA]] => NEL[TexQA] = _.toOption.get.toNel.get
  val testIn1 = new File("/home/vr/code/mbgene/07_TechniquesOfMolecularBiology/07-General.QandA.json")
  val qaG1 = Parsing.readFile(testIn1).toOption.get.toNel.get
  val testIn2 = new File("/home/vr/code/mbgene/07_TechniquesOfMolecularBiology/07-EMSA.QandA.json")
  val qaG2 = Parsing.readFile(testIn2).toOption.get.toNel.get
  val testOut1 = new File("/home/vr/testtex/testQaOut1.tex")
  def standardPreamble(title: String, author: String, date: String) = List(
    "\\documentclass{article}", 
    "\\usepackage{amsmath, amssymb}", 
    "\\usepackage{dirtytalk}", 
    "\\usepackage{enumitem}", 
    "\\newenvironment{QandA}{\\begin{enumerate}[label=\\bfseries Q\\arabic*.]}", 
    "                       {\\end{enumerate}}", 
    "\\newenvironment{answered}{\\par\\normalfont\\underline{Answer:}}{}", 
    "", 
    s"\\title{$title}", 
    s"\\author{$author}", 
    s"\\date{$date}", 
    "", 
    "\\begin{document}", 
    "", 
    "\\maketitle", 
    ""
  ) mkString "\n"
  implicit val myRender = RenderQA(
    (q: String) => s"\\item{$q}", "QandA", "\\begin{answered}", "\\end{answered}")
  Writing.writeTexQA( testOut1, standardPreamble("Test", "Vince", "Feb. 16"), NEL("General" -> qaG1, List("EMSA" -> qaG2)) )
}