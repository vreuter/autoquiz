package autoquiz

import org.scalatest.FunSuite

/**
 * Leverage question-and-answer instances at select locations on the local filesystem for testing
 *
 * Specifically, there are here hardcoded several repository roots designated as starting points for 
 * searches for question-and-answer data files.
 *
 * @author Vince Reuter
 */
class AllOutputsDataTests {
  import java.io.{ BufferedWriter =>  BW, File, FileWriter => FW }
  import java.nio.file.Paths
  import java.time.{ LocalDate }
  import Targets.{ TargetFolder, Relpath }
  import cats.data.{ NonEmptyList => NEL }
  import cats.syntax.list._
  import mouse.boolean._
  import io.circe._
  
  def parse2Nel: Either[Error, List[TexQA]] => NEL[TexQA] = _.toOption.get.toNel.get

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

  def makeDate: String = {
    val d = LocalDate.now()
    val m = d.getMonth.toString.split("").toList match {
      case h :: t => (h :: t.map(_.toLowerCase)) mkString ""
    }
    s"${m} ${d.getDayOfMonth}, ${d.getYear}"
  }

  implicit val myRender = RenderQA(
    (q: String) => s"\\item{$q}", "QandA", "\\begin{answered}", "\\end{answered}")
  
  /** Data file search roots (start points). */
  def qaDataFolders = List(
    "AlbertsMolbio", 
    "biology-random-notes", 
    "MathStat", 
    "mbgene", 
    "popgen") map { foldName => new File(Paths.get(System.getenv("CODE"), foldName).toString) }

  def outName: File => String = f => s"${f.getName}.tex"

  def writeAllQA(exclude: File => Boolean = (_: File) => false): (Option[File], List[File], List[(File, String)]) = {
    import cats.Alternative, cats.instances.either._, cats.instances.list._
    def file2Name(f: File): String = {
      val nDot = f.getName.count(_ == '.')
      val fnFields = f.getName.split("\\.").dropRight(nDot).mkString(".").split("-").toList
      fnFields.head.forall(Character.isDigit).fold(fnFields.tail, fnFields) mkString " "
    }
    val folders = List("biology-random-notes", "mbgene", "AlbertsMolbio", "MathStat") map {
      foldName => new File(Paths.get(System.getenv("CODE"), foldName).toString) }
    val sectFpairs = (folders flatMap { 
      DataSeek.seekData(_: File, NEL(".QandA.json", List()), file2Name) }).filterNot(sf => exclude(sf._2))
    val (errFilePairs, secFileGroupTrios) = Alternative[List].separate(
      sectFpairs map { case (sect, f) =>  Parsing.readFile(f) match {
        case Left(e) => Left[(File, String), (String, File, NEL[TexQA])](f -> e.getMessage)
        case Right(qas) => qas.toNel match {
          case None => Left[(File, String), (String, File, NEL[TexQA])](f -> "Empty parse")
          case Some(res) => Right[(File, String), (String, File, NEL[TexQA])]((sect, f, res))
        }
      } }
    )
    secFileGroupTrios.toNel match {
      case None => (Option.empty[File], List.empty[File], errFilePairs)
      case Some(trios) => {
        val outfile = new File("/home/vr/testtex/testQaOutAll.tex")
        val preamble = standardPreamble("All Questions", "Vince Reuter", makeDate)
        val (files, groups) = trios.toList.foldRight(
          List.empty[File] -> List.empty[(String, NEL[TexQA])]){ 
            case ((n, f, qas), (fs, gs)) => (f :: fs, (n, qas) :: gs) }
        Writing.writeTexQA(outfile, preamble, groups.toNel.get)
        (Some(outfile), files, errFilePairs)
      }
    }
  }

}
