object InteractiveTesting {
  
  import java.io.{ BufferedWriter =>  BW, File, FileWriter => FW }
  import java.nio.file.Paths
  val libpath = "/home/vr/code/autoquiz/target/scala-2.12/autoquiz_v0.0.4-SNAPSHOT.jar"
  interp.load.cp(ammonite.ops.Path(libpath))
  import autoquiz._, Targets.{ TargetFolder, Relpath }
  interp.load.ivy("org.typelevel" %% "cats-core" % "2.1.0")
  import cats.data.{ NonEmptyList => NEL }
  import cats.syntax.list._
  interp.load.ivy("org.typelevel" % "mouse_2.12" % "0.19")
  import mouse.boolean._
  interp.load.ivy("io.circe" %% "circe-core" % "0.12.3")
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
  
  implicit val myRender = RenderQA(
    (q: String) => s"\\item{$q}", "QandA", "\\begin{answered}", "\\end{answered}")
  
  def writeAllQA: (Option[File], List[File], List[(File, String)]) = {
    import cats.Alternative, cats.instances.either._, cats.instances.list._
    val ext = ".QandA.json"
    def file2Name(f: File): String = {
      val nDot = f.getName.count(_ == '.')
      val fnFields = f.getName.split("\\.").dropRight(nDot).mkString(".").split("-").toList
      fnFields.head.forall(Character.isDigit).fold(fnFields.tail, fnFields) mkString " "
    }
    val folders = List("mbgene", "AlbertsMolbio") map {
      foldName => new File(Paths.get(System.getenv("CODE"), foldName).toString) }
    val sectFpairs = folders flatMap { DataSeek.seekData(_: File, NEL(ext, List()), file2Name) }
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
        val preamble = standardPreamble("All Questions", "Vince Reuter", "February 18, 2020")
        val (files, groups) = trios.toList.foldRight(
          List.empty[File] -> List.empty[(String, NEL[TexQA])]){ 
            case ((n, f, qas), (fs, gs)) => (f :: fs, (n, qas) :: gs) }
        Writing.writeTexQA(outfile, preamble, groups.toNel.get)
        (Some(outfile), files, errFilePairs)
      }
    }
  }

  val (maybeTestF, processedInfiles, inErrPairs) = writeAllQA
  maybeTestF.fold(println("No TeX source to make")){ f => println(Writing.pdftex(f, Relpath("target"))) }

}