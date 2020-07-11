object InteractiveTesting {
  
  import java.io.{ BufferedWriter =>  BW, File, FileWriter => FW }
  import java.nio.file.Paths

  interp.load.ivy("org.typelevel" %% "cats-core" % "2.1.1")
  import cats.data.{ NonEmptyList => NEL }
  import cats.syntax.list._
  interp.load.ivy("org.typelevel" % "mouse_2.13" % "0.25")
  import mouse.boolean._
  interp.load.ivy("io.circe" %% "circe-core" % "0.13.0")
  import io.circe._
  
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

  /** Create header/preamble for {@code TeX} source file, importing packages and adjusting settings. */
  def standardPreamble(title: String, author: String, date: String) = List(
    "\\documentclass{article}", 
    "\\usepackage{amsmath, amssymb}", 
    "\\usepackage{dirtytalk}", 
    "\\usepackage{enumitem}", 
    "\\usepackage[a4paper,bindingoffset=0.2in,%", 
    "             left=0.75in,right=0.75in,top=0.75in,bottom=0.75in,%", 
    "             footskip=.25in]{geometry}", 
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
  
  val libVersion = "0.1.0"
  val libname = "autoquiz"
  val codePath: File = findCodeUnsafe()
  val repoPath: File = new File(codePath, libname)
  val libPath: File = new File(Paths.get(
    repoPath.getPath, "target", "scala-2.13", s"${libname}_v${libVersion}.jar").toString)
  if (!libPath.isFile) { throw new Exception(s"Missing library JAR: ${libPath}") }
  interp.load.cp(ammonite.ops.Path(libPath.getPath))
  import autoquiz._, Targets.{ TargetFolder, Relpath }

  implicit val myRender = RenderQA(
    (q: String) => s"\\item{$q}", "QandA", "\\begin{answered}", "\\end{answered}")
  
  def writeAllQA(outfile: File, 
  exclude: Option[File => Boolean] = None): (Option[File], List[File], List[(File, String)]) = {
    import cats.Alternative, cats.instances.either._, cats.instances.list._
    def file2Name(f: File): String = {
      val nDot = f.getName.count(_ == '.')
      val fnFields = f.getName.split("\\.").dropRight(nDot).mkString(".").split("-").toList
      fnFields.head.forall(Character.isDigit).fold(fnFields.tail, fnFields) mkString " "
    }
    val folders = List("biology-random-notes", "mbgene", "AlbertsMolbio", "MathStat", "Virology-VRR-2020") map {
      foldName => new File(Paths.get(System.getenv("CODE"), foldName).toString) }
    val filtPred: File => Boolean = exclude.getOrElse((_: File) => false)
    val sectFpairs = (folders flatMap { DataSeek.seekData(
      _: File, NEL(".QandA.json", List()), file2Name) }).filterNot(sf => filtPred(sf._2))
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
        val preamble = standardPreamble("All Questions", "Vince Reuter", "July 11, 2020")
        val (files, groups) = trios.toList.foldRight(
          List.empty[File] -> List.empty[(String, NEL[TexQA])]){ 
            case ((n, f, qas), (fs, gs)) => (f :: fs, (n, qas) :: gs) }
        Writing.writeTexQA(outfile, preamble, groups.toNel.get)
        (Some(outfile), files, errFilePairs)
      }
    }
  }

  def isMbgeneTelomeresFile: File => Boolean = {
    import cats.instances.string._, cats.syntax.eq._
    _.getName === "09-DNA-Replication-Telomeres.QandA.json"
  }

  val (maybeTestF, processedInfiles, inErrPairs) = {
    val outfile = new File( Paths.get(repoPath.getPath, "outputs", "testQaOutAll.tex").toString )
    println(s"Output TeX file: $outfile")
    writeAllQA(outfile, Some(isMbgeneTelomeresFile) )
  }
  maybeTestF.fold(println("No TeX source to make")){ f => println(Writing.pdftex(f, Relpath("target"))) }

}