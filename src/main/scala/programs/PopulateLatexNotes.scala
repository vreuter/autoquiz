package autoquiz

import com.typesafe.scalalogging.StrictLogging

/**
 * Program to populate {@code LaTeX} notes files with concept instances
 *
 * @author Vince Reuter
 */
object PopulateLatexNotes extends StrictLogging {
  import java.io.{ BufferedWriter, File, FileWriter }
  import scala.io.Source
  import scopt._
  import autoquizinfo.BuildInfo
  import cats.Show
  import cats.data.{ NonEmptyList => NEL }
  import cats.instances.int._
  import cats.syntax.either._, cats.syntax.list._, cats.syntax.show._
  import mouse.boolean._
  import Conceptual._, Support._
  
  val placeholderDirective = "%INSERT:"

  case class Config(
    instanceManifest: File = new File(""), 
    templateRoot: File = new File(""), 
    summaryFile: File = new File("")
  )
  
  def main(args: Array[String]): Unit = {
    val parser = new OptionParser[Config](BuildInfo.name) {
      head(s"${BuildInfo.name}.PopulateLatexNotes", s"${BuildInfo.version}")
      opt[File]('I', "instanceManifest")
        .required()
        .action((f, c) => c.copy(instanceManifest = f))
        .validate(f => f.isFile.either(s"Instances manifest isn't file: $f", ()))
        .text("Path to concepts instances manifest file, listing JSON sources")
      opt[File]('S', "summary")
        .required()
        .action((f, c) => c.copy(summaryFile = f))
        .text("Path to file to write processing summary")
      opt[File]('T', "templateRoot")
        .required()
        .action((d, c) => c.copy(templateRoot = d))
        .validate(d => if (d.isDirectory) Right(()) else Left(s"Templates root folder isn't a directory: $d"))
        .text("Path to filesystem root in which to find LaTeX templates")
    }
    
    parser.parse(args, Config()).fold(throw new Exception("Bad use.")){ conf => {
      logger.info(s"Reading instances manifest: ${conf.instanceManifest}")
      val instanceFiles: List[File] = 
        readInstancesManifest(conf.instanceManifest).fold(msg => throw new Exception(msg), _.toList)
      logger.info("Building instances")
      val (instFails, concepts) = instanceFiles.foldRight(
        List.empty[(File, String)] -> List.empty[Concept]){
          case (instFile, (errPairs, accCon)) => { JsonUtil.readJsonFile[List[Concept]](instFile).fold(
            msg => ((instFile, msg) :: errPairs, accCon), newCons => (errPairs, newCons ::: accCon)) } }
      val concPool: Map[String, Concept] = toUniqueMap((_: Concept).key)(concepts).fold(
        concsByKey => {
          val errMsg = s"${concsByKey.size} repeated keys: ${concsByKey.toList map { case (k, cs) => s"$k: ${cs.size}" } mkString ", "}"
          throw new Exception(errMsg)
        }, identity _)
      logger.info(s"Templates root: ${conf.templateRoot}")
      val templateCandidates: List[File] = {
        import scala.sys.process._
        Seq("find", conf.templateRoot.getPath, "-name", "*.tex").lazyLines.toList.map(p => new File(p))
      }
      logger.info(s"Found ${templateCandidates.size} template candidates: ${templateCandidates.map(_.getPath) mkString ", "}")

      implicit val showConcept: Show[Concept] = Show.show(c => {
        val stateLines = List("\\\\par\\\\smallskip", "\\\\begin{center}", c.statements.head, "\\\\end{center}", "\\\\par\\\\smallskip")
        val formLines = List("\\\\begin{align*}", c.formula, "\\\\end{align*}")
        val eqvStates: List[String] = c.statements.tail match {
          case Nil => Nil
          case eqs => "\\\\underline{Equivalent statements}" :: "\\\\par\\\\smallskip" :: eqs
        }
        val supportLines: List[String] = c.supports match {
          case Nil => Nil
          case supp :: Nil => supp match {
            case ProofLines(ls) => ls.toList
            case LatexFile(f) => List(s"\\input{$f}")
            case ImageFile(f) => throw new Exception(s"Image file support show not yet supported (used for ${c.key})")
          }
          case supps => throw new Exception(s"${supps.size} supports for concept key ${c.key} -- only 1 support currently implemented")
        }
        (stateLines ::: formLines ::: eqvStates ::: supportLines) mkString "\n"
      })

      val results: List[ProcessResult] = templateCandidates.map(tf => {
        val (maybeOut, countByKey) = procFile(concPool)(tf)
        ProcessResult(tf, maybeOut, countByKey)
      })
      val unusedKeys = concPool.keySet -- results.foldLeft(Set.empty[String]){ case (acc, r) => acc ++ r.keyCounts.keySet }
      if (unusedKeys.nonEmpty) {
        logger.info(s"${unusedKeys.size} unused concept key(s): ${unusedKeys.toList.sorted mkString ", "}")
      }
      val unfoundKeys: List[(File, Set[String])] = results.foldRight(List.empty[(File, Set[String])]){
        case (r, acc) => r.missingKeys match {
          case Nil => acc
          case missed => (r.infile, missed.toSet) :: acc
        }
      }
      if (unfoundKeys.nonEmpty) {
        logger.warn(s"${unfoundKeys.size} file(s) with missing key(s): ${unfoundKeys.map(_._1.getPath) mkString ", "}")
        val allUnfound: Set[String] = unfoundKeys.map(_._2).reduce(_ ++ _)
        logger.warn(s"${allUnfound.size} unfound key(s): ${allUnfound.toList.sorted mkString ", "}")
      }
      val summaryWriter = new BufferedWriter(new FileWriter(conf.summaryFile))
      try {
        summaryWriter.write(List("infile", "outfile", "key", "nTimesReplaced") mkString "\t")
        summaryWriter.newLine()
        results foreach { r => {
          val currIn = r.infile.getPath
          val currOut = r.outfile.fold("NA")(_.getPath)
          r.keyCounts foreach { case (k, n) => {
            val line = List(currIn, currOut, k, n.show) mkString "\t"
            summaryWriter.write(line)
            summaryWriter.newLine()
          } }
        } }
      } finally { summaryWriter.close() }
    }}

    logger.info("Done.")
  }

  final case class ProcessResult(infile: File, outfile: Option[File], missingKeys: List[String], keyCounts: Map[String, Int])

  object ProcessResult {
    def apply(in: File, out: Option[File], countByKey: Map[String, Option[Int]]): ProcessResult = {
      val (missed, counted) = countByKey.foldLeft(List.empty[String] -> Map.empty[String, Int]){
        case ((ms, ns), (k, maybeX)) => maybeX.fold((k :: ms, ns))(x => (ms, ns + (k -> x))) }
      new ProcessResult(in, out, missed, counted)
    }
  }

  def procFile(pool: Map[String, Concept])(infile: File)(implicit ev: Show[Concept]): (Option[File], Map[String, Option[Int]]) = {
    val outfile = new File(infile.getPath.replace(".tex", ".filled.tex"))
    val lines = Source.fromFile(infile).getLines().toList
    val keysLinesPairs = extractKeys(lines)
    if (keysLinesPairs.forall(_._1.isEmpty)) Option.empty[File] -> Map()
    else {
      val w = new BufferedWriter(new FileWriter(outfile))
      try {
        Some(outfile) -> keysLinesPairs.foldLeft(Map.empty[String, Option[Int]]) {
          case (acc, (Nil, line)) => { w.write(line); w.newLine(); acc }
          case (acc, (keys, line)) => {
            val (updated, newTallies) = keys.foldLeft(line -> acc){ case ((l, subAcc), k) => {
              val vOpt: Option[Concept] = pool.get(k)
              vOpt match {
                case None => (l, subAcc + (k -> Option.empty[Int]))
                case Some(v) => (l.replaceAll(s"${placeholderDirective}${k}", v.show), subAcc + (k -> subAcc.get(k).fold(Some(1))(
                  xOpt => { val x = xOpt.getOrElse(throw new Exception("Illegal state!")); Some(x+1) } )))
              }
            } }
            w.write(updated)
            w.newLine()
            newTallies
          }
        }
      }
      finally { w.close() }
    }
  }

  def extractKeys(lines: List[String]): List[(List[String], String)] = {
    import scala.util.matching.Regex
    val re = raw"%INSERT:[^\s]+".r
    val getKey: Regex.Match => String = _.toString.stripPrefix(placeholderDirective)
    lines.map(l => re.findAllMatchIn(l).toList.map(getKey) -> l)
  }

  def toUniqueMap[A, K](getKey: A => K)(items: Iterable[A]): Either[NEL[(K, List[A])], Map[K, A]] = {
    val grouped = items.groupBy(getKey).view.mapValues(_.toList).toMap
    grouped.toList.filter(_._2.size > 1).toNel.toLeft(grouped.view.mapValues(_.head).toMap)
  }

  def readInstancesManifest(f: File): Either[String, NEL[File]] = {
    import cats.Alternative
    val (missing, extant) = Alternative[List].separate(
      Source.fromFile(f).getLines().toList.map(l => { val f = new File(l); f.isFile.either(l, f)}))
    missing.nonEmpty.either(s"${missing.size} missing instance file(s): ${missing mkString ", "}", extant).flatMap(
      _.toNel.toRight(s"No files in manifest: $f"))
  }

}
