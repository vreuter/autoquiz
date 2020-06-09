package autoquiz

/**
 * JSON writing-related functionality for questions and answers
 *
 * @author Vince Reuter
 */
object Writing {
  import java.io.{ BufferedWriter => BW, File, FileWriter => FW }
  import cats.data.{ NonEmptyList => NEL }
  import Targets.TargetFolder

  /**
   * Run {@code pdflatex} to produce PDF from {@code TeX} source.
   *
   * @param texfile Path to the {@code TeX} source file
   * @param out Representation of path to output folder
   * @param mkdir Whether to license creation of intermediate folder(s)
   * @return Either a {@code Left} wrapping hypothetical output file path and 
   *         the error that occurred, or a {@code Right} wrapping path to 
   *         output file written
   */
  def pdftex(texfile: File, out: TargetFolder, 
    mkdir: Boolean = true): Either[(File, RuntimeException), File] = {
    import scala.sys.process._
    val outfile = new File(out.texOutFolder(texfile), texfile.getName)
    val progPath = {
      try { Right(Seq("which", "pdflatex").lazyLines.head) }
      catch { case e: RuntimeException => Left(outfile -> e) }
    }
    progPath flatMap { tex2Pdf(texfile, out, _: String) }
  }
  
  /**
   * @throws java.lang.RuntimeException if the program crashes
   */
  private def tex2Pdf(infile: File, target: TargetFolder, 
    progPath: String, mkdir: Boolean = true): Either[(File, RuntimeException), File] = {
    import scala.sys.process._
    import mouse.boolean._
    val outfile = new File(target.texOutFolder(infile), infile.getName)
    val outdir = outfile.getParentFile
    val cmd = Seq(progPath, "--output-directory", outdir.getPath, infile.getPath)
    println(s"Command: ${cmd mkString " "}")
    for {
      _ <- (outdir.isDirectory, mkdir) match {
        case (true, _) => Right[(File, RuntimeException), File](outfile)
        case (false, true) => { outdir.mkdirs(); Right[(File, RuntimeException), File](outfile) }
        case (false, false) => { Left[(File, RuntimeException), File](
          outfile -> new RuntimeException(s"Target folder missing: $outdir")) }
      }
      lastLine <- try { Right(cmd.lazyLines.last) } catch { case e: RuntimeException => Left(outfile -> e) }
      res <- lastLine.startsWith("Transcript written on").either(
        outfile -> new RuntimeException(s"Suspicious last log line: $lastLine"), outfile)
    } yield res
  }

  /**
   * Write to disk a collection of question-and-answer groups (e.g. by topic).
   *
   * @param preamble The intro to the {@code TeX} document needed for formatting
   * @param renderSection How to produce a block of text for a group of questions and answers
   * @param f Path to the file to write
   * @param qaGroups The question-and-answer groups to write, keyed by topic/group name
   */
  def writeTexFile(
    preamble: String, renderSection: (String, NEL[TexQA]) => String)(
    f: File, qaGroups: NEL[(String, NEL[TexQA])]): Unit = {
    val sectText = qaGroups map { case (sectHead, qas) => renderSection(sectHead, qas) }
    val chunks = preamble :: (sectText.toList :+ "\\end{document}")
    val writer = new BW(new FW(f))
    try { chunks foreach { s => { writer.write(s); writer.newLine() } } }
    finally { writer.close() }
  }
  
  /**
   * Write to disk a collection of question-and-answer groups (e.g. by topic).
   *
   * @param f Path to the file to write
   * @param preamble The intro to the {@code TeX} document needed for formatting
   * @param qaGroups The question-and-answer groups to write, keyed by topic/group name
   * @param render How to produce a block of text for a group of questions and answers
   */
  def writeTexQA(f: File, preamble: String, qaGroups: NEL[(String, NEL[TexQA])])(implicit render: RenderQA): Unit = 
    writeTexFile(preamble, render.asChunkWriter)(f, qaGroups)

}
