package autoquiz

/**
 * JSON writing-related functionality for questions and answers
 *
 * @author Vince Reuter
 */
object Writing {
  import java.io.{ BufferedWriter => BW, File, FileWriter => FW }
  import cats.data.{ NonEmptyList => NEL }
  
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
