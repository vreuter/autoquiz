package autoquiz

object Writing {
  import java.io.{ BufferedWriter => BW, File, FileWriter => FW }
  import cats.data.{ NonEmptyList => NEL }
  def writeTexFile(
    preamble: String, renderSection: (String, NEL[TexQA]) => String)(
    f: File, qaGroups: NEL[(String, NEL[TexQA])]): Unit = {
    val sectText = qaGroups map { case (sectHead, qas) => renderSection(sectHead, qas) }
    val writer = new BW(new FW(f))
    try { (preamble :: sectText).toList foreach { s => { writer.write(s); writer.newLine() } } }
    finally { writer.close() }
  }
  def writeTexQA(f: File, preamble: String, qaGroups: NEL[(String, NEL[TexQA])])(implicit render: RenderQA): Unit = 
    writeTexFile(preamble, render.asChunkWriter)(f, qaGroups)
}
