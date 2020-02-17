package autoquiz

object Parsing {
  import scala.io.Source
  import java.io.File
  import io.circe._, io.circe.parser.decode
  def readFile(f: File): Either[Error, List[TexQA]] = 
    decode[List[TexQA]](Source.fromFile(f).mkString)
}
