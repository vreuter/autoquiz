package autoquiz

/**
 * JSON utility functions
 *
 * @author Vince Reuter
 */
object JsonUtil {
  import scala.io.Source
  import java.io.File
  import io.circe._, io.circe.syntax._
  
  private[autoquiz] def readJsonFile[A](f: File)(implicit decode: Decoder[A]): Either[String, A] = {
    import scala.io.Source
    import cats.syntax.either._
    val content = Source.fromFile(f).mkString
    io.circe.parser.parse(content) match {
      case Left(e) => Left(e.getMessage)
      case Right(doc) => decode(doc.hcursor).leftMap(_.getMessage)
    }
  }

  private[autoquiz] def unifyDecoders[A, B, U](dA: Decoder[A], dB: Decoder[B]): Decoder[U] = 
    dA.either(dB).map(_.fold(_.asInstanceOf[U], _.asInstanceOf[U]))

}
