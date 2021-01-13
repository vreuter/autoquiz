package autoquiz

object Support {
  import java.io.File
  import cats.data.{ NonEmptyList => NEL }
  import cats.syntax.list._
  import mouse.boolean._
  import io.circe._, io.circe.parser._, io.circe.syntax._
  
  sealed trait SupportSourceLike

  object SupportSourceLike {
    implicit val supportLikeCodec: Codec[SupportSourceLike] = new Codec[SupportSourceLike] {
      import ProofLines._, SupportFile._
      def apply(ssl: SupportSourceLike): Json = ssl match {
        case pfl: ProofLines => pfl.asJson
        case sf: SupportFile => sf.asJson
      }
      def apply(c: HCursor): Decoder.Result[SupportSourceLike] = 
        JsonUtil.unifyDecoders(implicitly[Decoder[SupportFile]], implicitly[Decoder[ProofLines]])(c)
    }
  }

  private implicit val extantFileCodec: Codec[File] = new Codec[File] {
    def apply(f: File): Json = f.getPath.asJson
    def apply(c: HCursor): Decoder.Result[File] = c.as[String].map(
      p => new File(p)).flatMap(f => f.isFile.either(DecodingFailure(s"Missing file: $f", Nil), f))
  }

  sealed trait SupportFile extends SupportSourceLike
  object SupportFile {
    implicit val supportFileCodec: Codec[SupportFile] = new Codec[SupportFile] {
      import ImageFile._, LatexFile._
      def apply(sf: SupportFile): Json = sf match {
        case f: LatexFile => f.asJson
        case f: ImageFile => f.asJson
      }
      def apply(c: HCursor): Decoder.Result[SupportFile] = 
        JsonUtil.unifyDecoders(implicitly[Decoder[LatexFile]], implicitly[Decoder[ImageFile]])(c)
    }
  }

  final case class ImageFile(image: File) extends SupportFile {
    require(get.isFile, s"Alleged image file isn't a file: $image")
    final def get: File = image
  }
  object ImageFile {
    private[this] val key = "image"
    implicit val imageFileCodec: Codec[ImageFile] = new Codec[ImageFile] {
      def apply(f: ImageFile): Json = Json.obj(key -> f.get.asJson)
      def apply(c: HCursor): Decoder.Result[ImageFile] = c.get[File](key).map(f => ImageFile(f))
    }
  }
  
  final case class LatexFile(latex: File) extends SupportFile {
    require(get.isFile, s"Alleged image file isn't a file: $get")
    final def get: File = latex
  }
  object LatexFile {
    private[this] val key = "latex"
    implicit val latexFileCodec: Codec[LatexFile] = new Codec[LatexFile] {
      def apply(f: LatexFile): Json = Json.obj(key -> f.get.asJson)
      def apply(c: HCursor): Decoder.Result[LatexFile] = c.get[File](key).map(f => LatexFile(f))
    }
  }
  
  final case class ProofLines(get: NEL[String]) extends SupportSourceLike {
    final def list: List[String] = get.toList
  }
  
  object ProofLines {
    implicit val proofLinesCodec: Codec[ProofLines] = new Codec[ProofLines] {
      def apply(lines: ProofLines): Json = lines.get.toList.asJson
      def apply(c: HCursor): Decoder.Result[ProofLines] = c.as[List[String]].flatMap(
        _.toNel.toRight(DecodingFailure("Empty proof lines", Nil)).map(ls => ProofLines(ls)))
    }
  }
}
