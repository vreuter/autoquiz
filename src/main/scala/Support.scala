package autoquiz

object Support {
  import java.io.File
  import cats.data.{ NonEmptyList => NEL }
  
  sealed trait SupportSourceLike
  
  final case class ImageFile(get: File) extends SupportSourceLike {
    require(get.isFile, s"Alleged image file isn't a file: $get")
  }
  
  final case class LatexFile(get: File) extends SupportSourceLike {
    require(get.isFile, s"Alleged image file isn't a file: $get")
  }
  
  final case class ProofLines(get: NEL[String]) extends SupportSourceLike {
    final def list: List[String] = get.toList
  }
}
