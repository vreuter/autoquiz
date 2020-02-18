package autoquiz

object Targets {
  import java.io.File
  sealed trait TargetFolder {
    def name: String
    def texOutFolder: File => File
  }
  final case class Relpath(name: String) extends TargetFolder {
    def texOutFolder: File => File = f => new File(f.getParentFile, name)
  }
  final case class Abspath(name: String) extends TargetFolder {
    def texOutFolder: File => File = f => new File(name)
  }
}
