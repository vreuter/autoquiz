package autoquiz

/**
 * Representations of {@code pdflatex} targets.
 *
 * @author Vince Reuter
 */
object Targets {
  import java.io.File
  
  /** {@code pdflatex --output-directory} */
  sealed trait TargetFolder {
    def name: String
    def texOutFolder: File => File
  }
  
  /**
   * Relative output directory for {@code pdflatex}
   *
   * @param name Folder name
   * @return New instance
   */
  final case class Relpath(name: String) extends TargetFolder {
    def texOutFolder: File => File = f => new File(f.getParentFile, name)
  }
  
  /**
   * Absolute output directory path for {@code pdflatex}
   *
   * @param name Path as text
   * @return New instance
   */
  final case class Abspath(name: String) extends TargetFolder {
    def texOutFolder: File => File = f => new File(name)
  }
}
