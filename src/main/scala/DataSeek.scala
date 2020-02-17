package autoquiz

/**
 * Finding data files to use for question-and-answer generation.
 *
 * @author Vince Reuter
 */
object DataSeek {
  import scala.sys.process._
  import java.io.File
  import cats.data.{ NonEmptyList => NEL }
  
  /**
   * Run a {@code find} based on file extension, to get all MIDI within a FS subtree.
   *
   * @param ext The extension to match
   * @return Function that accepts a filesystem subtree root (folder) in which to search, 
   *         and then runs the {@code find} query from the filesystem subtree rooted 
   *         at the givemn folder
   */
  def getAllExt(ext: String): File => List[File] = root => {
    require(root.isDirectory, s"Search root isn't a directory: ${root}")
    val cmd = Seq("find", root.getPath, "-type", "f", "-name", s"*$ext")
    println(s"Command: ${cmd mkString " "}")
    cmd.lineStream.toList map { fp => new File(fp) }
  }


  /**
   * From a filesystem subtree of interest, gather files data files, associating each with section title/header.
   *
   * @param rootdir FS subtree root from which to begin search
   * @param exts Extensions of files to regard as potential data sources
   * @param sectName How to infer section title/header from filename
   * @return Collection of pairs in which first elem is section title/header to associate with any 
   *         question-and-answer instances parsed from the associated file, and the second element is a filepath
   */
  def seekData(rootdir: File, exts: NEL[String], sectName: File => String): List[(String, File)] = {
    require(rootdir.isDirectory, s"Not a directory: $rootdir")
    val applyName = (_: List[File]).map(f => sectName(f) -> f)
    exts.toList flatMap { ext => applyName.compose(getAllExt(ext))(rootdir) }
  }

}
