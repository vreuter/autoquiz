object InteractiveTemplateQA {

  interp.load.ivy("org.typelevel" %% "cats-core" % "2.1.1")
  import java.io.{ BufferedWriter, File, FileWriter }
  import cats.Monoid
  val circeVersion = "0.13.0"
  Seq("circe-core") foreach { circePkg => interp.load.ivy("io.circe" %% circePkg % circeVersion) }
  import io.circe._, io.circe.syntax._
  
  def buildEmpty[C : Encoder : Monoid](n: Int): List[Json] = 
    List.fill(n){ Json.obj("Q" -> "".asJson, "A" -> implicitly[Monoid[C]].empty.asJson) }

  def stubQAFile(numSingleAns: Int, numListAns: Int)(f: File): Unit = {
    require(!f.exists, s"Output target already exists: $f")
    val (singleQAs, listQAs) = {
      import cats.instances.string._, cats.instances.list._
      buildEmpty[String](numSingleAns) -> buildEmpty[List[String]](numListAns)
    }
    (singleQAs ::: listQAs) match {
      case Nil => ()
      case qas => {
        val w = new BufferedWriter(new FileWriter(f))
        try { w.write(qas.asJson.spaces2); w.newLine() } finally { w.close() }
      }
    }
  }

  val testFile = new File("/home/vr/test-QA-template.json")
  stubQAFile(3, 3)(testFile)

}
