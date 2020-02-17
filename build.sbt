name := "autoquiz"
version := "0.0.1-SNAPSHOT"
scalaVersion := "2.12.9"
organization := "vreuter"

assemblyJarName in assembly := s"${name.value}_v${version.value}.jar"
publishTo := Some(Resolver.file(s"${name.value}",  new File(Path.userHome.absolutePath + "/.m2/repository")))

/* More runtime-y stuff */
libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.1"
//libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
//libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"

/* Core abstractions */
libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
libraryDependencies += "org.typelevel" % "mouse_2.12" % "0.23"

// JSON encoding, decoding and rendering
val circeVersion = "0.12.3"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

/* Java and compiler options */
scalacOptions ++= Seq("-deprecation", "-feature", "-language:higherKinds", "-Ypartial-unification")
//javaOptions += "-Xmx4G"

/* Testing tools, or at least only used in tests */
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
libraryDependencies += "org.scalamock" %% "scalamock" % "4.1.0" % "test"

/* ScalaTest options */
testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oS")    // o for stdout, S for "short" stack trace; "F" for full
parallelExecution in Test := false                                        // Run tests serially for more intelligible exec output.

// Autogenerate source code as package autoquizinfo, with object BuildInfo, to access version; useful for GFF provenance.
lazy val root  = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(buildInfoKeys := Seq[BuildInfoKey](name, version), buildInfoPackage := "autoquizinfo")

// Enable quitting a run without quitting sbt.
cancelable in Global := true

// Ignore certain file patterns for the build.
excludeFilter in unmanagedSources := HiddenFileFilter || "Interactive*.scala" || ( new FileFilter { def accept(f: File) = Set("reserved")(f.getParentFile.getName) } )
