import AssemblyKeys._

name := "parallelnlp"

version := "1.0-SNAPSHOT"

scalaVersion := "2.9.1"

seq(assemblySettings: _*)

test in assembly := {}

excludedFiles in assembly := { (bases: Seq[File]) =>
  bases flatMap { base =>
    (base / "META-INF" * "*").get collect {
      case f if f.getName.toUpperCase == "BCKEY.SF" => f
      case f if f.getName.toUpperCase == "BCKEY.DSA" => f
      case f if f.getName.toLowerCase == "license" => f
      case f if f.getName.toLowerCase == "manifest.mf" => f      
    }
  }}

resolvers ++= Seq(
  ScalaToolsSnapshots,
  DefaultMavenRepository,
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "ondex" at "http://ondex.rothamsted.bbsrc.ac.uk/nexus/content/groups/public",
  "OpenNLP Maven Repository" at "http://opennlp.sourceforge.net/maven2"  
)

libraryDependencies ++= Seq(
  "epfl" %% "libmenthor" % "0.1",
  "org.scalala" %% "scalala" % "1.0.0.RC2-SNAPSHOT",
  "org.apache.opennlp" % "opennlp-tools" % "1.5.2-incubating",
  "net.sf.trove4j" % "trove4j" % "3.0.2",
  "org.scalatest" %% "scalatest" % "1.6.1" % "test"
)
