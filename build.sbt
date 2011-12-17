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
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "ondex" at "http://ondex.rothamsted.bbsrc.ac.uk/nexus/content/groups/public"
)

libraryDependencies ++= Seq(
  "epfl" %% "libmenthor" % "0.1",
  "org.scalala" %% "scalala" % "1.0.0.RC2-SNAPSHOT",
  "org.scalatest" %% "scalatest" % "1.6.1" % "test"
)
