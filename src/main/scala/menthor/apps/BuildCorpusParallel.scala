package menthor.apps

import processing.parallel.Graph
import processing.parallel.Message
import processing.parallel.Substep
import processing.parallel.Vertex
import scala.collection.JavaConversions._
import scala.io.Source
import java.io.File
import scala.collection.mutable.HashMap
import menthor.util.FileUtils._
import scala.util.Random
import menthor.classifier.naivebayes.NaiveBayesTrainer
import menthor.classifier.naivebayes.NaiveBayesTrainerParallel
import menthor.classifier.maxent.MaxentTrainer
import menthor.classifier.maxent.MaxentTrainerParallel
import scala.collection.mutable.ListBuffer
import scala.util.logging.ConsoleLogger
import menthor.classifier.featureselector.IGFeatureSelector
import java.io.FileWriter
import gnu.trove.map.hash.TIntObjectHashMap
import gnu.trove.procedure.TObjectIntProcedure
import gnu.trove.procedure.TIntDoubleProcedure
import scala.collection.JavaConversions._
import scala.xml.XML
import scala.io.Source
import java.io.File
import scala.collection.mutable.HashMap
import menthor.util.FileUtils._
import scala.util.Random
import menthor.classifier.Classifier
import menthor.classifier.naivebayes._
import menthor.classifier.maxent._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.util.logging.ConsoleLogger
import menthor.classifier.featureselector.IGFeatureSelector
import benchmark.TicToc
import gnu.trove.map.hash.TObjectIntHashMap
import gnu.trove.map.hash.TIntDoubleHashMap

object BuildCorpusParallel extends TicToc {

  // dbpedia classes
  val categories = List("activity", "game", "sport", "anatomical structure", "artery", "bone", "brain", "embryology", "lymph", "muscle", "nerve", "vein", "asteroid", "award", "celestial body", "chemical substance", "chemical compound", "chemical element", "colour", "constellation", "currency", "database", "biological database", "device", "automobile engine", "weapon", "disease", "drug", "ethnic group", "event", "convention", "election", "film festival", "military conflict", "music festival", "space mission", "sports event", "football match", "grand prix", "mixed martial arts event", "olympics", "race", "soccer match", "soccer tournament", "womens tennis association tournament", "wrestling event", "year", "year in spaceflight", "flag", "food", "beverage", "galaxy", "gene", "government type", "holiday", "i m d b name", "ideology", "language", "legal case", "supreme court of the united states case", "mean of transportation", "aircraft", "automobile", "locomotive", "rocket", "ship", "space shuttle", "space station", "spacecraft", "music genre", "name", "given name", "surname", "olympic result", "organisation", "band", "broadcaster", "broadcast network", "radio station", "television station", "company", "airline", "law firm", "record label", "educational institution", "college", "library", "school", "university", "geopolitical organisation", "government agency", "legislature", "military unit", "non- profit organisation", "political party", "sports league", "american football league", "australian football league", "auto racing league", "baseball league", "basketball league", "bowling league", "boxing league", "canadian football league", "cricket league", "curling league", "cycling league", "field hockey league", "golf league", "handball league", "ice hockey league", "inline hockey league", "lacrosse league", "mixed martial arts league", "motorcycle racing league", "paintball league", "polo league", "radio controlled racing league", "rugby league", "soccer league", "soccer league season", "softball league", "speedway league", "tennis league", "videogames league", "volleyball league", "sports team", "american football team", "basketball team", "canadian football team", "hockey team", "soccer club", "national soccer club", "speedway team", "trade union", "person", "ambassador", "architect", "artist", "actor", "adult actor", "voice actor", "comedian", "comics creator", "musical artist", "writer", "astronaut", "athlete", "australian rules football player", "badminton player", "baseball player", "basketball player", "boxer", "cricketer", "cyclist", "figure skater", "formula one racer", "gaelic games player", "golf player", "gridiron football player", "american football player", "canadian football player", "ice hockey player", "martial artist", "nascar driver", "national collegiate athletic association athlete", "rugby player", "snooker player", "snooker champ", "soccer player", "tennis player", "volleyball player", "wrestler", "british royalty", "bull fighter", "celebrity", "chess player", "cleric", "cardinal", "christian bishop", "pope", "priest", "saint", "college coach", "criminal", "fictional character", "comics character", "journalist", "judge", "military person", "model", "monarch", "office holder", "philosopher", "playboy playmate", "poker player", "politician", "chancellor", "congressman", "deputy", "governor", "lieutenant", "mayor", "member of parliament", "president", "prime minister", "senator", "vice president", "vice prime minister", "referee", "royalty", "polish king", "scientist", "soccer manager", "person function", "place", "architectural structure", "building", "arena", "church", "historic building", "hospital", "hotel", "lighthouse", "museum", "restaurant", "shopping mall", "skyscraper", "stadium", "theatre", "infrastructure", "airport", "launch pad", "power station", "route of transportation", "bridge", "public transit system", "railway line", "road", "road junction", "tunnel", "railway tunnel", "road tunnel", "waterway tunnel", "station", "park", "historic place", "monument", "mountain pass", "natural place", "body of water", "lake", "stream", "canal", "river", "cave", "lunar crater", "mountain", "mountain range", "valley", "populated place", "administrative region", "atoll", "continent", "country", "island", "settlement", "city", "town", "village", "protected area", "site of special scientific interest", "ski area", "wine region", "world heritage site", "planet", "programming language", "project", "research project", "protein", "sales", "snooker world ranking", "species", "archaea", "bacteria", "eukaryote", "animal", "amphibian", "arachnid", "bird", "crustacean", "fish", "insect", "mammal", "mollusca", "reptile", "fungus", "plant", "club moss", "conifer", "cycad", "fern", "flowering plant", "grape", "ginkgo", "gnetophytes", "green alga", "moss", "tax", "work", "film", "musical", "musical work", "album", "single", "song", "eurovision song contest entry", "painting", "sculpture", "software", "video game", "television episode", "television show", "website", "written work", "book", "periodical literature", "academic journal", "magazine", "newspaper", "play")

  def getFiles(folder: String): Iterable[File] = {
    val files = new ListBuffer[File]

    forEachFileIn(new File(folder)) {
      f =>
        try {
          files += f
        } catch {
          case e: java.io.IOException =>
        }
    }

    files.toIterable
  }

  def main(args: Array[String]) {
    if (args.size < 3) {
      println("Please specify [wikipedia corpus path] [benchmark result file] [benchmark iteration]")
      exit
    }

    val corpusPath = args(0)
    val benchmarkResultFile = args(1)
    val benchmarkIterations = args(2).toInt
    
    val partitions = 8

    val files = getFiles(corpusPath)

    for (i <- 1 to benchmarkIterations + 1) {
      if (i == 1) {
    	process(files, partitions)
      } else {
        tic()

        process(files, partitions)

        toc("i_" + (i - 1))
      }
    }

    writeTimesLog(benchmarkResultFile)
  }

  def process(files: Iterable[File], partitions: Int) {
    val graph = new Graph[ListBuffer[Document]]

    for ((group, i) <- files.grouped(Math.ceil(files.size.toDouble / partitions).toInt).zipWithIndex) {
      val fileVertex = new FileVertex("group" + i, group)
      graph.addVertex(fileVertex)
    }

    println("Processing files")

    graph.start()
    graph.iterate(1)

    graph.terminate()

    println("Aggregating results")

    val collection = graph.vertices.map(_.value.toList).reduce(_ ++ _)

    println("Done.")
  }

  type Data = ListBuffer[Document]

  class FileVertex(label: String, files: Iterable[File])
    extends Vertex[Data](label, new ListBuffer[Document]) {

    def update(superstep: Int, incoming: List[Message[Data]]): Substep[Data] = {
      for (f <- files) {
        try {
          println(f)
          val xml = XML.loadFile(f)

          val id = (xml \\ "article" \ "header" \ "id").text
          val title = (xml \\ "article" \ "header" \ "title").text
          val body = (xml \\ "article" \ "bdy").text

          val dcategories = (xml \\ "article" \ "header" \ "categories" \ "category").flatMap { dc =>
            val text = dc.text.toLowerCase
            categories.filter(c => text.contains(c)).toSet
          }

          if (dcategories.size > 0) {
            val document = new Document(
              id + "_" + title,
              dcategories.distinct.toList,
              termFrequency(body, true))

            value += document
          }
        } catch {
          case e: org.xml.sax.SAXParseException =>
          case e: java.io.IOException =>
        }
      }
      List()
    }

    val stopWords = {
      val source = Source.fromURL(getClass.getResource("/english-stopwords.txt"))
      source.getLines().mkString(",").split(",").toSet
    }

    val index = new TObjectIntHashMap[String]()

    def addToIndex(term: String): Int = {
      val id = index.size + 1
      index.put(term, id)
      id
    }

    def termFrequency(text: String, extendIndex: Boolean): TIntDoubleHashMap = {
      val result = new TIntDoubleHashMap

      for (rawTerm <- text.split("(?m)\\s+")) {
        val term = rawTerm.trim.toLowerCase

        if (term.matches("^[\\w']+$") && term.size > 2 && !stopWords.contains(term)) {
          var id = index.get(term)

          if (extendIndex && id == index.getNoEntryValue()) {
            addToIndex(term)
          }

          if (id != index.getNoEntryValue()) {
            var count = result.get(id)
            result.put(id, count + 1)
          }
        }
      }

      result
    }

  }
}