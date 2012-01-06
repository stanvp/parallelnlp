package menthor.apps

import scala.collection.JavaConversions._
import scala.xml.XML
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
import scala.collection.mutable.HashSet
import scala.util.logging.ConsoleLogger
import menthor.classifier.featureselector.IGFeatureSelector

object WikipediaClassifier {

  // dbpedia classes
  val categories = List("activity", "game", "sport", "anatomical structure", "artery", "bone", "brain", "embryology", "lymph", "muscle", "nerve", "vein", "asteroid", "award", "celestial body", "chemical substance", "chemical compound", "chemical element", "colour", "constellation", "currency", "database", "biological database", "device", "automobile engine", "weapon", "disease", "drug", "ethnic group", "event", "convention", "election", "film festival", "military conflict", "music festival", "space mission", "sports event", "football match", "grand prix", "mixed martial arts event", "olympics", "race", "soccer match", "soccer tournament", "womens tennis association tournament", "wrestling event", "year", "year in spaceflight", "flag", "food", "beverage", "galaxy", "gene", "government type", "holiday", "i m d b name", "ideology", "language", "legal case", "supreme court of the united states case", "mean of transportation", "aircraft", "automobile", "locomotive", "rocket", "ship", "space shuttle", "space station", "spacecraft", "music genre", "name", "given name", "surname", "olympic result", "organisation", "band", "broadcaster", "broadcast network", "radio station", "television station", "company", "airline", "law firm", "record label", "educational institution", "college", "library", "school", "university", "geopolitical organisation", "government agency", "legislature", "military unit", "non- profit organisation", "political party", "sports league", "american football league", "australian football league", "auto racing league", "baseball league", "basketball league", "bowling league", "boxing league", "canadian football league", "cricket league", "curling league", "cycling league", "field hockey league", "golf league", "handball league", "ice hockey league", "inline hockey league", "lacrosse league", "mixed martial arts league", "motorcycle racing league", "paintball league", "polo league", "radio controlled racing league", "rugby league", "soccer league", "soccer league season", "softball league", "speedway league", "tennis league", "videogames league", "volleyball league", "sports team", "american football team", "basketball team", "canadian football team", "hockey team", "soccer club", "national soccer club", "speedway team", "trade union", "person", "ambassador", "architect", "artist", "actor", "adult actor", "voice actor", "comedian", "comics creator", "musical artist", "writer", "astronaut", "athlete", "australian rules football player", "badminton player", "baseball player", "basketball player", "boxer", "cricketer", "cyclist", "figure skater", "formula one racer", "gaelic games player", "golf player", "gridiron football player", "american football player", "canadian football player", "ice hockey player", "martial artist", "nascar driver", "national collegiate athletic association athlete", "rugby player", "snooker player", "snooker champ", "soccer player", "tennis player", "volleyball player", "wrestler", "british royalty", "bull fighter", "celebrity", "chess player", "cleric", "cardinal", "christian bishop", "pope", "priest", "saint", "college coach", "criminal", "fictional character", "comics character", "journalist", "judge", "military person", "model", "monarch", "office holder", "philosopher", "playboy playmate", "poker player", "politician", "chancellor", "congressman", "deputy", "governor", "lieutenant", "mayor", "member of parliament", "president", "prime minister", "senator", "vice president", "vice prime minister", "referee", "royalty", "polish king", "scientist", "soccer manager", "person function", "place", "architectural structure", "building", "arena", "church", "historic building", "hospital", "hotel", "lighthouse", "museum", "restaurant", "shopping mall", "skyscraper", "stadium", "theatre", "infrastructure", "airport", "launch pad", "power station", "route of transportation", "bridge", "public transit system", "railway line", "road", "road junction", "tunnel", "railway tunnel", "road tunnel", "waterway tunnel", "station", "park", "historic place", "monument", "mountain pass", "natural place", "body of water", "lake", "stream", "canal", "river", "cave", "lunar crater", "mountain", "mountain range", "valley", "populated place", "administrative region", "atoll", "continent", "country", "island", "settlement", "city", "town", "village", "protected area", "site of special scientific interest", "ski area", "wine region", "world heritage site", "planet", "programming language", "project", "research project", "protein", "sales", "snooker world ranking", "species", "archaea", "bacteria", "eukaryote", "animal", "amphibian", "arachnid", "bird", "crustacean", "fish", "insect", "mammal", "mollusca", "reptile", "fungus", "plant", "club moss", "conifer", "cycad", "fern", "flowering plant", "grape", "ginkgo", "gnetophytes", "green alga", "moss", "tax", "work", "film", "musical", "musical work", "album", "single", "song", "eurovision song contest entry", "painting", "sculpture", "software", "video game", "television episode", "television show", "website", "written work", "book", "periodical literature", "academic journal", "magazine", "newspaper", "play")

  def load(folder: String, extendIndex: Boolean): Iterable[Document] = {
    val collection = new ListBuffer[Document]

    forEachFileIn(new File(folder)) {
      f =>
        try {
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
            Analyzer.termFrequency(body, extendIndex))

          collection += document
        }
        } catch {
          case e : org.xml.sax.SAXParseException => 
          case e : java.io.IOException => 
        }
    }
    collection.toIterable
  }  

  def main(args: Array[String]) {
    if (args.size < 5) {
      println("Please specify [algorithm] [traning mode] [wikipedia train corpus file] [wikipedia test corpus path] [features file] [evaluation]")
      println("algorithm can be: maxent or naivebayes")
      println("traning mode can be: parallel or sequential")
      println("evaluation be: true or false, default is false")
      exit
    }
    
    val algorithm = args.first
    val traningMode = args(1)
    val trainCorpus = args(2)
    val testCorpus = args(3)
    val featuresFile = args(4)
    val evaluation = if (args.length < 5) false else args(5).toBoolean    

    val features = loadFeatures(featuresFile)   

    val train = loadCorpus(trainCorpus)

    val samples = train.flatMap(d =>
      d.categories.map(c => (c, d)))

    val trainer = algorithm match {
      case "maxent" =>
        traningMode match {
          case "parallel" => new MaxentTrainerParallel[Category, Document](5, features) with ConsoleLogger
          case "sequential" => new MaxentTrainer[Category, Document](features) with ConsoleLogger
          case _ => throw new IllegalArgumentException("Illegal traning mode, choose parallel or sequential")
        }
      case "naivebayes" =>
        traningMode match {
          case "parallel" => new NaiveBayesTrainerParallel[Category, Document](20, features) with ConsoleLogger
          case "sequential" => new NaiveBayesTrainer[Category, Document](features) with ConsoleLogger
          case _ => throw new IllegalArgumentException("Illegal traning mode, choose parallel or sequential")
        }
      case _ => throw new IllegalArgumentException("Illegal algorithm, choose maxent or naivebayes")
    }

    val classifier = trainer.train(categories, samples)

    if (evaluation == true) {

      println("Evaluation")

      val test = load(testCorpus, false)

      var success = 0
      for (d <- test) {
        val r = classifier.classify(d)

        if (d.categories.contains(r._1)) {
          success += 1
        }
      }

      println("Total: " + test.size)
      println("Success: " + success)
      println("Percent: " + ((success / test.size.toFloat) * 100) + " %")
    }

    println("Done.")
  }
}