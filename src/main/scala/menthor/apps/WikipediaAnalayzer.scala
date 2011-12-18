package menthor.apps

import xml.{ PrettyPrinter, XML }
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
import scala.util.matching.Regex

object WikipediaAnalayzer {
  def main(args: Array[String]) {
//    if (args.size < 1) {
//      println("Please specify [wikipedia corpus path]")
//      exit
//    }
//
//    val folder = args.first
//
////    val collection = new ListBuffer[Document]
//    
//    val allCategories = new HashMap[Category, Int]
//
//    forEachFileIn(new File(folder)) {
//      f =>
//        println(f)
//        
//        val xml = XML.loadFile(f)
//
//        //val id = (xml \\ "article" \ "header" \ "id").text
//        //val title = (xml \\ "article" \ "header" \ "title").text
//        val categories = (xml \\ "article" \ "header" \ "categories" \ "category").map(_.text)
//        //val categories = (xml \\ "article" \ "bdy" \ "categories" \ "category").map(_.text)
//        //val body = (xml \\ "article" \ "bdy").text
//
//        for (c <- categories) {
//          allCategories.put(c, allCategories.getOrElse(c,0) + 1)
//        }
//    }
//    
//    allCategories.toList.sort((e1, e2) => (e1._2 compareTo e2._2) > 0).slice(0,100).foreach(println)
//    
        
    val xml = XML.loadFile(new File("/home/stanvp/workspace/semesterproject/classes.html"))
    
    val regex = new Regex("""([A-Z])""")
    
    
    (xml \\ "a").foreach { a =>
      a.attribute("name") match {
        case Some(name) => println(regex.replaceAllIn(name.text, m => " " + m.group(1)).trim.toLowerCase)
        case None => 
      }
    }
    
  }
}