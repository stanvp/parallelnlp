package menthor

import scala.io.Source
import java.io.File
import scala.collection.mutable.ListBuffer

package object apps {
	type Category = String
	type Feature = Int
	
	def loadFeatures(path: String) : List[Feature] = {
	  
	  val lines = Source.fromFile(new File(path)).getLines()
	  
	  val features = new ListBuffer[Feature]
	  
	  for (line <- lines) {
		  val parts = line.split(" ")
		  val id = Analyzer.addToIndex(parts.init.mkString(" "))
		  features += id
	  }
	  
	  features.toList
	}
}