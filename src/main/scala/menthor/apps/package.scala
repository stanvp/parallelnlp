package menthor

import scala.io.Source
import java.io.File
import scala.collection.mutable.ListBuffer
import gnu.trove.map.hash.TIntDoubleHashMap

package object apps {
  type Category = String
  type Feature = Int

  def loadFeatures(path: String): List[Feature] = {
    val lines = Source.fromFile(new File(path)).getLines()

    val features = new ListBuffer[Feature]

    for (line <- lines) {
      val parts = line.split("""\|""").map(_.trim)
      val id = parts(0).toInt
      val value = parts(1)

      Analyzer.index.put(value, id)

      features += id
    }

    features.toList
  }

  def loadCorpus(file: String): Iterable[Document] = {
    val collection = new ListBuffer[Document]

    Source.fromFile(file).getLines().foreach {
      line =>
        val parts = line.split("""\|""").map(_.trim)
        val termFrequency = new TIntDoubleHashMap

        parts(2).split(" ").foreach { f =>
          val freq = f.split(":").map(_.trim)

          if (freq.size >= 2) {
            termFrequency.put(freq(0).toInt, freq(1).toDouble)
          }
        }

        val document = new Document(
          parts(1),
          parts(0).split(",").map(_.trim).toList,
          termFrequency)

        collection += document
    }

    collection.toIterable
  }
}