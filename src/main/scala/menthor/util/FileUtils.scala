package menthor.util

import java.io.File

import scala.collection.JavaConversions._

/**
 * Provides utilities methods for working with files
 * 
 * @author Stanislav Peshterliev
 */
object FileUtils {
  
  /**
   * Iterate recursively over the files in directory 
   * 
   * @param file starting directory
   * @param op callback function executed for every file    
   */
  def forEachFileIn(file: File)(op: File => Unit) {
    if (file.isDirectory()) {
      file.list() foreach (x => forEachFileIn(new File(file, x))(op))
    } else {
      op(file)
    }
  }
}