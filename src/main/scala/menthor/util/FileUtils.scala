package menthor.util
import java.io.File

import scala.collection.JavaConversions._

object FileUtils {
  def forEachFileIn(file: File)(op: File => Unit) {
    if (file.isDirectory()) {
      file.list() foreach (x => forEachFileIn(new File(file, x))(op))
    } else {
      op(file)
    }
  }

  def fileStream[T](file: File)(op: File => Option[T]) : Stream[T] = {
    if (file.isDirectory()) {
      file.list().toStream.flatMap { (x => fileStream[T](new File(file, x))(op)) }
    } else {
      op(file) match {
        case Some(t) => Stream(t)
        case None => Stream.empty
      }
    }
  }
}