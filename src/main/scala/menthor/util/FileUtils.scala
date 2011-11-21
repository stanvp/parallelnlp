package menthor.util
import java.io.File

object FileUtils {
  def forEachFileIn(file: File)(op: File => Unit) {
    if (file.isDirectory()) {
      file.list() foreach (x => forEachFileIn(new File(file, x))(op))
    } else {
      op(file)
    }
  }
}