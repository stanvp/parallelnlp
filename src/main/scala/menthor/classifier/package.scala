package menthor

package object classifier {
  type Feature = Int

  def timed[R](blockName: String)(block: => R) = {
    val start = System.currentTimeMillis
    val result = block
    println("Block (" + blockName + ") took " + (System.currentTimeMillis - start) + "ms.")
    result
  }

}