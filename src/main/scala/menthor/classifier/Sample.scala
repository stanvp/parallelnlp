package menthor.classifier

import gnu.trove.map.hash.TIntDoubleHashMap

/**
 * Sample is represented as feature id to feature value mapping
 * 
 * @author Stanislav Peshterliev
 */
trait Sample {
  /**
   * Features are mappings from int to double
   */  
  val features: TIntDoubleHashMap
  
  /**
   * Total number of features
   */
  val total: Double
}