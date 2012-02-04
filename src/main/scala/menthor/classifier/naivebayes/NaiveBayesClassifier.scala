package menthor.classifier
package naivebayes

import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.operators.Implicits._
import scalala.scalar._
import scalala.tensor.dense._
import scalala.tensor.mutable._
import scala.Math
import scala.collection.mutable.HashMap
import menthor.util.ProbabilityDistribution
import gnu.trove.procedure.TIntDoubleProcedure
import gnu.trove.function.TDoubleFunction

/**
 * Naive Bayes classifier, given a model, determines the class of sample using the Bayes's rule.
 * The probabilities are calculated in log space.
 * 
 * For more information @see the technical report.
 * 
 * @author Stanislav Peshterliev
 */
class NaiveBayesClassifier[C, S <: Sample](val model: NaiveBayesModel[C, S]) extends Classifier[C, S] {
  val mu = 0.1

  val prior = model.classSamplesFreqDistr.toMap
  prior.transformValues(new TDoubleFunction {
    override def execute(value: Double) : Double = {
      Math.log(value / model.classSamplesFreqDistr.total)
    }
  })
  
  val cache = new HashMap[(C, Feature), Double]

  override def probClassify(sample: S): ProbabilityDistribution[C] = {
    val prob = new HashMap[C, Double]

    for (cls <- model.classes) {
      var total: Double = prior.get(cls)

      model.encode(sample).forEachEntry(new TIntDoubleProcedure {
        override def execute(feature: Int, value: Double): Boolean = {
          val p = cache.get((cls, feature)) match {
            case Some(_p) => _p
            case None => {
              val p1 = model.classFeatureFreqDistr(cls).count(feature) + (mu * (model.featureFreqDistr.frequency(feature)))
              val p2 = model.classFeatureFreqDistr(cls).total + mu
              val _p = (Math.log(p1) - Math.log(p2))
              cache.put((cls, feature), _p)
              _p
            }
          }
          total += value * p
          true
        }
      })

      prob.put(cls, total)
    }

    new ProbabilityDistribution(prob.toMap, true)
  }
}