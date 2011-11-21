package menthor.classifier

import scala.collection.mutable.HashMap

import menthor.processing.Graph
import menthor.processing.Message
import menthor.processing.Substep
import menthor.processing.Vertex
import menthor.util.MathUtils._
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.operators.Implicits._
import scalala.scalar._
import scalala.tensor.dense._
import scalala.tensor.mutable._
import scala.Math

/**
 * Maximum entropy classifier
 *
 * Uses logarithmic weights
 *
 * For more information see the tutorial available at http://nlp.stanford.edu/software/classifier.shtml
 * Christopher Manning and Dan Klein. 2003. Optimization, Maxent Models, and Conditional Estimation without Magic. Tutorial at HLT-NAACL 2003 and ACL 2003.
 *
 */
case class MaxentClassifier[C, S](classes: List[C], weights: Vector[Double], featureGenerator: FeatureGenerator[C, S]) extends Classifier[C,S] {
  override def classify(sample: S) : (C,Double) = probClassify(sample).maxBy(_._2)

  def probClassify(sample: S): Map[C, Double] = {
    val prob = new HashMap[C, Double]
    for (cls <- classes) {
      val featureset = featureGenerator.generate(cls, sample)

      var total = 0.0

      featureset.foreachPair { (id, value) =>
        total += weights(id) * value
      }

      prob.put(cls, total)
    }

    normalizeLogProbabilty(prob.toMap)
  }
}

object MaxentClassifier {
  val iterations = 100
  val k = 2
  
  /**
   * Sequentially train maximum entropy classifier, but before that train featureGenerator
   */
  def trainSequential[C, S](classes: List[C], samples: List[(C, S)], featureTrainer: FeatureTrainer[C, S]) = {
    val featureGenerator = featureTrainer.train(classes, samples)
    trainSequential[C, S](classes, samples, featureGenerator)
  }

  /**
   * Sequentially train maximum entropy classifier
   */
  def trainSequential[C, S](classes: List[C], samples: List[(C, S)], featureGenerator: FeatureGenerator[C, S]) = {
    val classifier = new MaxentClassifier[C, S](classes, DenseVector.ones[Double](featureGenerator.size), featureGenerator)

    val Cinv = 1.0 / featureGenerator.C //This controls the learning rate: higher Cinv (or lower C) gives faster learning.     

    val empiricalFeatureCounts = DenseVector.zeros[Double](featureGenerator.size)

    for ((cls, sample) <- samples) {
      empiricalFeatureCounts += featureGenerator.generate(cls, sample)
    }

    val logEmpiricalFeatureCounts = empiricalFeatureCounts.map(log2)

    val estimatedFeatureCounts = DenseVector.zeros[Double](featureGenerator.size)

    for (n <- 1 to iterations) {
      println("Iteration: " + n)

      estimatedFeatureCounts(0 to estimatedFeatureCounts.size - 1) := 0.0

      for ((_, sample) <- samples) {
        val dist = classifier.probClassify(sample)
        for ((distcls, prob) <- dist) {
          estimatedFeatureCounts += (featureGenerator.generate(distcls, sample) * prob)
        }
      }

      val logEstimatedFeatureCounts = estimatedFeatureCounts.map(log2)

      val diff = (logEmpiricalFeatureCounts - logEstimatedFeatureCounts) * Cinv
      classifier.weights += diff

      println("Weights: " + classifier.weights)
    }

    classifier
  }

  /**
   * Parallel train maximum entropy classifier, but before that train featureGenerator
   */
  def trainParallel[C, S](classes: List[C], samples: List[(C, S)], featureTrainer: FeatureTrainer[C, S]) = {
    val featureGenerator = featureTrainer.train(classes, samples)
    trainParallel[C, S](classes, samples, featureGenerator)
  }

  /**
   * Parallel train maximum entropy classifier
   */
  def trainParallel[C, S](classes: List[C], samples: List[(C, S)], featureGenerator: FeatureGenerator[C, S]) = {
    val classifier = new MaxentClassifier[C, S](classes, DenseVector.ones[Double](featureGenerator.features.size), featureGenerator)

    val graph = new Graph[ProcessingResult]

    val masters = (for (i <- 0 to k - 1) yield new SampleVertex[C, S]("master" + i, classes.first, samples.first._2, classifier, i, null, true)).toList

    for (master <- masters) {
      master.masters = masters
      graph.addVertex(master)
    }

    for ((group, i) <- samples.grouped(Math.ceil(samples.size.toDouble / k).toInt).zipWithIndex) {
      for ((c, sample) <- group) {
        val vertex = new SampleVertex(sample.toString, c, sample, classifier, i, masters, false)
        graph.addVertex(vertex)
        vertex.connectTo(masters(i))
        masters(i).connectTo(vertex)
      }
    }

    graph.start()
    graph.iterate(iterations * 7)

    graph.terminate()

    graph.vertices.first.asInstanceOf[SampleVertex[C, S]].getClassifer
  }
}

class SampleVertex[C, S](label: String, cls: C, sample: S, classifier: MaxentClassifier[C, S], group: Int, var _masters: List[SampleVertex[C, S]], isMaster: Boolean)
  extends Vertex[ProcessingResult](label, new ProcessingResult) {

  def getClassifer = classifier  
  def numVertices = graph.vertices.size
  
  def masters_= (m: List[SampleVertex[C, S]]) {
    _masters = m
  }
  
  def masters  = _masters

  val featureGenerator = classifier.featureGenerator
  val Cinv = 1.0 / featureGenerator.C

  var logEmpiricalFeatureCounts: Vector[Double] = _
  var logEstimatedFeatureCounts: Vector[Double] = _

  def update(): Substep[ProcessingResult] = {
    {
      if (superstep == 0 && !isMaster) {
        println("step 1 : " + superstep)
        value.empiricalFeatureCount = featureGenerator.generate(cls, sample)

        for (neighbor <- masters) yield Message(this, neighbor, this.value)
      } else {
        List()
      }
    } then {
      if (superstep == 1 && isMaster) {
        println("step 2 : " + superstep)
        val empiricalFeatureCounts = incoming.map(_.value.empiricalFeatureCount).reduce { (x, y) => x + y }
        logEmpiricalFeatureCounts = empiricalFeatureCounts.map(log2)
        value.logEmpiricalFeatureCounts = logEmpiricalFeatureCounts
        for (neighbor <- neighbors) yield Message(this, neighbor, this.value)
      } else {
        List()
      }
    } then {
      if (superstep == 2 && !isMaster) {
        println("step 3 : " + superstep)
        logEmpiricalFeatureCounts = incoming.first.value.logEmpiricalFeatureCounts
        value.logEmpiricalFeatureCounts = logEmpiricalFeatureCounts
      }
      List()
    } then {
      if (!isMaster) {
        println("Iteration: " + superstep)

        val estimatedFeatureCount = DenseVector.zeros[Double](featureGenerator.size)

        val dist = classifier.probClassify(sample)

        for ((distcls, prob) <- dist) {
          estimatedFeatureCount += (featureGenerator.generate(distcls, sample) * prob)
        }

        value.estimatedFeatureCount = estimatedFeatureCount
        for (neighbor <- neighbors) yield Message(this, neighbor, this.value)
      } else {
        List()
      }
    } then {
      if (isMaster) {
        println("step 5 : " + superstep)
        val estimatedFeatureCounts = incoming.map(_.value.estimatedFeatureCount).reduce { (x, y) => x + y }

        logEstimatedFeatureCounts = estimatedFeatureCounts.map(log2)

        val diff = (logEmpiricalFeatureCounts - logEstimatedFeatureCounts) * Cinv
        classifier.weights += diff

        println("Weights: " + classifier.weights)

        value.weights = classifier.weights
        for (master <- masters) yield Message(this, master, this.value)
      } else {
        List()
      }
    } then {
      if (isMaster) {
        println("step 6 : " + superstep)
        val weights = incoming.map(_.value.weights).reduce { (x, y) => x + y }
        for (neighbor <- neighbors) yield Message(this, neighbor, this.value)
      } else {
        List()
      }
    } then {
      if (!isMaster) {
        println("step 7 : " + superstep)
        val weights = incoming.first.value.weights
        classifier.weights(0 to classifier.weights.size - 1) := weights
        List()
      } else {
        List()
      }
    }
  }
}

case class ProcessingResult {
  var empiricalFeatureCount: Vector[Double] = _

  var estimatedFeatureCount: Vector[Double] = _

  var logEmpiricalFeatureCounts: Vector[Double] = _

  var weights: Vector[Double] = _
}