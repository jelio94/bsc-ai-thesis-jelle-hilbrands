package com.markblokpoel.lanag.adaptive.atoms

import com.markblokpoel.probability4scala.ConditionalDistribution
import com.markblokpoel.probability4scala.datastructures.BigNatural

/** Lexicon class
 *
 *  @param signals the possible signals
 *  @param referents the possible referents
 *  @param data The lexicon mappings from signal to referent
 */
case class Lexicon(signals: List[StringSignal], referents: List[StringReferent], data: List[List[Double]]) {
  require(data.nonEmpty, "data is empty")
  require(data.head.nonEmpty, "data mustn't contain non empty referents")
  require(data.length == signals.length,
    s"the lexicon data (len ${data.length}) must contain exactly the right number of signals  (len ${signals.length})")
  require({
    data.forall(_.length == data.head.length)
  }, "each signal should refer to the same number of referents")
  require({
    data.forall(_.length == referents.length)
  }, "each signal should refer to the correct number of referents")

  val vocabularySize: Int = data.length
  val contextSize: Int = data.head.length

  /*
  I need another constructor in this Lexicon class, one that allows for boolean value added to the fields.
  Lexicons that have a false value for certain signal/referent mappings can not be changed.

   */

  /** Normalising over rows in the Lexicon
   *
   * @return Lexicon with conditional probabilities over signals
   */
  def deltaL: ConditionalDistribution[StringReferent, StringSignal] = {
    val newData = data.map((conditionalProbabilities: List[Double]) => {
      val sum = conditionalProbabilities.sum
      conditionalProbabilities.map {
        case 0 => 0
        case x => x / sum
      }
    })

    val map: Map[(StringReferent, StringSignal), BigNatural]=
      (for(ri <- referents.indices; si <- signals.indices) yield {
        (referents(ri), signals(si)) -> BigNatural(newData(si)(ri))
      }).toMap

    ConditionalDistribution(referents.toSet, signals.toSet, map)
  }

  /** Normalising over columns in the Lexicon
   *
   * @return Lexicon with conditional probabilities over referents
   */
  def deltaS: ConditionalDistribution[StringSignal, StringReferent] = {
    val sumColumn = Array.fill(contextSize)(0.0)
    for (i <- 0 until vocabularySize) {
      for (j <- 0 until contextSize) {
        sumColumn(j) = sumColumn(j) + data(i)(j)
      }
    }
    val newData = data.map((conditionalProbabilities: List[Double]) => {
      (conditionalProbabilities zip sumColumn) map {
        case (_,0) => 0
        case (x,y) => x / y
      }
    })

    val map: Map[(StringSignal, StringReferent), BigNatural]=
      (for(ri <- referents.indices; si <- signals.indices) yield {
        (signals(si), referents(ri)) -> BigNatural(newData(si)(ri))
      }).toMap

    ConditionalDistribution(signals.toSet, referents.toSet, map)
  }

  /** Checks consistency of lexicon
   * Do all signals refer to at least 1 referent, and do all referent have at least 1 signal
   * that refers to it?
   * @return
   */
  def isConsistent: Boolean = {
    val signalsHaveAtLeastOneReferent = data.forall(_.exists(_ > 0))
    val referentsHaveAtLeastOneSignal = data.transpose.forall(_.exists(_ > 0))
    signalsHaveAtLeastOneReferent && referentsHaveAtLeastOneSignal
  }

  /**
    * Checks if an element in the lexicon is considered a fact, and that it thus must have a
    * signal/referent mapping of non-zero. Possible upgrade could be that no other signal/referent
    * mappings are allowed for that specific signal and that specific referent. Or maybe just the
    * referent or just the signal.
    * @param facts List of mappings that are considered facts in the lexicon by the agent.
    * @return Boolean: false if there is a fact that has a zero signal/referent mapping, true otherwise.
    */
  def isFactual(facts: List[List[Boolean]]): Boolean = {
    assert(facts.length == signals.length, "Facts list is not of same size as signals")
    assert(facts.forall( list => list.length == facts.head.length ), "Facts list is not all of same size")
    assert(facts.head.length == referents.length, "Facts list is not of same size as referents")
    var isOkay = true
    for (signalCounter <- signals.indices; referentCounter <- referents.indices) {
      if (isOkay) {
        if (facts(signalCounter)(referentCounter) && data(signalCounter)(referentCounter) == 0.0) {
          isOkay = false
        }
      }
    }
    isOkay
  }

  def getNeighborliness(other: Lexicon) : Double ={
    var neighborlinessValue = 0.0
    for (i <- signals.indices; j <- referents.indices) {
      if (data(i)(j) == other.data(i)(j)) {
        neighborlinessValue += 1.0
      }
    }
    neighborlinessValue / (signals.length * referents.length).toDouble
  }

  /** Finds the maximum value of a column
   *
   * @param column Column to find maximum in
   * @return the value and index of the maximum in the column
   */
  def maxIndexOfColumn(column: Int): Int = {
    // find maximum in column $column, and return max value and its index
    val columnValues = Array.ofDim[Double](vocabularySize)
    for(i <- 0 until vocabularySize) {
      columnValues(i) = data(i)(column)
    }
    //		TODO: this still returns the FIRST max value element, Hawkins' model uses random max value
    //		(columnValues.max, columnValues.indexOf(columnValues.max))
    columnValues.indexOf(columnValues.max)
  }

  def printLexicon(): Unit = {
    val sb = new StringBuilder
    for (referentIndex <- referents.indices) {
      sb.append("     %s\t".format(referents(referentIndex)))
    }
    sb.append("\n")
    for (signalIndex <- signals.indices) {
      sb.append("%s [ ".format(signals(signalIndex)))
      for (dataIndex <- data.head.indices) {
        sb.append("%.2f\t".format(data(signalIndex)(dataIndex)))
      }
      sb.setLength(sb.length - 1)
      sb.append(" ]\n")
    }
    println(sb.toString())
  }

  @Override
  override def toString: String = {
    val sb = new StringBuilder
    for (referentIndex <- referents.indices) {
      sb.append("     %s\t".format(referents(referentIndex)))
    }
    sb.append("\n")
    for (signalIndex <- signals.indices) {
      sb.append("%s [ ".format(signals(signalIndex)))
      for (dataIndex <- data.head.indices) {
        sb.append("%.2f\t".format(data(signalIndex)(dataIndex)))
      }
      sb.setLength(sb.length - 1)
      sb.append(" ]\n")
    }
    sb.toString()
  }
}

/** Creates all consistent lexicons
 *
 */
case object Lexicon {
  def allConsistentLexicons(signals: Set[StringSignal], referents: Set[StringReferent]): Set[Lexicon] = {
    val length1d = signals.size * referents.size

    def apl(curLength: Int): Set[List[Double]] = {
      if(curLength == length1d) Set(List.empty)
      else {
        val rest = apl(curLength+1)
        val left = rest.map(0.0 :: _)
        val right = rest.map(1.0 :: _)
        left ++ right
      }
    }

    apl(0)
      .map(lex1d => Lexicon(signals.toList, referents.toList, lex1d.sliding(referents.size, referents.size).toList))
      .filter(_.isConsistent)
  }

  /**
    * Extended version that takes a list of facts as well.
    * @param signals Set of strings that represent the signals.
    * @param referents Set of strings that represent the referents.
    * @param facts List of List of Boolean that represents in 2D structure what signal/referent mappings are facts.
    * @return A set of lexicons that are both consistent and factual
    */
  def allConsistentLexicons(signals: Set[StringSignal], referents: Set[StringReferent], facts: List[List[Boolean]]): Set[Lexicon] = {
    val length1d = signals.size * referents.size
    println(facts.size)
    println("Possible lexicons:\t\t%d".format(scala.math.pow(2.0, length1d.toDouble).toInt))
    def apl(curLength: Int): Set[List[Double]] = {
      if(curLength == length1d) Set(List.empty)
      else {
        val rest = apl(curLength+1)
        val left = rest.map(0.0 :: _)
        val right = rest.map(1.0 :: _)
        left ++ right
      }
    }

    apl(0)
      .map(lex1d => Lexicon(signals.toList, referents.toList, lex1d.sliding(referents.size, referents.size).toList))
      .filter(_.isConsistent)
  }
}