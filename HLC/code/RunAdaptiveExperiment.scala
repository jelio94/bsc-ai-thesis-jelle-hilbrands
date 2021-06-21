package com.markblokpoel.lanag.adaptive

import com.markblokpoel.probability4scala.datastructures.BigNatural
import com.markblokpoel.probability4scala.Implicits._

import scala.util.Random
import java.io._
import com.markblokpoel.lanag.adaptive.agents.{Initiator, Responder}
import com.markblokpoel.lanag.adaptive.atoms._
import com.markblokpoel.lanag.adaptive.storage.InteractionData


/** Runs the non-ostensive simulations
 *
 *  Sets up the settings to run simulations
 *  Runs the simulations
 *  Saves datafiles of simulations automatically
 */
object RunAdaptiveExperiment extends App{

	val signals = Set("S1", "S2", "S3", "S4").map(StringSignal)
	val referents = Set("R1", "R2", "S4").map(StringReferent)

	/***** Here be my code insertion *****/

	println("Possible lexicons:\t\t\t\t\t%d".format(scala.math.pow(2.0, signals.size.toDouble * referents.size.toDouble).toInt))
	// no facts
	val facts0 = List(List(false, false, false), List(false, false, false), List(false, false, false), List(false, false, false))
	// 1 fact, upper left is fixed
	val facts1 = List(List(true, false, false), List(false, false, false), List(false, false, false), List(false, false, false))
	// 2 facts, upper left and bottom middle are fixed
	val facts2 = List(List(true, false, false), List(false, false, false), List(false, false, false), List(false, true, false))
	// 2 facts, bottom middle and bottom right are fixed
	val facts3 = List(List(false, false, false), List(false, false, false), List(false, false, false), List(false, true, true))

	util.Random.setSeed(1000L)

	val nrPairs = 500
	val maxTurns = 6
	val nrRounds = 6
	val entropyThreshold = 0.8.toBigNatural
	val order = 1
	val costs = 0.toBigNatural
	val beta = 5.toBigNatural
	val distribution = 0.5
	/** Options for the gamma and facts */
	val gammaOptions = List(0.9, 0.7, 0.5, 0.0)
	val factsOptions = List(facts3, facts2, facts1, facts0)
	val factsOptionsAsText = List(3, 2, 1, 0)

	val consistentLexicons = Lexicon.allConsistentLexicons(signals, referents)
	val allLexicons = consistentLexicons
	println("Consistent lexicons:\t\t\t\t%d".format(consistentLexicons.size))

	for (gamma <- gammaOptions){
		var counter = 0
		for (facts <- factsOptions) {

				/***** Code insertion, adding symmetry **** */

			val factualLexicons = consistentLexicons.filter(_.isFactual(facts))
			println("Facts:\t%d\t\t\t\t\tGamma:\t%.2f".format(factsOptionsAsText(counter), gamma))
			println("Factual lexicons:\t\t\t\t\t%d".format(factualLexicons.size))
			println("Difference:\t\t\t\t\t\t\t%d".format(consistentLexicons.size - factualLexicons.size))
			println("Percent decrease:\t\t\t\t\t%.2f".format((1 - factualLexicons.size.toDouble / consistentLexicons.size.toDouble) * 100))

			var lexiconPriorInitiator = factualLexicons.binomialDistribution(BigNatural(distribution))
			var lexiconPriorResponder = factualLexicons.binomialDistribution(BigNatural(1 - distribution))

			// Get most probable lexicon for neighborliness comparison
			val initiatorMostProbableLexicon = lexiconPriorInitiator.argMax
			val responderMostProbableLexicon = lexiconPriorResponder.argMax

			// Get the neighborliness of all lexicons to argMax
			val initiatorNeighborliness = for (l <- factualLexicons) yield (l, initiatorMostProbableLexicon.getNeighborliness(l))
			val responderNeighborliness = for (l <- factualLexicons) yield (l, responderMostProbableLexicon.getNeighborliness(l))

			// Get the set of lexicons that are closest by with cutoff gamma
			val initiatorNeighboringLexicons = initiatorNeighborliness.filter(_._2 > gamma).map(_._1)
			val responderNeighboringLexicons = responderNeighborliness.filter(_._2 > gamma).map(_._1)

			// Make a new distribution over the Neighboring set of lexicons
			lexiconPriorInitiator = initiatorNeighboringLexicons.binomialDistribution(BigNatural(distribution))
			lexiconPriorResponder = responderNeighboringLexicons.binomialDistribution(BigNatural(1 - distribution))

			println("Neighbouring initiator lexicons:\t%d".format(initiatorNeighboringLexicons.size))
			println("Neighbouring responder lexicons:\t%d".format(responderNeighboringLexicons.size))
			println("Difference:\t\t\t\t\t\t\t%d".format(factualLexicons.size - initiatorNeighboringLexicons.size))
			println("Percent decrease:\t\t\t\t\t%.2f\n\n".format((1 - initiatorNeighboringLexicons.size.toDouble / factualLexicons.size.toDouble) * 100))

			/***** End code insertion *****/

			val firstReferent = referents.toList(Random.nextInt(referents.size))
			val interactionsParallelized = (for (pair <- 0 until nrPairs) yield {
				val initiator = Initiator(
					order,
					signals,
					referents,
					firstReferent,
					None,
					List.empty,
					initiatorNeighboringLexicons,
					lexiconPriorInitiator,
					signals.uniformDistribution,
					referents.uniformDistribution,
					signals.map(_ -> costs).toMap,
					beta,
					entropyThreshold)

				val responder = Responder(
					order,
					signals,
					referents,
					List.empty,
					responderNeighboringLexicons,
					lexiconPriorResponder,
					signals.uniformDistribution,
					referents.uniformDistribution,
					signals.map(_ -> costs).toMap,
					beta,
					entropyThreshold)

				val interaction = AdaptiveInteraction(referents, initiator, responder, maxTurns, nrRounds).toList

				val parameters = s"a$nrPairs-${pair}_b${beta}_d${distribution}_f${factsOptionsAsText(counter)}_n${gamma}"

				val pwt = new PrintWriter(new File("output/datafiles/results_turns_" + parameters + ".csv"))
				pwt.println("pair;round;turn;initiatorIntention;initiatorSignal;responderInference;responderSignal;entropyInitiatorListen;entropyResponderListen;entropyInitiatorLexicon;entropyResponderLexicon;KLDivItoR;KLDivRtoI")
				//				val rounds: List[InteractionData] = interaction
				interaction.indices.foreach(round => {
					val roundData: InteractionData = interaction(round)
					val turn0i = roundData.initialInitiatorData
					val turn0r = roundData.responderData.head
					pwt.println(s"$pair;$round;0;${turn0i.intendedReferent};${turn0i.signal.toString};${turn0r.inferredReferent};${turn0r.signal.toString};NA;${turn0r.listenEntropy};${turn0i.lexiconEntropy};${turn0r.lexiconEntropy};${roundData.klInitItoR};${roundData.klInitRtoI}")

					val restTurnsI = roundData.initiatorData
					val restTurnsR = roundData.responderData.tail
					for (turn <- 0 until math.max(restTurnsI.size, restTurnsR.size)) {
						if (restTurnsI.isDefinedAt(turn) && restTurnsR.isDefinedAt(turn)) {
							val turni = restTurnsI(turn)
							val turnr = restTurnsR(turn)
							val turnklItoR = roundData.klInitiatorToResponder(turn)
							val turnklRtoI = roundData.klResponderToInitiator(turn)
							pwt.println(s"$pair;$round;$turn;${turni.intendedReferent};${turni.signal.toString};${turnr.inferredReferent};${turnr.signal.toString};${turni.listenEntropy};${turnr.listenEntropy};${turni.lexiconEntropy};${turnr.lexiconEntropy};$turnklItoR;$turnklRtoI")
						} else {
							val turni = restTurnsI(turn)
							pwt.println(s"$pair;$round;$turn;${turni.intendedReferent};${turni.signal.toString};NA;NA;${turni.listenEntropy};NA;${turni.lexiconEntropy};NA;NA;NA")
						}
					}
				})
				pwt.flush()

				val pwr = new PrintWriter(new File("output/datafiles/results_rounds_" + parameters + ".csv"))
				pwr.println("pair;round;nrTurns;success")
				//					val pair: Int = intPair
				val rounds: List[InteractionData] = interaction
				rounds.indices.foreach(round => {
					val nrTurns = rounds(round).initiatorData.length + 1
					val success = rounds(round).initialInitiatorData.intendedReferent == rounds(round).responderData.last.inferredReferent
					pwr.println(s"$pair;$round;$nrTurns;$success")
				})
				pwr.flush()

				val pwc = new PrintWriter(new File("output/datafiles/config_" + parameters + ".csv"))
				pwc.println("agentPairs;maxTurns;roundsPlayed;beta;entropyThreshold;order;costs;initiatorDistribution;responderDistribution")
				pwc.println(s"$nrPairs;$maxTurns;$nrRounds;$beta;$entropyThreshold;$order;$costs;$distribution;${1 - distribution}")
				pwc.flush()

				pair -> interaction
			}).toParArray

			val allData = interactionsParallelized.map(interaction => interaction._1 -> interaction._2)
				.toList

			val parameters = s"a${nrPairs}_b${beta}_d${distribution}_f${factsOptionsAsText(counter)}_n${gamma}"

			val pwt = new PrintWriter(new File("output/datafiles/results_turns_" + parameters + ".csv"))
			pwt.println("pair;round;turn;initiatorIntention;initiatorSignal;responderInference;responderSignal;entropyInitiatorListen;entropyResponderListen;entropyInitiatorLexicon;entropyResponderLexicon;KLDivItoR;KLDivRtoI")
			allData.foreach(pairData => {
				val pair: Int = pairData._1
				val rounds: List[InteractionData] = pairData._2
				rounds.indices.foreach(round => {
					val roundData: InteractionData = rounds(round)
					val turn0i = roundData.initialInitiatorData
					val turn0r = roundData.responderData.head
					pwt.println(s"$pair;$round;0;${turn0i.intendedReferent};${turn0i.signal.toString};${turn0r.inferredReferent};${turn0r.signal.toString};NA;${turn0r.listenEntropy};${turn0i.lexiconEntropy};${turn0r.lexiconEntropy};${roundData.klInitItoR};${roundData.klInitRtoI}")

					val restTurnsI = roundData.initiatorData
					val restTurnsR = roundData.responderData.tail
					for (turn <- 0 until math.max(restTurnsI.size, restTurnsR.size)) {
						if (restTurnsI.isDefinedAt(turn) && restTurnsR.isDefinedAt(turn)) {
							val turni = restTurnsI(turn)
							val turnr = restTurnsR(turn)
							val turnklItoR = roundData.klInitiatorToResponder(turn)
							val turnklRtoI = roundData.klResponderToInitiator(turn)
							pwt.println(s"$pair;$round;$turn;${turni.intendedReferent};${turni.signal.toString};${turnr.inferredReferent};${turnr.signal.toString};${turni.listenEntropy};${turnr.listenEntropy};${turni.lexiconEntropy};${turnr.lexiconEntropy};$turnklItoR;$turnklRtoI")
						} else {
							val turni = restTurnsI(turn)
							pwt.println(s"$pair;$round;$turn;${turni.intendedReferent};${turni.signal.toString};NA;NA;${turni.listenEntropy};NA;${turni.lexiconEntropy};NA;NA;NA")
						}
					}
				})
			})
			pwt.close()

			val pwr = new PrintWriter(new File("output/datafiles/results_rounds_" + parameters + ".csv"))
			pwr.println("pair;round;nrTurns;success")
			allData.foreach(pairData => {
				val pair: Int = pairData._1
				val rounds: List[InteractionData] = pairData._2
				rounds.indices.foreach(round => {
					val nrTurns = rounds(round).initiatorData.length + 1
					val success = rounds(round).initialInitiatorData.intendedReferent == rounds(round).responderData.last.inferredReferent
					pwr.println(s"$pair;$round;$nrTurns;$success")
				})
			})
			pwr.close()

			val pwc = new PrintWriter(new File("output/datafiles/config_" + parameters + ".csv"))
			pwc.println("agentPairs;maxTurns;roundsPlayed;beta;entropyThreshold;order;costs;initiatorDistribution;responderDistribution")
			pwc.println(s"$nrPairs;$maxTurns;$nrRounds;$beta;$entropyThreshold;$order;$costs;$distribution;${1 - distribution}")
			pwc.close()

		}
		counter = counter + 1
	}
}
