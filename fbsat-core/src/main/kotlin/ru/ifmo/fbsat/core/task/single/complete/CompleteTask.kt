package ru.ifmo.fbsat.core.task.single.complete

import com.github.lipen.satlib.core.BoolVarArray
import com.github.lipen.satlib.core.IntVar
import com.github.lipen.satlib.core.IntVarArray
import com.github.lipen.satlib.core.Lit
import com.github.lipen.satlib.core.newBoolVarArray
import com.github.lipen.satlib.core.newIntVar
import com.github.lipen.satlib.solver.Solver
import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.constraints.declareNegativeAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declareNegativeGuardConditionsConstraints
import ru.ifmo.fbsat.core.constraints.declareNegativeMappingConstraints
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.timeSince

private val logger = MyLogger {}

data class CompleteTask(
    val negativeScenarioTree: NegativeScenarioTree? = null, // empty if null
) : Task() {
    override fun Solver.declare_() {
        val positiveScenarioTree: PositiveScenarioTree = context["positiveScenarioTree"]
        val negativeScenarioTree = context("negativeScenarioTree") {
            this@CompleteTask.negativeScenarioTree ?: NegativeScenarioTree(positiveScenarioTree)
        }

        /* Variables */
        comment("$name: Variables")
        declareCompleteVariables(
            negativeScenarioTree = negativeScenarioTree
        )

        /* Initial negative constraints */
        comment("$name: Initial negative constraints")
        updateNegativeReduction()
    }
}

@Suppress("LocalVariableName")
fun Solver.updateNegativeReduction(
    isForbidLoops: Boolean = true,
) {
    val timeStart = PerformanceCounter.reference
    val nVarsStart = numberOfVariables
    val nClausesStart = numberOfClauses

    /* Constants */
    val positiveScenarioTree: PositiveScenarioTree = context["positiveScenarioTree"]
    val negativeScenarioTree: NegativeScenarioTree = context["negativeScenarioTree"]

    val oldNegV: Int = context["negV"]
    val negV: Int = context("negV", negativeScenarioTree.size)
    val newNegVs = (oldNegV + 1)..negV

    val negU: Int = context("negU", negativeScenarioTree.uniqueInputs.size)

    val oldNegUIs: List<InputValues> = context["negUIs"]
    val negUIs = context("negUIs", negativeScenarioTree.uniqueInputs)
    val posUIs = positiveScenarioTree.uniqueInputs

    fun getOldNegU(input: InputValues): Int = oldNegUIs.indexOf(input) + 1
    fun getNegU(input: InputValues): Int = negUIs.indexOf(input) + 1
    fun getPosU(input: InputValues): Int = posUIs.indexOf(input) + 1

    fun inputChoice(
        u: Int,
        inOldNegUIs: (Int) -> Lit,
        inPosUIs: (Int) -> Lit,
    ): Lit =
        when (val input = negUIs[u - 1]) {
            in oldNegUIs -> inOldNegUIs(getOldNegU(input))
            in posUIs -> inPosUIs(getPosU(input))
            else -> newLiteral()
        }

    fun inputChoice(
        u: Int,
        inOldNegUIs: (Int) -> IntVar,
        inPosUIs: (Int) -> IntVar,
        new: () -> IntVar,
    ): IntVar =
        when (val input = negUIs[u - 1]) {
            in oldNegUIs -> inOldNegUIs(getOldNegU(input))
            in posUIs -> inPosUIs(getPosU(input))
            else -> new()
        }

    val oldOnlyNegUIs: List<InputValues> = context["onlyNegUIs"]
    val onlyNegUIs = context("onlyNegUIs") { negUIs - posUIs }
    val newOnlyNegUIs = onlyNegUIs - oldOnlyNegUIs
    val newOnlyNegUs = newOnlyNegUIs.map(::getNegU)

    /* Variables */
    val C: Int = context["C"]
    val K: Int = context["K"]
    val P: Int = context["P"]
    val E: Int = context["E"]
    val actualTransitionFunction: IntVarArray = context["actualTransitionFunction"]
    val transitionTruthTable: BoolVarArray = context["transitionTruthTable"]
    val transitionFiring: BoolVarArray = context["transitionFiring"]
    val firstFired: IntVarArray = context["firstFired"]
    val notFired: BoolVarArray = context["notFired"]
    val nodeValue: BoolVarArray = context["nodeValue"]
    val oldNegActualTransitionFunction: IntVarArray = context["negActualTransitionFunction"]
    val oldNegTransitionTruthTable: BoolVarArray = context["negTransitionTruthTable"]
    val oldNegTransitionFiring: BoolVarArray = context["negTransitionFiring"]
    val oldNegFirstFired: IntVarArray = context["negFirstFired"]
    val oldNegNotFired: BoolVarArray = context["negNotFired"]
    val oldNegNodeValue: BoolVarArray = context["negNodeValue"]
    val oldNegMapping: IntVarArray = context["negMapping"]

    val negActualTransitionFunction = context("negActualTransitionFunction") {
        IntVarArray.new(C, E, negU) { (c, e, u) ->
            inputChoice(u,
                { oldNegActualTransitionFunction[c, e, it] },
                { actualTransitionFunction[c, e, it] },
                { newIntVar(0..C) })
        }
    }
    val negTransitionTruthTable = context("negTransitionTruthTable") {
        newBoolVarArray(C, K, negU) { (c, k, u) ->
            inputChoice(u,
                { oldNegTransitionTruthTable[c, k, it] },
                { transitionTruthTable[c, k, it] })
        }
    }
    val negTransitionFiring = context("negTransitionFiring") {
        newBoolVarArray(C, K, E, negU) { (c, k, e, u) ->
            inputChoice(u,
                { oldNegTransitionFiring[c, k, e, it] },
                { transitionFiring[c, k, e, it] })
        }
    }
    val negFirstFired = context("negFirstFired") {
        IntVarArray.new(C, E, negU) { (c, e, u) ->
            inputChoice(u,
                { oldNegFirstFired[c, e, it] },
                { firstFired[c, e, it] },
                { newIntVar(0..K) })
        }
    }
    val negNotFired = context("negNotFired") {
        newBoolVarArray(C, K, E, negU) { (c, k, e, u) ->
            inputChoice(u,
                { oldNegNotFired[c, k, e, it] },
                { notFired[c, k, e, it] })
        }
    }
    val negNodeValue = context("negNodeValue") {
        newBoolVarArray(C, K, P, negU) { (c, k, p, u) ->
            if (p == 1) negTransitionTruthTable[c, k, u]
            else inputChoice(u,
                { oldNegNodeValue[c, k, p, it] },
                { nodeValue[c, k, p, it] })
        }
    }
    val negMapping = context("negMapping") {
        IntVarArray.new(negV) { (v) ->
            if (v in newNegVs) newIntVar(0..C)
            else oldNegMapping[v]
        }
    }

    if (Globals.IS_DUMP_VARS_IN_CNF) {
        logger.warn("Dumping of CompleteVariables to CNF is not implemented yet")
    }

    /* Constraints */
    declareNegativeAutomatonStructureConstraints(Us = newOnlyNegUs)
    declareNegativeGuardConditionsConstraints(Us = newOnlyNegUs)
    declareNegativeMappingConstraints(Vs = newNegVs, isForbidLoops = isForbidLoops)

    val nVarsDiff = numberOfVariables - nVarsStart
    val nClausesDiff = numberOfClauses - nClausesStart
    logger.debug {
        "updateNegativeReduction: declared $nVarsDiff variables and $nClausesDiff clauses in %.3f s."
            .format(timeSince(timeStart).seconds)
    }
}
