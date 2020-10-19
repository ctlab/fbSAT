package ru.ifmo.fbsat.core.task.single.complete

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.constraints.declareNegativeAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declareNegativeGuardConditionsConstraints
import ru.ifmo.fbsat.core.constraints.declareNegativeMappingConstraints
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.BoolVarArray
import ru.ifmo.fbsat.core.solver.IntVar
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Literal
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.mutateBoolVarArray
import ru.ifmo.fbsat.core.solver.mutateIntVarArray
import ru.ifmo.fbsat.core.solver.newIntVar
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeSince

data class CompleteTask(
    val negativeScenarioTree: NegativeScenarioTree? = null // empty if null
) : Task() {
    override fun Solver.declare_() {
        val scenarioTree: PositiveScenarioTree by context
        val negativeScenarioTree by context(
            this@CompleteTask.negativeScenarioTree ?: NegativeScenarioTree(
                inputEvents = scenarioTree.inputEvents,
                outputEvents = scenarioTree.outputEvents,
                inputNames = scenarioTree.inputNames,
                outputNames = scenarioTree.outputNames
            )
        )

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
    isForbidLoops: Boolean = true
) {
    val timeStart = PerformanceCounter.reference
    val nVarsStart = numberOfVariables
    val nClausesStart = numberOfClauses

    /* Constants */
    val scenarioTree: PositiveScenarioTree by context
    val negativeScenarioTree: NegativeScenarioTree by context

    var negV: Int by context
    val oldNegV = negV
    negV = negativeScenarioTree.size
    val newNegVs = (oldNegV + 1)..negV

    val negU by context(negativeScenarioTree.uniqueInputs.size)

    val posUIs = scenarioTree.uniqueInputs
    var negUIs: List<InputValues> by context
    val oldNegUIs = negUIs
    negUIs = negativeScenarioTree.uniqueInputs

    fun getOldNegU(input: InputValues): Int = oldNegUIs.indexOf(input) + 1
    fun getNegU(input: InputValues): Int = negUIs.indexOf(input) + 1
    fun getPosU(input: InputValues): Int = posUIs.indexOf(input) + 1

    fun inputChoice(
        u: Int,
        inOldNegUIs: (Int) -> Literal,
        inPosUIs: (Int) -> Literal
    ): Literal =
        when (val input = negUIs[u - 1]) {
            in oldNegUIs -> inOldNegUIs(getOldNegU(input))
            in posUIs -> inPosUIs(getPosU(input))
            else -> newLiteral()
        }

    fun inputChoice(
        u: Int,
        inOldNegUIs: (Int) -> IntVar,
        inPosUIs: (Int) -> IntVar,
        new: () -> IntVar
    ): IntVar =
        when (val input = negUIs[u - 1]) {
            in oldNegUIs -> inOldNegUIs(getOldNegU(input))
            in posUIs -> inPosUIs(getPosU(input))
            else -> new()
        }

    var onlyNegUIs: List<InputValues> by context
    val oldOnlyNegUIs = onlyNegUIs
    onlyNegUIs = negUIs - posUIs
    val newOnlyNegUIs = onlyNegUIs - oldOnlyNegUIs
    val newOnlyNegUs = newOnlyNegUIs.map(::getNegU)

    /* Variables */
    val C: Int by context
    val K: Int by context
    val P: Int by context
    val E: Int by context
    val actualTransitionFunction: IntVarArray by context
    val transitionTruthTable: BoolVarArray by context
    val transitionFiring: BoolVarArray by context
    val firstFired: IntVarArray by context
    val notFired: BoolVarArray by context
    val nodeValue: BoolVarArray by context

    // var negTransitionTruthTable: BoolVarArray by context
    // negTransitionTruthTable = newBoolVarArray(C, K, negU) { (c, k, u) ->
    //     inputChoice(u,
    //         { negTransitionTruthTable[c, k, it] },
    //         { transitionTruthTable[c, k, it] })
    // }
    context.mutateBoolVarArray("negTransitionTruthTable", C, K, negU) { orig, (c, k, u) ->
        inputChoice(u,
            { orig[c, k, it] },
            { transitionTruthTable[c, k, it] })
    }

    // var negTransitionFiring: BoolVarArray by context
    // negTransitionFiring = newBoolVarArray(C, K, E, negU) { (c, k, e, u) ->
    //     inputChoice(u,
    //         { negTransitionFiring[c, k, e, it] },
    //         { transitionFiring[c, k, e, it] })
    // }
    context.mutateBoolVarArray("negTransitionFiring", C, K, E, negU) { orig, (c, k, e, u) ->
        inputChoice(u,
            { orig[c, k, e, it] },
            { transitionFiring[c, k, e, it] })
    }

    // var negFirstFired: IntVarArray by context
    // negFirstFired = IntVarArray.create(C, E, negU) { (c, e, u) ->
    //     inputChoice(u,
    //         { negFirstFired[c, e, it] },
    //         { firstFired[c, e, it] },
    //         { newIntVar(0..K) })
    // }
    context.mutateIntVarArray("negFirstFired", C, E, negU) { orig, (c, e, u) ->
        inputChoice(u,
            { orig[c, e, it] },
            { firstFired[c, e, it] },
            { newIntVar(0..K) })
    }

    // var negNotFired: BoolVarArray by context
    // negNotFired = newBoolVarArray(C, K, E, negU) { (c, k, e, u) ->
    //     inputChoice(u,
    //         { negNotFired[c, k, e, it] },
    //         { notFired[c, k, e, it] })
    // }
    context.mutateBoolVarArray("negNotFired", C, K, E, negU) { orig, (c, k, e, u) ->
        inputChoice(u,
            { orig[c, k, e, it] },
            { notFired[c, k, e, it] })
    }

    // var negActualTransitionFunction: IntVarArray by context
    // negActualTransitionFunction = IntVarArray.create(C, E, negU) { (c, e, u) ->
    //     inputChoice(u,
    //         { negActualTransitionFunction[c, e, it] },
    //         { actualTransitionFunction[c, e, it] },
    //         { newIntVar(0..C) })
    // }
    context.mutateIntVarArray("negActualTransitionFunction", C, E, negU) { orig, (c, e, u) ->
        inputChoice(u,
            { orig[c, e, it] },
            { actualTransitionFunction[c, e, it] },
            { newIntVar(0..C) })
    }

    // var negNodeValue: BoolVarArray by context
    // negNodeValue = newBoolVarArray(C, K, P, negU) { (c, k, p, u) ->
    //     if (p == 1) negTransitionTruthTable[c, k, u]
    //     else inputChoice(u,
    //         { negNodeValue[c, k, p, it] },
    //         { nodeValue[c, k, p, it] })
    // }
    val negTransitionTruthTable: BoolVarArray by context
    context.mutateBoolVarArray("negNodeValue", C, K, P, negU) { orig, (c, k, p, u) ->
        if (p == 1) negTransitionTruthTable[c, k, u]
        else inputChoice(u,
            { orig[c, k, p, it] },
            { nodeValue[c, k, p, it] })
    }

    // var negMapping: IntVarArray by context
    // negMapping = IntVarArray.create(negV) { (v) ->
    //     if (v in newNegVs) newIntVar(0..C)
    //     else negMapping[v]
    // }
    context.mutateIntVarArray("negMapping", negV) { orig, (v) ->
        if (v in newNegVs) newIntVar(0..C)
        else orig[v]
    }

    /* Constraints */
    declareNegativeAutomatonStructureConstraints(Us = newOnlyNegUs)
    declareNegativeGuardConditionsConstraints(Us = newOnlyNegUs)
    declareNegativeMappingConstraints(Vs = newNegVs, isForbidLoops = isForbidLoops)

    val nVarsDiff = numberOfVariables - nVarsStart
    val nClausesDiff = numberOfClauses - nClausesStart
    log.debug {
        "updateNegativeReduction: declared $nVarsDiff variables and $nClausesDiff clauses in %.3f s."
            .format(timeSince(timeStart).seconds)
    }
}
