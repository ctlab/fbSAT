package ru.ifmo.fbsat.core.task.single.complete

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.automaton.InputValues
import ru.ifmo.fbsat.core.constraints.declareNegativeAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declareNegativeGuardConditionsConstraints
import ru.ifmo.fbsat.core.constraints.declareNegativeMappingConstraints
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.solver.IntVar
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Literal
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.newBoolVarArray
import ru.ifmo.fbsat.core.solver.newIntVar
import ru.ifmo.fbsat.core.task.COMPLETE_VARS
import ru.ifmo.fbsat.core.task.completeVars
import ru.ifmo.fbsat.core.task.extendedVars
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeSince

fun Solver.declareComplete(
    negativeScenarioTree: NegativeScenarioTree? = null // empty if null
) {
    val timeStart = PerformanceCounter.reference
    val nvarStart = numberOfVariables
    val nconStart = numberOfClauses

    val scenarioTree = extendedVars.scenarioTree
    val negTree = negativeScenarioTree ?: NegativeScenarioTree(
        inputEvents = scenarioTree.inputEvents,
        outputEvents = scenarioTree.outputEvents,
        inputNames = scenarioTree.inputNames,
        outputNames = scenarioTree.outputNames
    )

    /* Variables */
    val vars = declareCompleteVariables(
        extendedVars = extendedVars,
        negativeScenarioTree = negTree
    ).also {
        context[COMPLETE_VARS] = it
    }

    /* Initial negative constraints */
    updateNegativeReduction()

    val nvarDiff = numberOfVariables - nvarStart
    val nconDiff = numberOfClauses - nconStart
    log.info(
        "Done declaring complete variables ($nvarDiff) and constraints ($nconDiff) in %.2f s."
            .format(timeSince(timeStart).seconds)
    )
}

fun Solver.updateNegativeReduction() {
    // TODO: timeit
    val vars = completeVars
    with(vars) {
        /* Constants */
        val oldNegV = negV
        negV = negativeScenarioTree.size
        val newNegVs = (oldNegV + 1)..negV

        negU = negativeScenarioTree.uniqueInputs.size

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

        val oldOnlyNegUIs = onlyNegUIs
        onlyNegUIs = negUIs - posUIs
        val newOnlyNegUIs = onlyNegUIs - oldOnlyNegUIs
        val newOnlyNegUs = newOnlyNegUIs.map(::getNegU)

        /* Variables */
        negTransitionFiring = newBoolVarArray(C, K, negU) { (c, k, u) ->
            inputChoice(u,
                { negTransitionFiring[c, k, it] },
                { transitionFiring[c, k, it] })
        }
        negFirstFired = IntVarArray.create(C, negU) { (c, u) ->
            inputChoice(u,
                { negFirstFired[c, it] },
                { firstFired[c, it] },
                { newIntVar(0..K) })
        }
        negNotFired = newBoolVarArray(C, K, negU) { (c, k, u) ->
            inputChoice(u,
                { negNotFired[c, k, it] },
                { notFired[c, k, it] })
        }
        negActualTransitionFunction = IntVarArray.create(C, E, negU) { (c, e, u) ->
            inputChoice(u,
                { negActualTransitionFunction[c, e, it] },
                { actualTransitionFunction[c, e, it] },
                { newIntVar(0..C) })
        }
        negNodeValue = newBoolVarArray(C, K, P, negU) { (c, k, p, u) ->
            if (p == 1) negTransitionFiring[c, k, u]
            else inputChoice(u,
                { negNodeValue[c, k, p, it] },
                { nodeValue[c, k, p, it] })
        }
        negMapping = IntVarArray.create(negV) { (v) ->
            if (v in newNegVs) newIntVar(0..C)
            else negMapping[v]
        }

        /* Constraints */
        declareNegativeAutomatonStructureConstraints(vars, Us = newOnlyNegUs)
        declareNegativeGuardConditionsConstraints(vars, Us = newOnlyNegUs)
        declareNegativeMappingConstraints(vars, Vs = newNegVs)
    }
}
