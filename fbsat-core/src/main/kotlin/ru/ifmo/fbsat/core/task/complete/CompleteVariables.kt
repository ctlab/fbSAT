package ru.ifmo.fbsat.core.task.complete

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.automaton.InputValues
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.declareNegativeColorConstraints
import ru.ifmo.fbsat.core.task.declareNegativeFiringConstraints
import ru.ifmo.fbsat.core.task.declareNegativeGuardConstraints
import ru.ifmo.fbsat.core.task.declareNegativeTransitionConstraints
import ru.ifmo.fbsat.core.task.extended.ExtendedVariables

@Suppress("MemberVisibilityCanBePrivate", "PropertyName")
class CompleteVariables(
    internal val extVars: ExtendedVariables,
    val negativeScenarioTree: NegativeScenarioTree
) {
    val scenarioTree: ScenarioTree = extVars.scenarioTree
    // Constants (extended)
    val C: Int = extVars.C
    val K: Int = extVars.K
    val P: Int = extVars.P
    val V: Int = extVars.V
    val E: Int = extVars.E
    val O: Int = extVars.O
    val X: Int = extVars.X
    val Z: Int = extVars.Z
    val U: Int = extVars.U
    // Variables (extended)
    val transition: IntMultiArray = extVars.transition
    val actualTransition: IntMultiArray = extVars.actualTransition
    val inputEvent: IntMultiArray = extVars.inputEvent
    val outputEvent: IntMultiArray = extVars.outputEvent
    val algorithm0: IntMultiArray = extVars.algorithm0
    val algorithm1: IntMultiArray = extVars.algorithm1
    val color: IntMultiArray = extVars.color
    val rootValue: IntMultiArray = extVars.rootValue
    val firstFired: IntMultiArray = extVars.firstFired
    val notFired: IntMultiArray = extVars.notFired
    val nodeType: IntMultiArray = extVars.nodeType
    val terminal: IntMultiArray = extVars.terminal
    val parent: IntMultiArray = extVars.parent
    val child: IntMultiArray = extVars.child
    val nodeValue: IntMultiArray = extVars.nodeValue

    // Cardinality
    val totalizer: IntArray?
        get() = extVars.totalizer
    val maxTotalGuardsSize: Int?
        get() = extVars.maxTotalGuardsSize

    // Negative constants and variables
    var negV: Int = negativeScenarioTree.size
        private set
    var negU: Int = negativeScenarioTree.uniqueOutputs.size
        private set
    var negUIs: List<InputValues> = negativeScenarioTree.uniqueInputs
        private set
    val posUIs: List<InputValues> = scenarioTree.uniqueInputs
    var onlyNegUIs: List<InputValues> = negUIs - posUIs
        private set
    val forbiddenLoops: MutableSet<Pair<Int, Int>> = mutableSetOf()
    lateinit var negActualTransition: IntMultiArray
        private set
    lateinit var satisfaction: IntMultiArray
        private set
    lateinit var negNodeValue: IntMultiArray
        private set
    lateinit var negRootValue: IntMultiArray
        private set
    lateinit var negFirstFired: IntMultiArray
        private set
    lateinit var negNotFired: IntMultiArray
        private set

    // fun Solver.updateCardinality(newMaxTotalGuardsSize: Int?) {
    //     with(extVars) {
    //         updateCardinality(newMaxTotalGuardsSize)
    //     }
    // }

    fun Solver.updateNegativeReduction() {
        // TODO: timeit
        // Constants
        val oldNegV = negV
        negV = negativeScenarioTree.size
        val newNegVs = (oldNegV + 1)..negV
        val newNegVsActive = newNegVs.filter { it in negativeScenarioTree.activeVertices }
        val newNegVsPassive = newNegVs.filter { it in negativeScenarioTree.passiveVertices }

        negU = negativeScenarioTree.uniqueInputs.size

        val oldNegUIs = negUIs
        negUIs = negativeScenarioTree.uniqueInputs

        fun getNegU(input: InputValues): Int = negUIs.indexOf(input) + 1
        fun getOldNegU(input: InputValues): Int = oldNegUIs.indexOf(input) + 1
        fun getPosU(input: InputValues): Int = posUIs.indexOf(input) + 1

        val oldOnlyNegUIs = onlyNegUIs
        onlyNegUIs = negUIs - posUIs
        val newOnlyNegUIs = onlyNegUIs - oldOnlyNegUIs
        val newOnlyNegUs = newOnlyNegUIs.map(::getNegU)

        // Variables
        negActualTransition = newArray(C, E, negU, C + 1) { (i, e, u, j) ->
            when (val input = negUIs[u - 1]) {
                in newOnlyNegUIs -> newVariable()
                in oldNegUIs -> negActualTransition[i, e, getOldNegU(input), j]
                else -> actualTransition[i, e, getPosU(input), j]
            }
        }
        satisfaction = newArray(negV, C + 1) { (v, c) ->
            if (v in newNegVs) newVariable()
            else satisfaction[v, c]
        }
        negNodeValue = newArray(C, K, P, negU) { (c, k, p, u) ->
            when (val input = negUIs[u - 1]) {
                in newOnlyNegUIs -> newVariable()
                in oldNegUIs -> negNodeValue[c, k, p, getOldNegU(input)]
                else -> nodeValue[c, k, p, getPosU(input)]
            }
        }
        negRootValue = newArray(C, K, negU) { (c, k, u) ->
            negNodeValue[c, k, 1, u]
        }
        negFirstFired = newArray(C, negU, K + 1) { (c, u, k) ->
            when (val input = negUIs[u - 1]) {
                in newOnlyNegUIs -> newVariable()
                in oldNegUIs -> negFirstFired[c, getOldNegU(input), k]
                else -> firstFired[c, getPosU(input), k]
            }
        }
        negNotFired = newArray(C, negU, K) { (c, u, k) ->
            when (val input = negUIs[u - 1]) {
                in newOnlyNegUIs -> newVariable()
                in oldNegUIs -> negNotFired[c, getOldNegU(input), k]
                else -> notFired[c, getPosU(input), k]
            }
        }

        // Constraints
        declareNegativeColorConstraints(
            negativeScenarioTree = negativeScenarioTree,
            C = C, Z = Z, negV = negV,
            newNegVs = newNegVs,
            newNegVsActive = newNegVsActive,
            newNegVsPassive = newNegVsPassive,
            outputEvent = outputEvent,
            algorithm0 = algorithm0,
            algorithm1 = algorithm1,
            satisfaction = satisfaction,
            negActualTransition = negActualTransition,
            forbiddenLoops = forbiddenLoops
        )
        declareNegativeTransitionConstraints(
            C = C, K = K, E = E,
            newOnlyNegUs = newOnlyNegUs,
            transition = transition,
            inputEvent = inputEvent,
            negActualTransition = negActualTransition,
            negFirstFired = negFirstFired
        )
        declareNegativeFiringConstraints(
            C = C, K = K,
            newOnlyNegUs = newOnlyNegUs,
            negRootValue = negRootValue,
            negFirstFired = negFirstFired,
            negNotFired = negNotFired
        )
        declareNegativeGuardConstraints(
            C = C, K = K, P = P, X = X,
            negUIs = negUIs,
            newOnlyNegUs = newOnlyNegUs,
            nodeType = nodeType,
            terminal = terminal,
            child = child,
            negNodeValue = negNodeValue
        )
    }
}

// Note: do not create getters for negative properties,
// because we need to access old values before recalculating them
