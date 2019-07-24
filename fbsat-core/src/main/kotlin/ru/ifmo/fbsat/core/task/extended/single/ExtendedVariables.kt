package ru.ifmo.fbsat.core.task.extended.single

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareComparatorLessThanOrEqual
import ru.ifmo.fbsat.core.solver.declareTotalizer
import ru.ifmo.fbsat.core.task.basic.single.BasicVariables
import ru.ifmo.fbsat.core.utils.Globals

class ExtendedVariables(
    private val basicVars: BasicVariables,
    // Constants
    val P: Int,
    // Variables
    val nodeType: IntMultiArray,
    val terminal: IntMultiArray,
    val parent: IntMultiArray,
    val child: IntMultiArray,
    val nodeValue: IntMultiArray
) {
    val scenarioTree: ScenarioTree = basicVars.scenarioTree
    // Constants (basic)
    val C: Int = basicVars.C
    val K: Int = basicVars.K
    val V: Int = basicVars.V
    val E: Int = basicVars.E
    val O: Int = basicVars.O
    val X: Int = basicVars.X
    val Z: Int = basicVars.Z
    val U: Int = basicVars.U

    // Variables (basic)
    val transition: IntMultiArray = basicVars.transition
    val actualTransition: IntMultiArray = basicVars.actualTransition
    val inputEvent: IntMultiArray = basicVars.inputEvent
    val outputEvent: IntMultiArray = basicVars.outputEvent
    val algorithm0: IntMultiArray = basicVars.algorithm0
    val algorithm1: IntMultiArray = basicVars.algorithm1
    val color: IntMultiArray = basicVars.color
    val rootValue: IntMultiArray = basicVars.rootValue
    val firstFired: IntMultiArray = basicVars.firstFired
    val notFired: IntMultiArray = basicVars.notFired

    // Cardinality
    var totalizer: IntArray? = null
        private set
    var maxTotalGuardsSize: Int? = null
        private set

    fun Solver.updateCardinality(newMaxTotalGuardsSize: Int?) {
        maxTotalGuardsSize?.let { N ->
            check(newMaxTotalGuardsSize != null && newMaxTotalGuardsSize <= N) { "Cannot soften UB" }
        }

        if (newMaxTotalGuardsSize == null && !Globals.IS_ENCODE_TOTALIZER) return
        if (totalizer == null) {
            totalizer = declareTotalizer {
                for (c in 1..C)
                    for (k in 1..K)
                        for (p in 1..P)
                            yield(-nodeType[c, k, p, NodeType.NONE.value])
            }
        }
        if (newMaxTotalGuardsSize == null) return

        declareComparatorLessThanOrEqual(totalizer!!, newMaxTotalGuardsSize, maxTotalGuardsSize)
        maxTotalGuardsSize = newMaxTotalGuardsSize
    }
}
