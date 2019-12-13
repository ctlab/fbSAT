package ru.ifmo.fbsat.core.task.modular.extended.consecutive

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.modular.basic.consecutive.ConsecutiveModularBasicVariables
import ru.ifmo.fbsat.core.task.single.extended.ExtendedVariables

@Suppress("PropertyName")
class ConsecutiveModularExtendedVariables(
    val scenarioTree: ScenarioTree,
    /* Constants */
    val M: Int,
    val C: Int,
    val K: Int,
    val P: Int,
    /* Core variables */
    val modularTransitionDestination: MultiArray<IntMultiArray>,
    val modularTransitionInputEvent: MultiArray<IntMultiArray>,
    val modularTransitionFiring: MultiArray<IntMultiArray>,
    val modularFirstFired: MultiArray<IntMultiArray>,
    val modularNotFired: MultiArray<IntMultiArray>,
    val modularStateOutputEvent: MultiArray<IntMultiArray>,
    val modularStateAlgorithmTop: MultiArray<IntMultiArray>,
    val modularStateAlgorithmBot: MultiArray<IntMultiArray>,
    /* Interface variables */
    val modularActualTransitionFunction: MultiArray<IntMultiArray>,
    /* Mapping variables */
    val modularMapping: MultiArray<IntMultiArray>,
    /* Guard conditions variables */
    val modularNodeType: MultiArray<IntMultiArray>,
    val modularNodeInputVariable: MultiArray<IntMultiArray>,
    val modularNodeParent: MultiArray<IntMultiArray>,
    val modularNodeChild: MultiArray<IntMultiArray>,
    val modularNodeValue: MultiArray<IntMultiArray>
) {
    /* Computable constants */
    // TODO: extract these variables to constructor
    val V: Int = scenarioTree.size
    val E: Int = scenarioTree.inputEvents.size
    val O: Int = scenarioTree.outputEvents.size
    val X: Int = scenarioTree.inputNames.size
    val Z: Int = scenarioTree.outputNames.size
    val U: Int = scenarioTree.uniqueInputs.size

    /* Cardinality variables */
    var totalizer: IntArray? = null
        internal set
    var maxTotalGuardsSize: Int? = null
        internal set
    val N: Int?
        get() = maxTotalGuardsSize

    /* Modularized BasicVariables */
    val modularExtendedVariables: MultiArray<ExtendedVariables> =
        MultiArray.create(M) { (m) ->
            ExtendedVariables(
                scenarioTree,
                C = C, K = K, P = P,
                transitionDestination = modularTransitionDestination[m],
                transitionInputEvent = modularTransitionInputEvent[m],
                transitionFiring = modularTransitionFiring[m],
                firstFired = modularFirstFired[m],
                notFired = modularNotFired[m],
                stateOutputEvent = modularStateOutputEvent[m],
                stateAlgorithmTop = modularStateAlgorithmTop[m],
                stateAlgorithmBot = modularStateAlgorithmBot[m],
                actualTransitionFunction = modularActualTransitionFunction[m],
                mapping = modularMapping[m],
                nodeType = modularNodeType[m],
                nodeInputVariable = modularNodeInputVariable[m],
                nodeParent = modularNodeParent[m],
                nodeChild = modularNodeChild[m],
                nodeValue = modularNodeValue[m]
            )
        }

    constructor(
        basicVars: ConsecutiveModularBasicVariables,
        M: Int,
        P: Int,
        modularNodeType: MultiArray<IntMultiArray>,
        modularNodeInputVariable: MultiArray<IntMultiArray>,
        modularNodeParent: MultiArray<IntMultiArray>,
        modularNodeChild: MultiArray<IntMultiArray>,
        modularNodeValue: MultiArray<IntMultiArray>
    ) : this(
        scenarioTree = basicVars.scenarioTree,
        M = M,
        C = basicVars.C,
        K = basicVars.K,
        P = P,
        modularTransitionDestination = basicVars.modularTransitionDestination,
        modularTransitionInputEvent = basicVars.modularTransitionInputEvent,
        modularTransitionFiring = basicVars.modularTransitionFiring,
        modularFirstFired = basicVars.modularFirstFired,
        modularNotFired = basicVars.modularNotFired,
        modularStateOutputEvent = basicVars.modularStateOutputEvent,
        modularStateAlgorithmTop = basicVars.modularStateAlgorithmTop,
        modularStateAlgorithmBot = basicVars.modularStateAlgorithmBot,
        modularActualTransitionFunction = basicVars.modularActualTransitionFunction,
        modularMapping = basicVars.modularMapping,
        modularNodeType = modularNodeType,
        modularNodeInputVariable = modularNodeInputVariable,
        modularNodeParent = modularNodeParent,
        modularNodeChild = modularNodeChild,
        modularNodeValue = modularNodeValue
    )
}

fun Solver.declareConsecutiveModularExtendedVariables(
    basicVars: ConsecutiveModularBasicVariables,
    P: Int
): ConsecutiveModularExtendedVariables =
    with(basicVars) {
        val modularExtendedVariables = MultiArray.create(M) { (m) ->
            // declareExtendedVariables(basicVars = basicVars.modularBasicVariables[m], P = P)

            /* Guard conditions variables */
            val nodeType = newArray(C, K, P, NodeType.values().size, one = true)
            val nodeInputVariable = newArray(C, K, P, X + 1, one = true)
            val nodeParent = newArray(C, K, P, P + 1, one = true) { (_, _, p, par) ->
                if (par < p || par == P + 1) newVariable()
                else Solver.falseVariable
            }
            val nodeChild = newArray(C, K, P, P + 1, one = true) { (_, _, p, ch) ->
                if (ch > p || ch == P + 1) newVariable()
                else Solver.falseVariable
            }
            val nodeValue = newArray(C, K, P, U) { (c, k, p, u) ->
                if (p == 1) modularTransitionFiring[m][c, k, u]
                else newVariable()
            }

            ExtendedVariables(
                basicVars = modularBasicVariables[m],
                P = P,
                nodeType = nodeType,
                nodeInputVariable = nodeInputVariable,
                nodeParent = nodeParent,
                nodeChild = nodeChild,
                nodeValue = nodeValue
            )
        }

        val modularNodeType = MultiArray.create(M) { (m) ->
            modularExtendedVariables[m].nodeType
        }
        val modularNodeInputVariable = MultiArray.create(M) { (m) ->
            modularExtendedVariables[m].nodeInputVariable
        }
        val modularNodeParent = MultiArray.create(M) { (m) ->
            modularExtendedVariables[m].nodeParent
        }
        val modularNodeChild = MultiArray.create(M) { (m) ->
            modularExtendedVariables[m].nodeChild
        }
        val modularNodeValue = MultiArray.create(M) { (m) ->
            modularExtendedVariables[m].nodeValue
        }

        ConsecutiveModularExtendedVariables(
            basicVars = basicVars,
            M = M,
            P = P,
            modularNodeType = modularNodeType,
            modularNodeInputVariable = modularNodeInputVariable,
            modularNodeParent = modularNodeParent,
            modularNodeChild = modularNodeChild,
            modularNodeValue = modularNodeValue
        )
    }
