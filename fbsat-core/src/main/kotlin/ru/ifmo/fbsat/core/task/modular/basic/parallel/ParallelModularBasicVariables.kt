package ru.ifmo.fbsat.core.task.modular.basic.parallel

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables

@Suppress("PropertyName")
class ParallelModularBasicVariables(
    val scenarioTree: ScenarioTree,
    /* Constants */
    val M: Int,
    val C: Int,
    val K: Int,
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
    val moduleControllingOutputVariable: IntMultiArray,
    /* Mapping variables */
    val modularMapping: MultiArray<IntMultiArray>
) {
    /* Computable constants */
    val V: Int = scenarioTree.size
    val E: Int = scenarioTree.inputEvents.size
    val O: Int = scenarioTree.outputEvents.size
    val X: Int = scenarioTree.inputNames.size
    val Z: Int = scenarioTree.outputNames.size
    val U: Int = scenarioTree.uniqueInputs.size

    /* Cardinality variables */
    var totalizer: IntArray? = null
        internal set
    var maxTransitions: Int? = null
        internal set
    val T: Int?
        get() = maxTransitions

    /* Modularized BasicVariables */
    val modularBasicVariables: MultiArray<BasicVariables> =
        MultiArray.create(M) { (m) ->
            BasicVariables(
                scenarioTree = scenarioTree,
                C = C, K = K,
                transitionDestination = modularTransitionDestination[m],
                transitionInputEvent = modularTransitionInputEvent[m],
                transitionFiring = modularTransitionFiring[m],
                firstFired = modularFirstFired[m],
                notFired = modularNotFired[m],
                stateOutputEvent = modularStateOutputEvent[m],
                stateAlgorithmTop = modularStateAlgorithmTop[m],
                stateAlgorithmBot = modularStateAlgorithmBot[m],
                actualTransitionFunction = modularActualTransitionFunction[m],
                mapping = modularMapping[m]
            )
        }
}
