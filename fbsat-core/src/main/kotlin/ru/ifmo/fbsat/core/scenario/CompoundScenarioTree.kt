package ru.ifmo.fbsat.core.scenario

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.utils.Compound

interface CompoundScenarioTree<S, N> : GenericScenarioTree<S, N>, Compound<ScenarioTree<*, *>>
    where S : CompoundScenario,
          N : CompoundScenarioTree.Node {

    val modularInputEvents: MultiArray<List<InputEvent>>
    val modularOutputEvents: MultiArray<List<OutputEvent>>
    val modularInputNames: MultiArray<List<String>>
    val modularOutputNames: MultiArray<List<String>>

    interface Node : GenericScenarioTree.Node<CompoundScenarioElement>
}
