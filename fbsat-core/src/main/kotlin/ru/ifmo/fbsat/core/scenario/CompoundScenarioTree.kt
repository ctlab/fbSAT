package ru.ifmo.fbsat.core.scenario

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import ru.ifmo.fbsat.core.utils.Compound

interface CompoundScenarioTree<T, S, N> : GenericScenarioTree<S, N>, Compound<T>
    where T : ScenarioTree<*, *>,
          S : CompoundScenario<*>,
          N : CompoundScenarioTree.Node<*> {

    val modularInputEvents: MultiArray<List<InputEvent>>
    val modularOutputEvents: MultiArray<List<OutputEvent>>
    val modularInputNames: MultiArray<List<String>>
    val modularOutputNames: MultiArray<List<String>>

    interface Node<out Self : Node<Self>> :
        GenericScenarioTree.Node<Self, CompoundScenarioElement>,
        Compound<ScenarioTree.Node<*>>
}

val CompoundScenarioTree<*, *, *>.modularInitialOutputValues: MultiArray<OutputValues>
    get() = modular.map { it.initialOutputValues }
