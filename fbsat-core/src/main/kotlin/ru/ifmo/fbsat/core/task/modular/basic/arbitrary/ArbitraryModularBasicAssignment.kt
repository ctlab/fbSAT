package ru.ifmo.fbsat.core.task.modular.basic.arbitrary

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.ArbitraryModularAutomaton
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.TruthTableGuard
import ru.ifmo.fbsat.core.automaton.endow
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.RawAssignment
import ru.ifmo.fbsat.core.solver.convert
import ru.ifmo.fbsat.core.utils.log

// @Suppress("LocalVariableName")
// fun ArbitraryModularBasicAssignment.toAutomaton(): ArbitraryModularAutomaton {
//     val modules = MultiArray.create(M) { (m) ->
//         Automaton(scenarioTree).endow(
//             C = C, K = K,
//             stateOutputEvent = { OutputEvent("CNF") },
//             stateAlgorithm = { c ->
//                 BinaryAlgorithm(
//                     algorithm0 = BooleanArray(Z) { z0 -> modularStateAlgorithmBot[m][c, z0 + 1] },
//                     algorithm1 = BooleanArray(Z) { z0 -> modularStateAlgorithmTop[m][c, z0 + 1] }
//                 )
//             },
//             transitionDestination = { c, k -> modularTransitionDestination[m][c, k] },
//             transitionInputEvent = { _, _ -> InputEvent("REQ") },
//             transitionGuard = { c, k ->
//                 TruthTableGuard(
//                     truthTable = (1..U).associate { u ->
//                         InputValues(
//                             (u - 1).toString(2).padStart(X, '0').reversed().map {
//                                 when (it) {
//                                     '1' -> true
//                                     '0' -> false
//                                     else -> error("Bad bit '$it'")
//                                 }
//                             }) to when {
//                             modularNotFired[m][c, k, u] -> false
//                             modularFirstFired[m][c, u] == k -> true
//                             else -> null
//                         }
//                     },
//                     inputNames = scenarioTree.inputNames,
//                     uniqueInputs = scenarioTree.uniqueInputs
//                 )
//             }
//
//         )
//     }
//
//     return ArbitraryModularAutomaton(modules, inboundVarPinParent, scenarioTree)
// }
