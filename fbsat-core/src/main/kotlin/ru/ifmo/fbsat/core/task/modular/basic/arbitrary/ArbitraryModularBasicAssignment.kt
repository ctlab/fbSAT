package ru.ifmo.fbsat.core.task.modular.basic.arbitrary

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
