package ru.ifmo.fbsat.core.task.modular.basic.parallel

// fun ParallelModularBasicAssignment.toAutomaton(): ParallelModularAutomaton {
//     val modules = MultiArray.create(M) { (m) ->
//         with(modularBasicAssignment[m]) {
//             Automaton(
//                 scenarioTree.inputEvents,
//                 scenarioTree.outputEvents,
//                 scenarioTree.inputNames,
//                 moduleOutputVariables[m].map { z -> scenarioTree.outputNames[z - 1] }
//             ).endow(
//                 C = C, K = K,
//                 stateOutputEvent = { c ->
//                     stateOutputEvent[c].let { o ->
//                         if (o == 0) null else scenarioTree.outputEvents[o - 1]
//                     }
//                 },
//                 stateAlgorithm = { c ->
//                     BinaryAlgorithm(
//                         algorithm0 = moduleOutputVariables[m].map { z -> stateAlgorithmBot[c, z] },
//                         algorithm1 = moduleOutputVariables[m].map { z -> stateAlgorithmTop[c, z] }
//                     )
//                 },
//                 transitionDestination = { c, k ->
//                     transitionDestination[c, k]
//                 },
//                 transitionInputEvent = { c, k ->
//                     scenarioTree.inputEvents[transitionInputEvent[c, k] - 1]
//                 },
//                 transitionGuard = { c, k ->
//                     TruthTableGuard(
//                         truthTable = scenarioTree.uniqueInputs
//                             .withIndex(start = 1)
//                             .associate { (u, input) ->
//                                 input to transitionTruthTable[c, k, u]
//                             },
//                         inputNames = scenarioTree.inputNames,
//                         uniqueInputs = scenarioTree.uniqueInputs
//                     )
//                 }
//             )
//         }
//     }
//
//     return ParallelModularAutomaton(modules, moduleControllingOutputVariable)
// }
