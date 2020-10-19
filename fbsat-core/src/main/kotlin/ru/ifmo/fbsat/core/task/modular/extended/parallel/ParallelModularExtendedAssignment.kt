package ru.ifmo.fbsat.core.task.modular.extended.parallel

// @Suppress("LocalVariableName")
// fun ParallelModularExtendedAssignment.toAutomaton(): ParallelModularAutomaton {
//     val modules = MultiArray.create(M) { (m) ->
//         with(modularExtendedAssignment[m]) {
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
//                     ru.ifmo.fbsat.core.automaton.BinaryAlgorithm(
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
//                     ParseTreeGuard(
//                         nodeType = MultiArray.create(P) { (p) -> nodeType[c, k, p] },
//                         terminal = IntMultiArray.create(P) { (p) -> nodeInputVariable[c, k, p] },
//                         parent = IntMultiArray.create(P) { (p) -> nodeParent[c, k, p] },
//                         childLeft = IntMultiArray.create(P) { (p) -> nodeChild[c, k, p] },
//                         childRight = IntMultiArray.create(P) { (p) ->
//                             if (nodeType[c, k, p] in setOf(NodeType.AND, NodeType.OR))
//                                 nodeChild[c, k, p] + 1
//                             else
//                                 0
//                         },
//                         inputNames = scenarioTree.inputNames
//                     )
//                 }
//             )
//         }
//     }
//
//     return ParallelModularAutomaton(modules, moduleControllingOutputVariable)
// }
