package ru.ifmo.fbsat.core.task.single.extforest

// fun ExtForestAssignment.toAutomaton(): Automaton =
//     Automaton(scenarioTree).endow(
//         C = C, K = K,
//         stateOutputEvent = { c ->
//             stateOutputEvent[c].let { o ->
//                 if (o == 0) null else scenarioTree.outputEvents[o - 1]
//             }
//         },
//         stateAlgorithm = { c ->
//             BinaryAlgorithm(
//                 algorithm0 = BooleanArray(Z) { z0 -> stateAlgorithmBot[c, z0 + 1] },
//                 algorithm1 = BooleanArray(Z) { z0 -> stateAlgorithmTop[c, z0 + 1] }
//             )
//         },
//         transitionDestination = { c, k ->
//             transitionDestination[c, k]
//         },
//         transitionInputEvent = { c, k ->
//             scenarioTree.inputEvents[transitionInputEvent[c, k] - 1]
//         },
//         transitionGuard = { c, k ->
//             val root = ck2p(c, k, K)
//             check(nodeType[root] != NodeType.NONE)
//             val nodes: MutableList<Int> = mutableListOf()
//             val queue: Deque<Int> = ArrayDeque(listOf(root))
//             while (queue.isNotEmpty()) {
//                 val p = queue.removeFirst()
//                 nodes.add(p)
//                 for (ch in (C * K + 1)..P) {
//                     if (nodeParent[ch] == p) {
//                         queue.addLast(ch)
//                     }
//                 }
//             }
//             check(nodes.isNotEmpty()) { "Empty parse tree for c = $c, k = $k" }
//             val G = nodes.size
//             ParseTreeGuard(
//                 nodeType = MultiArray.new(G) { (p) ->
//                     nodeType[nodes[p - 1]]
//                 },
//                 terminal = IntMultiArray.new(G) { (p) ->
//                     nodeInputVariable[nodes[p - 1]]
//                 },
//                 parent = IntMultiArray.new(G) { (p) ->
//                     nodeParent[nodes[p - 1]].let { if (it != 0) nodes.indexOf(it) + 1 else 0 }
//                 },
//                 childLeft = IntMultiArray.new(G) { (p) ->
//                     nodeChild[nodes[p - 1]].let { if (it != 0) nodes.indexOf(it) + 1 else 0 }
//                 },
//                 childRight = IntMultiArray.new(G) { (p) ->
//                     if (nodeType[nodes[p - 1]] in setOf(NodeType.AND, NodeType.OR))
//                         nodes.indexOf(nodeChild[nodes[p - 1]]) + 2
//                     else
//                         0
//                 },
//                 inputNames = scenarioTree.inputNames
//             )
//         }
//     )
