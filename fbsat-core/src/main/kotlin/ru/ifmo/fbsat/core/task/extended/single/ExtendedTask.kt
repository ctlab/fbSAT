package ru.ifmo.fbsat.core.task.extended.single

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.Solver.Companion.falseVariable
import ru.ifmo.fbsat.core.task.basic.single.BasicTask
import ru.ifmo.fbsat.core.task.declareAndOrNodesConstraints
import ru.ifmo.fbsat.core.task.declareGuardBfsConstraints
import ru.ifmo.fbsat.core.task.declareNodeTypeConstraints
import ru.ifmo.fbsat.core.task.declareNoneTypeNodesConstraints
import ru.ifmo.fbsat.core.task.declareNotNodesConstraints
import ru.ifmo.fbsat.core.task.declareParentAndChildrenConstraints
import ru.ifmo.fbsat.core.task.declareTerminalsConstraints
import ru.ifmo.fbsat.core.utils.Globals
import java.io.File

@Suppress("MemberVisibilityCanBePrivate", "LocalVariableName", "UnnecessaryVariable")
class ExtendedTask(
    scenarioTree: ScenarioTree,
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxGuardSize: Int, // P
    maxTotalGuardsSize: Int? = null, // N, unconstrained if null
    val outDir: File,
    val solver: Solver,
    val autoFinalize: Boolean = true,
    isEncodeReverseImplication: Boolean = true
) {
    private val basicTask: BasicTask =
        BasicTask(
            scenarioTree = scenarioTree,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxTransitions = null,
            outDir = outDir,
            solver = solver,
            autoFinalize = false,
            isEncodeReverseImplication = isEncodeReverseImplication
        )

    val vars: ExtendedVariables

    init {
        with(solver) {
            with(basicTask.vars) {
                // Constants
                val P = maxGuardSize

                // Variables
                val nodeType = newArray(C, K, P, NodeType.values().size) { (_, _, _, nt) ->
                    if (Globals.IS_FORBID_OR && nt == NodeType.OR.value) falseVariable else newVariable()
                }
                val terminal = newArray(C, K, P, X + 1)
                val parent = newArray(C, K, P, P + 1) { (_, _, p, par) ->
                    if (par < p || par == P + 1) newVariable() else falseVariable
                }
                val child = newArray(C, K, P, P + 1) { (_, _, p, ch) ->
                    if (ch >= p + 1 || ch == P + 1) newVariable() else falseVariable
                }
                val nodeValue = newArray(C, K, P, U) { (c, k, p, u) ->
                    if (p == 1) rootValue[c, k, u] else newVariable()
                }

                vars = ExtendedVariables(
                    basicVars = basicTask.vars,
                    P = P,
                    nodeType = nodeType,
                    terminal = terminal,
                    parent = parent,
                    child = child,
                    nodeValue = nodeValue
                )
            }

            with(vars) {
                // Constraints
                declareNodeTypeConstraints(
                    C = C, K = K, P = P,
                    transition = transition,
                    nodeType = nodeType
                )
                declareParentAndChildrenConstraints(
                    C = C, K = K, P = P,
                    nodeType = nodeType,
                    parent = parent,
                    child = child
                )
                declareNoneTypeNodesConstraints(
                    C = C, K = K, P = P, U = U,
                    nodeType = nodeType,
                    parent = parent,
                    child = child,
                    nodeValue = nodeValue
                )
                declareTerminalsConstraints(
                    scenarioTree = scenarioTree,
                    C = C, K = K, P = P, U = U, X = X,
                    nodeType = nodeType,
                    terminal = terminal,
                    child = child,
                    nodeValue = nodeValue
                )
                declareAndOrNodesConstraints(
                    C = C, K = K, P = P, U = U,
                    nodeType = nodeType,
                    parent = parent,
                    child = child,
                    nodeValue = nodeValue
                )
                declareNotNodesConstraints(
                    C = C, K = K, P = P, U = U,
                    nodeType = nodeType,
                    parent = parent,
                    child = child,
                    nodeValue = nodeValue
                )
                if (Globals.IS_BFS_GUARD) {
                    declareGuardBfsConstraints(
                        C = C, K = K, P = P,
                        parent = parent
                    )
                }
                declareAdhocConstraints(
                    C = C, K = K, P = P,
                    nodeType = nodeType,
                    child = child
                )
            }
        }

        updateCardinality(maxTotalGuardsSize)
    }

    private fun Solver.declareAdhocConstraints(
        C: Int,
        K: Int,
        P: Int,
        nodeType: IntMultiArray,
        child: IntMultiArray
    ) {
        comment("A. AD-HOCs")

        comment("A.1. Forbid double negation")
        // nodetype[p, NOT] & child_left[p, ch] => ~nodetype[ch, NOT]
        for (c in 1..C)
            for (k in 1..K)
                for (p in 1 until P)
                    for (ch in (p + 1)..P)
                        clause(
                            -nodeType[c, k, p, NodeType.NOT.value],
                            -child[c, k, p, ch],
                            -nodeType[c, k, ch, NodeType.NOT.value]
                        )

        comment("A.2. Distinct transitions")
        // TODO: Distinct transitions

        if (Globals.IS_FORBID_OR) {
            comment("A.3. Forbid ORs")
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        clause(-nodeType[c, k, p, NodeType.OR.value])
        }
    }

    fun updateCardinality(newMaxTotalGuardsSize: Int?) {
        with(vars) {
            solver.updateCardinality(newMaxTotalGuardsSize)
        }
    }

    fun infer(): Automaton? {
        val rawAssignment = solver.solve()?.data
        if (autoFinalize) finalize2()
        return rawAssignment?.let { raw ->
            ExtendedAssignment.fromRaw(raw, vars).toAutomaton()
        }
    }

    fun finalize2() {
        basicTask.finalize2()
        // Note: basicTask already finalizes the solver, so it is not necessary to do it here again
        // solver.finalize2()
    }
}
