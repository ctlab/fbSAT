package ru.ifmo.fbsat.core.task.modular.extended.consecutive

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.automaton.ConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.constraints.declareConsecutiveModularGuardConditionsBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareConsecutiveModularGuardConditionsConstraints
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.solver.implyAnd
import ru.ifmo.fbsat.core.solver.implyImply
import ru.ifmo.fbsat.core.task.modular.basic.consecutive.ConsecutiveModularBasicTask
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeSince
import java.io.File

@Suppress("LocalVariableName")
class ConsecutiveModularExtendedTask(
    scenarioTree: ScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxGuardSize: Int, // P
    maxTotalGuardsSize: Int? = null, // N, unconstrained if null
    val outDir: File,
    val solver: Solver,
    val autoFinalize: Boolean = true,
    isEncodeReverseImplication: Boolean = true
) {
    private val basicTask: ConsecutiveModularBasicTask =
        ConsecutiveModularBasicTask(
            scenarioTree = scenarioTree,
            numberOfModules = numberOfModules,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxTransitions = null,
            outDir = outDir,
            solver = solver,
            autoFinalize = false,
            isEncodeReverseImplication = isEncodeReverseImplication
        )

    val vars: ConsecutiveModularExtendedVariables
    internal val cardinality: Cardinality

    init {
        val timeStart = PerformanceCounter.reference
        val nvarStart = solver.numberOfVariables
        val nconStart = solver.numberOfClauses

        with(solver) {
            /* Variables */
            vars = declareConsecutiveModularExtendedVariables(
                basicVars = basicTask.vars,
                P = maxGuardSize
            )

            /* Cardinality */
            cardinality = declareCardinality {
                with(vars) {
                    for (m in 1..M) with(modularExtendedVariables[m]) {
                        for (c in 1..C)
                            for (k in 1..K)
                                for (p in 1..P)
                                    yield(nodeType[c, k, p] neq NodeType.NONE)
                    }
                }
            }

            /* Constraints */
            declareConsecutiveModularGuardConditionsConstraints(vars)
            if (Globals.IS_BFS_GUARD) declareConsecutiveModularGuardConditionsBfsConstraints(vars)
            declareAdhocConstraints()
        }

        updateCardinalityLessThan(maxTotalGuardsSize?.let { it + 1 })

        val nvarDiff = solver.numberOfVariables - nvarStart
        val nconDiff = solver.numberOfClauses - nconStart
        log.info(
            "ConsecutiveModularExtendedTask: Done declaring variables ($nvarDiff) and constraints ($nconDiff) in %.2f s.".format(
                timeSince(timeStart).seconds
            )
        )
    }

    private fun Solver.declareAdhocConstraints() {
        comment("A. AD-HOCs")

        with(vars) {
            comment("A.1. Forbid double negation")
            // (nodeType[p] = NOT) & (nodeChild[p] = ch) => (nodeType[ch] != NOT)
            for (m in 1..M) with(modularExtendedVariables[m]) {
                for (c in 1..C)
                    for (k in 1..K)
                        for (p in 1 until P)
                            for (ch in (p + 1)..P)
                                implyImply(
                                    nodeType[c, k, p] eq NodeType.NOT,
                                    nodeChild[c, k, p] eq ch,
                                    nodeType[c, k, ch] neq NodeType.NOT
                                )
            }

            comment("A.2. Distinct transitions")
            // TODO: Distinct transitions

            if (Globals.IS_FORBID_OR) {
                comment("A.3. Forbid ORs")
                for (m in 1..M) with(modularExtendedVariables[m]) {
                    for (c in 1..C)
                        for (k in 1..K)
                            for (p in 1..P)
                                clause(nodeType[c, k, p] neq NodeType.OR)
                }
            }

            if (Globals.IS_ENCODE_TERMINALS_ORDER) {
                comment("A.4. Terminals order")
                // terminal[p, x] => AND_{p'<p, x'>=x}( ~terminal[r_, x_] )
                for (m in 1..M) with(modularExtendedVariables[m]) {
                    for (c in 1..C)
                        for (k in 1..K)
                            for (p in 1..P)
                                for (x in 1..X)
                                    implyAnd(nodeInputVariable[c, k, p] eq x, sequence {
                                        for (p_ in 1 until p)
                                            for (x_ in x..X)
                                                yield(nodeInputVariable[c, k, p_] neq x_)
                                    })
                }
            }

            if (Globals.IS_ENCODE_TERMINALS_MINI_ORDER) {
                // Note: this constraint seems to be very expensive, but does not provide visible speed-up
                comment("A.5. Terminals mini-order: AND/OR children-terminals order")
                // (nodeType[p] = AND/OR) & (nodeChild[p] = ch) & (nodeType[ch] = TERMINAL) & (nodeType[ch+1] = TERMINAL) => (nodeInputVariable[ch] < nodeInputVariable[ch+1])
                for (m in 1..M) with(modularExtendedVariables[m]) {
                    for (c in 1..C)
                        for (k in 1..K)
                            for (p in 1..P)
                                for (t in listOf(NodeType.AND, NodeType.OR))
                                    for (ch in (p + 1) until P)
                                        for (x in 1..X)
                                            for (x_ in 1..x)
                                                clause(
                                                    nodeType[c, k, p] neq t,
                                                    nodeChild[c, k, p] neq ch,
                                                    nodeType[c, k, ch] neq NodeType.NOT,
                                                    nodeType[c, k, ch + 1] neq NodeType.NOT,
                                                    nodeInputVariable[c, k, ch] neq x,
                                                    nodeInputVariable[c, k, ch + 1] neq x_
                                                )
                }
            }
        }
    }

    fun updateCardinalityLessThan(newMaxTotalGuardsSize: Int?) {
        cardinality.updateUpperBoundLessThan(newMaxTotalGuardsSize)
    }

    fun infer(): ConsecutiveModularAutomaton? {
        val rawAssignment = solver.solve()
        if (autoFinalize) finalize2()
        if (rawAssignment == null) return null

        val assignment = ConsecutiveModularExtendedAssignment.fromRaw(rawAssignment, vars)
        val automaton = assignment.toAutomaton()

        with(vars) {
            // check(
            //     automaton.checkMapping(
            //         scenarios = scenarioTree.scenarios,
            //         mapping = assignment.mapping
            //     )
            // ) { "Positive mapping mismatch" }
        }

        return automaton
    }

    fun finalize2() {
        basicTask.finalize2()
        // Note: basicTask already finalizes the solver, so it is not necessary to do it here again
        // solver.finalize2()
    }
}
