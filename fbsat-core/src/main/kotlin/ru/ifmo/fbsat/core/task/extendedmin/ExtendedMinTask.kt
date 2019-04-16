package ru.ifmo.fbsat.core.task.extendedmin

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.basicmin.BasicMinTask
import ru.ifmo.fbsat.core.task.extended.ExtendedTask
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeIt
import java.io.File

class ExtendedMinTask(
    val scenarioTree: ScenarioTree,
    val negativeScenarioTree: NegativeScenarioTree?,
    val numberOfStates: Int?, // C, search if null
    val maxOutgoingTransitions: Int?, // K, =C if null
    val maxGuardSize: Int, // P
    val initialMaxTotalGuardsSize: Int?, // N_init, unconstrained if null
    val outDir: File,
    private val solverProvider: () -> Solver,
    private val isForbidLoops: Boolean = true,
    private val isEncodeAutomaton: Boolean = false,
    private val isEncodeTransitionsOrder: Boolean,
    private val isEncodeReverseImplication: Boolean
) {
    init {
        require(!(numberOfStates == null && maxOutgoingTransitions != null)) {
            "do not specify only K"
        }
    }

    @Suppress("LocalVariableName")
    fun infer(): Automaton? {
        log.debug { "ExtendedMinTask::infer()" }

        val C = numberOfStates ?: run {
            log.info("ExtMinTask: delegating to BasicMinTask to search for minimal number of states C...")
            val task = BasicMinTask(
                scenarioTree = scenarioTree,
                numberOfStates = null,
                maxOutgoingTransitions = null,
                initialMaxTransitions = null,
                outDir = outDir,
                solverProvider = solverProvider
            )
            val automaton = task.infer(isOnlyC = true) ?: return null
            return@run automaton.numberOfStates
        }

        log.info("ExtMinTask: searching for minimal N...")
        val task = ExtendedTask(
            scenarioTree = scenarioTree,
            negativeScenarioTree = negativeScenarioTree,
            numberOfStates = C,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxGuardSize = maxGuardSize,
            outDir = outDir,
            solver = solverProvider(),
            isForbidLoops = isForbidLoops,
            isEncodeAutomaton = isEncodeAutomaton,
            isEncodeTransitionsOrder = isEncodeTransitionsOrder,
            isEncodeReverseImplication = isEncodeReverseImplication
        )
        val (automaton, runningTime) = timeIt { task.infer(initialMaxTotalGuardsSize, finalize = false) }
        if (automaton != null)
            log.success(
                "ExtMinTask: initial N = $initialMaxTotalGuardsSize -> ${automaton.getN()} in %.2f s"
                    .format(runningTime)
            )
        else
            log.failure("ExtMinTask: initial N = $initialMaxTotalGuardsSize -> UNSAT in %.2f s".format(runningTime))
        var best: Automaton? = automaton

        if (best != null) {
            while (true) {
                // val N = best!!.totalGuardsSize - 1
                val N = best!!.getN() - 1
                val (automaton, runningTime) = timeIt { task.infer(N, finalize = false) }
                if (automaton != null) {
                    log.success("ExtMinTask: N = $N -> ${automaton.getN()} in %.2f s".format(runningTime))
                    best = automaton
                } else {
                    log.failure("ExtMinTask: N = $N -> UNSAT in %.2f s".format(runningTime))
                    log.info("ExtMinTask: minimal N = ${best.totalGuardsSize}")
                    break
                }
            }
        } else {
            log.error("ExtMinTask: could not infer automaton with C = $C, P = $maxGuardSize, N = $initialMaxTotalGuardsSize")
        }

        task.finalize2()

        return best
    }
}