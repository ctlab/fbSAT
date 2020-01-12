package ru.ifmo.fbsat.core.task.single.extended

import com.soywiz.klock.measureTimeWithResult
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.single.basic.BasicMinTask
import ru.ifmo.fbsat.core.utils.log
import java.io.File

class ExtendedMinTask(
    val scenarioTree: ScenarioTree,
    val numberOfStates: Int?, // C, search if null
    val maxOutgoingTransitions: Int?, // K, K=C if null
    val maxGuardSize: Int, // P
    val initialMaxTotalGuardsSize: Int?, // N_init, unconstrained if null
    val outDir: File,
    val solverProvider: () -> Solver,
    val isEncodeReverseImplication: Boolean = true
) {
    init {
        require(!(numberOfStates == null && maxOutgoingTransitions != null)) { "do not specify only K" }
    }

    @Suppress("LocalVariableName")
    fun infer(): Automaton? {
        log.debug { "ExtendedMinTask::infer()" }

        val C = numberOfStates ?: run {
            log.info("ExtMinTask: delegating to BasicMinTask to search for minimal number of states C...")
            val task = BasicMinTask(
                scenarioTree = scenarioTree,
                outDir = outDir,
                solverProvider = solverProvider,
                isEncodeReverseImplication = true,
                isOnlyC = true
            )
            val automaton = task.infer() ?: return null
            automaton.numberOfStates
        }

        log.info("ExtMinTask: searching for minimal N...")
        // TODO: (`reuse`) pass through the last BasicTask (Note: only if isEncodeReverseImplication is true, because if is false, then BasicTask can't be reused)
        val task = ExtendedTask(
            scenarioTree = scenarioTree,
            numberOfStates = C,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxGuardSize = maxGuardSize,
            maxTotalGuardsSize = initialMaxTotalGuardsSize,
            outDir = outDir,
            solver = solverProvider(),
            autoFinalize = false,
            isEncodeReverseImplication = isEncodeReverseImplication
        )
        val (automaton, runningTime) = measureTimeWithResult { task.infer() }
        if (automaton != null) {
            log.success(
                "ExtMinTask: initial N = $initialMaxTotalGuardsSize -> ${automaton.totalGuardsSize} in %.2f s.".format(
                    runningTime.seconds
                )
            )
        } else {
            log.failure("ExtMinTask: initial N = $initialMaxTotalGuardsSize -> UNSAT in %.2f s.".format(runningTime.seconds))
        }
        var best: Automaton? = automaton

        if (best != null) {
            while (true) {
                val N = best!!.totalGuardsSize
                task.updateCardinalityLessThan(N)
                val (automaton, runningTime) = measureTimeWithResult { task.infer() }
                if (automaton != null) {
                    log.success("ExtMinTask: N < $N -> ${automaton.totalGuardsSize} in %.2f s.".format(runningTime.seconds))
                    best = automaton
                } else {
                    log.failure("ExtMinTask: N < $N -> UNSAT in %.2f s.".format(runningTime.seconds))
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
