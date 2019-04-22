package ru.ifmo.fbsat.core.task.extended

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.basic.BasicMinTask
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeIt
import java.io.File

interface ExtendedMinTask {
    val scenarioTree: ScenarioTree
    val numberOfStates: Int? // C, search if null
    val maxOutgoingTransitions: Int? // K, =C if null
    val maxGuardSize: Int // P
    val initialMaxTotalGuardsSize: Int? // N_init, unconstrained if null
    val outDir: File

    fun infer(): Automaton?

    companion object {
        @JvmStatic
        fun create(
            scenarioTree: ScenarioTree,
            numberOfStates: Int?, // C, search if null
            maxOutgoingTransitions: Int?, // K, K=C if null
            maxGuardSize: Int, // P
            initialMaxTotalGuardsSize: Int?, // N_init, unconstrained if null
            outDir: File,
            solverProvider: () -> Solver,
            isEncodeReverseImplication: Boolean = true
        ): ExtendedMinTask = ExtendedMinTaskImpl(
            scenarioTree = scenarioTree,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxGuardSize = maxGuardSize,
            initialMaxTotalGuardsSize = initialMaxTotalGuardsSize,
            outDir = outDir,
            solverProvider = solverProvider,
            isEncodeReverseImplication = isEncodeReverseImplication
        )
    }
}

private class ExtendedMinTaskImpl(
    override val scenarioTree: ScenarioTree,
    override val numberOfStates: Int?, // C, search if null
    override val maxOutgoingTransitions: Int?, // K, K=C if null
    override val maxGuardSize: Int, // P
    override val initialMaxTotalGuardsSize: Int?, // N_init, unconstrained if null
    override val outDir: File,
    private val solverProvider: () -> Solver,
    private val isEncodeReverseImplication: Boolean
) : ExtendedMinTask {
    init {
        require(!(numberOfStates == null && maxOutgoingTransitions != null)) {
            "do not specify only K"
        }
    }

    @Suppress("LocalVariableName")
    override fun infer(): Automaton? {
        log.debug { "ExtendedMinTask::infer()" }

        val C = numberOfStates ?: run {
            log.info("ExtMinTask: delegating to BasicMinTask to search for minimal number of states C...")
            val task = BasicMinTask.create(
                scenarioTree = scenarioTree,
                outDir = outDir,
                solverProvider = solverProvider,
                isOnlyC = true
            )
            val automaton = task.infer() ?: return null
            automaton.numberOfStates
        }

        log.info("ExtMinTask: searching for minimal N...")
        var task = ExtendedTask.create(
            scenarioTree = scenarioTree,
            numberOfStates = C,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxGuardSize = maxGuardSize,
            maxTotalGuardsSize = initialMaxTotalGuardsSize,
            outDir = outDir,
            solverProvider = solverProvider,
            autoFinalize = false,
            isEncodeReverseImplication = isEncodeReverseImplication
        )
        val (automaton, runningTime) = timeIt { task.infer() }
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
                task = task.reuse(N)
                val (automaton, runningTime) = timeIt { task.infer() }
                if (automaton != null) {
                    log.success("ExtMinTask: N = $N -> ${automaton.getN()} in %.2f s".format(runningTime))
                    if (automaton.getN() > N) {
                        automaton.pprint()
                        error("Automaton has more parse tree nodes than expected")
                    }
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
