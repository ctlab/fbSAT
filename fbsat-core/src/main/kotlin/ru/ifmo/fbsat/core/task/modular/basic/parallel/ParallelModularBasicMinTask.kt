package ru.ifmo.fbsat.core.task.modular.basic.parallel

import ru.ifmo.fbsat.core.automaton.ModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeIt
import java.io.File
import kotlin.properties.Delegates

class ParallelModularBasicMinTask(
    val numberOfModules: Int, // M
    val scenarioTree: ScenarioTree,
    val numberOfStates: Int? = null, // C, search if null
    val maxOutgoingTransitions: Int? = null, // K, =C if null
    private val initialMaxTransitions: Int? = null, // T_init, unconstrained if null
    val outDir: File,
    val solverProvider: () -> Solver,
    val isEncodeReverseImplication: Boolean = true,
    val isOnlyC: Boolean = false
) {
    init {
        require(!(numberOfStates == null && maxOutgoingTransitions != null)) { "do not specify only K" }
    }

    @Suppress("LocalVariableName")
    fun infer(): ModularAutomaton? {
        var best: ModularAutomaton? = null
        var task: ParallelModularBasicTask by Delegates.notNull()

        if (numberOfStates == null) {
            log.info("ParallelModularBasicMinTask: searching for minimal C...")
            for (C in 1..50) {
                task = ParallelModularBasicTask(
                    scenarioTree = scenarioTree,
                    numberOfModules = numberOfModules,
                    numberOfStates = C,
                    maxOutgoingTransitions = maxOutgoingTransitions ?: C,
                    maxTransitions = initialMaxTransitions,
                    outDir = outDir,
                    solver = solverProvider(),
                    autoFinalize = false,
                    isEncodeReverseImplication = isEncodeReverseImplication
                )
                val (automaton, runningTime) = timeIt { task.infer() }
                if (automaton != null) {
                    log.success("ParallelModularBasicMinTask: C = $C -> SAT in %.2f s".format(runningTime))
                    log.info("ParallelModularBasicMinTask: minimal C = $C")
                    best = automaton
                    break
                } else {
                    log.failure("ParallelModularBasicMinTask: C = $C -> UNSAT in %.2f s".format(runningTime))
                    task.finalize2()
                }
            }
        } else {
            val C = numberOfStates
            log.info("ParallelModularBasicMinTask: using provided C = $C")
            task = ParallelModularBasicTask(
                scenarioTree = scenarioTree,
                numberOfModules = numberOfModules,
                numberOfStates = C,
                maxOutgoingTransitions = maxOutgoingTransitions ?: C,
                maxTransitions = initialMaxTransitions,
                outDir = outDir,
                solver = solverProvider(),
                autoFinalize = false,
                isEncodeReverseImplication = isEncodeReverseImplication
            )
            val (automaton, runningTime) = timeIt { task.infer() }
            if (automaton != null)
                log.success("ParallelModularBasicMinTask: C = $C -> SAT in %.2f s".format(runningTime))
            else
                log.failure("ParallelModularBasicMinTask: C = $C -> UNSAT in %.2f s".format(runningTime))
            best = automaton
        }

        if (!isOnlyC && best != null) {
            log.info("ParallelModularBasicMinTask: searching for minimal T...")
            while (true) {
                val T = best!!.numberOfTransitions - 1
                task.updateCardinality(T)
                val (automaton, runningTime) = timeIt { task.infer() }
                if (automaton != null) {
                    log.success(
                        "ParallelModularBasicMinTask: T = $T -> ${automaton.numberOfTransitions} in %.2f s"
                            .format(runningTime)
                    )
                    best = automaton
                } else {
                    log.failure("ParallelModularBasicMinTask: T = $T -> UNSAT in %.2f".format(runningTime))
                    log.info("ParallelModularBasicMinTask: minimal T = ${best.numberOfTransitions}")
                    break
                }
            }
        }

        task.finalize2()

        return best
    }
}
