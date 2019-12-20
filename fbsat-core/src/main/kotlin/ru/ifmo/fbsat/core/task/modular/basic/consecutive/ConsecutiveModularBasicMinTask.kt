package ru.ifmo.fbsat.core.task.modular.basic.consecutive

import ru.ifmo.fbsat.core.automaton.ConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeIt
import java.io.File
import kotlin.properties.Delegates

class ConsecutiveModularBasicMinTask(
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
    fun infer(): ConsecutiveModularAutomaton? {
        var best: ConsecutiveModularAutomaton? = null
        var task: ConsecutiveModularBasicTask by Delegates.notNull()

        if (numberOfStates == null) {
            log.info("ConsecutiveModularBasicMinTask: searching for minimal C...")
            for (C in 1..50) {
                task = ConsecutiveModularBasicTask(
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
                    log.success("ConsecutiveModularBasicMinTask: C = $C -> SAT in %.2f s".format(runningTime))
                    log.info("ConsecutiveModularBasicMinTask: minimal C = $C")
                    best = automaton
                    break
                } else {
                    log.failure("ConsecutiveModularBasicMinTask: C = $C -> UNSAT in %.2f s".format(runningTime))
                    task.finalize2()
                }
            }
        } else {
            val C = numberOfStates
            log.info("ConsecutiveModularBasicMinTask: using provided C = $C")
            task = ConsecutiveModularBasicTask(
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
                log.success("ConsecutiveModularBasicMinTask: C = $C -> SAT in %.2f s".format(runningTime))
            else
                log.failure("ConsecutiveModularBasicMinTask: C = $C -> UNSAT in %.2f s".format(runningTime))
            best = automaton
        }

        if (!isOnlyC && best != null) {
            log.info("ConsecutiveModularBasicMinTask: searching for minimal T...")
            while (true) {
                val T = best!!.numberOfTransitions - 1
                task.updateCardinality(T)
                val (automaton, runningTime) = timeIt { task.infer() }
                if (automaton != null) {
                    log.success(
                        "ConsecutiveModularBasicMinTask: T = $T -> ${automaton.numberOfTransitions} in %.2f s"
                            .format(runningTime)
                    )
                    best = automaton
                } else {
                    log.failure("ConsecutiveModularBasicMinTask: T = $T -> UNSAT in %.2f".format(runningTime))
                    log.info("ConsecutiveModularBasicMinTask: minimal T = ${best.numberOfTransitions}")
                    break
                }
            }
        }

        task.finalize2()

        return best
    }
}
