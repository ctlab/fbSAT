package ru.ifmo.fbsat.task.basicmin

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.task.basic.BasicTask
import ru.ifmo.fbsat.utils.log
import ru.ifmo.fbsat.utils.timeIt
import java.io.File
import kotlin.properties.Delegates

class BasicMinTask(
    val scenarioTree: ScenarioTree,
    val numberOfStates: Int?, // C, search if null
    val maxOutgoingTransitions: Int?, // K, =C if null
    val initialMaxTransitions: Int?, // T_init, unconstrained if null
    val outDir: File,
    private val solverProvider: () -> Solver
) {
    init {
        require(!(numberOfStates == null && maxOutgoingTransitions != null)) {
            "do not specify only K"
        }
    }

    @Suppress("LocalVariableName")
    fun infer(isOnlyC: Boolean = false): Automaton? {
        log.debug { "BasicMinTask::infer(${if (isOnlyC) "onlyC" else ""})" }

        var best: Automaton? = null
        var task: BasicTask by Delegates.notNull()

        if (numberOfStates == null) {
            log.info("BasicMinTask: searching for minimal C...")
            for (C in 1..20) {
                task = BasicTask(
                    scenarioTree = scenarioTree,
                    numberOfStates = C,
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxTransitions = initialMaxTransitions,
                    outDir = outDir,
                    solver = solverProvider(),
                    autoFinalize = false
                )
                val (automaton, runningTime) = timeIt { task.infer() }
                if (automaton != null) {
                    log.success("BasicMinTask: C = $C -> SAT in %.2f s".format(runningTime))
                    log.info("BasicMinTask: minimal C = $C")
                    best = automaton
                    break
                } else {
                    log.failure("BasicMinTask: C = $C -> UNSAT in %.2f s".format(runningTime))
                    task.finalize2()
                }
            }
        } else {
            val C = numberOfStates
            log.info("BasicMinTask: using provided C = $C")
            task = BasicTask(
                scenarioTree = scenarioTree,
                numberOfStates = numberOfStates,
                maxOutgoingTransitions = maxOutgoingTransitions,
                maxTransitions = initialMaxTransitions,
                outDir = outDir,
                solver = solverProvider(),
                autoFinalize = false
            )
            val (automaton, runningTime) = timeIt { task.infer() }
            if (automaton != null)
                log.success("BasicMinTask: C = $C -> SAT in %.2f s".format(runningTime))
            else
                log.failure("BasicMinTask: C = $C -> UNSAT in %.2f s".format(runningTime))
            best = automaton
        }

        if (!isOnlyC && best != null) {
            log.info("BasicMinTask: searching for minimal T...")
            while (true) {
                val T = best!!.numberOfTransitions - 1
                task = task.reuse(T)
                val (automaton, runningTime) = timeIt { task.infer() }
                if (automaton != null) {
                    log.success(
                        "BasicMinTask: T = $T -> SAT in %.2f with T = ${automaton.numberOfTransitions}"
                            .format(runningTime)
                    )
                    best = automaton
                } else {
                    log.failure("BasicMinTask: T = $T -> UNSAT in %.2f".format(runningTime))
                    log.info("BasicMinTask: minimal T = ${best.numberOfTransitions}")
                    break
                }
            }
            log.info("BasicMinTask: minimal T = ${best?.numberOfTransitions}")
        }

        task.finalize2()

        return best
    }
}
