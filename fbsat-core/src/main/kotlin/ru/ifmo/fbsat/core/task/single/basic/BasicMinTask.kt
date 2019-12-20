package ru.ifmo.fbsat.core.task.single.basic

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeIt
import java.io.File

@Suppress("MemberVisibilityCanBePrivate")
class BasicMinTask(
    val scenarioTree: ScenarioTree,
    val numberOfStates: Int? = null, // C, search if null
    val maxOutgoingTransitions: Int? = null, // K, =C if null
    val initialMaxTransitions: Int? = null, // T_init, unconstrained if null
    val outDir: File,
    val solverProvider: () -> Solver,
    val isEncodeReverseImplication: Boolean = true,
    val isOnlyC: Boolean = false
) {
    init {
        require(!(numberOfStates == null && maxOutgoingTransitions != null)) { "do not specify only K" }
    }

    @Suppress("LocalVariableName")
    fun infer(): Automaton? {
        var best: Automaton? = null
        lateinit var task: BasicTask

        if (numberOfStates == null) {
            log.info("BasicMinTask: searching for minimal C...")
            for (C in 1..50) {
                task = BasicTask(
                    scenarioTree = scenarioTree,
                    numberOfStates = C,
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxTransitions = initialMaxTransitions,
                    outDir = outDir,
                    solver = solverProvider(),
                    autoFinalize = false,
                    isEncodeReverseImplication = isEncodeReverseImplication
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
            val C: Int = numberOfStates
            log.info("BasicMinTask: using provided C = $C")
            task = BasicTask(
                scenarioTree = scenarioTree,
                numberOfStates = C,
                maxOutgoingTransitions = maxOutgoingTransitions,
                maxTransitions = initialMaxTransitions,
                outDir = outDir,
                solver = solverProvider(),
                autoFinalize = false,
                isEncodeReverseImplication = isEncodeReverseImplication
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
                val T = best!!.numberOfTransitions
                task.updateCardinalityLessThan(T)
                val (automaton, runningTime) = timeIt { task.infer() }
                if (automaton != null) {
                    log.success(
                        "BasicMinTask: T < $T -> ${automaton.numberOfTransitions} in %.2f s"
                            .format(runningTime)
                    )
                    best = automaton
                } else {
                    log.failure("BasicMinTask: T < $T -> UNSAT in %.2f".format(runningTime))
                    log.info("BasicMinTask: minimal T = ${best.numberOfTransitions}")
                    break
                }
            }
        }

        task.finalize2()

        return best
    }
}
