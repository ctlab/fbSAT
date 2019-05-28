package ru.ifmo.fbsat.core.task.basic

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeIt
import java.io.File
import kotlin.properties.Delegates

interface BasicMinTask {
    val scenarioTree: ScenarioTree
    val numberOfStates: Int? // C, search if null
    val maxOutgoingTransitions: Int? // K, K=C if null
    val initialMaxTransitions: Int? // T_init, unconstrained if null
    val outDir: File

    fun infer(): Automaton?

    companion object {
        @JvmStatic
        fun create(
            scenarioTree: ScenarioTree,
            numberOfStates: Int? = null, // C, search if null
            maxOutgoingTransitions: Int? = null, // K, K=C if null
            initialMaxTransitions: Int? = null, // T_init, unconstrained if null
            outDir: File,
            solverProvider: () -> Solver,
            isEncodeReverseImplication: Boolean = true,
            isOnlyC: Boolean = false
        ): BasicMinTask = BasicMinTaskImpl(
            scenarioTree = scenarioTree,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            initialMaxTransitions = initialMaxTransitions,
            outDir = outDir,
            solverProvider = solverProvider,
            isEncodeReverseImplication = isEncodeReverseImplication,
            isOnlyC = isOnlyC
        )
    }
}

private class BasicMinTaskImpl(
    override val scenarioTree: ScenarioTree,
    override val numberOfStates: Int?, // C, search if null
    override val maxOutgoingTransitions: Int?, // K, =C if null
    override val initialMaxTransitions: Int?, // T_init, unconstrained if null
    override val outDir: File,
    private val solverProvider: () -> Solver,
    private val isEncodeReverseImplication: Boolean,
    private val isOnlyC: Boolean
) : BasicMinTask {
    init {
        require(!(numberOfStates == null && maxOutgoingTransitions != null)) {
            "do not specify only K"
        }
    }

    @Suppress("LocalVariableName")
    override fun infer(): Automaton? {
        var best: Automaton? = null
        var task: BasicTask by Delegates.notNull()

        if (numberOfStates == null) {
            log.info("BasicMinTask: searching for minimal C...")
            for (C in 1..50) {
                task = BasicTask.create(
                    scenarioTree = scenarioTree,
                    numberOfStates = C,
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxTransitions = initialMaxTransitions,
                    outDir = outDir,
                    solverProvider = solverProvider,
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
            val C = numberOfStates
            log.info("BasicMinTask: using provided C = $C")
            task = BasicTask.create(
                scenarioTree = scenarioTree,
                numberOfStates = numberOfStates,
                maxOutgoingTransitions = maxOutgoingTransitions,
                maxTransitions = initialMaxTransitions,
                outDir = outDir,
                solverProvider = solverProvider,
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
                val T = best!!.numberOfTransitions - 1
                task = task.reuse(T)
                val (automaton, runningTime) = timeIt { task.infer() }
                if (automaton != null) {
                    log.success(
                        "BasicMinTask: T = $T -> ${automaton.numberOfTransitions} in %.2f s"
                            .format(runningTime)
                    )
                    best = automaton
                } else {
                    log.failure("BasicMinTask: T = $T -> UNSAT in %.2f".format(runningTime))
                    log.info("BasicMinTask: minimal T = ${best.numberOfTransitions}")
                    break
                }
            }
        }

        task.finalize2()

        return best
    }
}
