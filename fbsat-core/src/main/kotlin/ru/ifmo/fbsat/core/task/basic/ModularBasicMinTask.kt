package ru.ifmo.fbsat.core.task.basic

import ru.ifmo.fbsat.core.automaton.ModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeIt
import java.io.File
import kotlin.properties.Delegates

interface ModularBasicMinTask {
    val scenarioTree: ScenarioTree
    val numberOfModules: Int // M
    val numberOfStates: Int? // C, search if null
    val maxOutgoingTransitions: Int? // K, K=C if null
    val initialMaxTransitions: Int? // T_init, unconstrained if null
    val outDir: File

    fun infer(): ModularAutomaton?

    companion object {
        @JvmStatic
        fun create(
            scenarioTree: ScenarioTree,
            numberOfModules: Int, // M
            numberOfStates: Int? = null, // C, search if null
            maxOutgoingTransitions: Int? = null, // K, K=C if null
            initialMaxTransitions: Int? = null, // T_init, unconstrained if null
            outDir: File,
            solverProvider: () -> Solver,
            isOnlyC: Boolean = false
        ): ModularBasicMinTask = ModularBasicMinTaskImpl(
            scenarioTree = scenarioTree,
            numberOfModules = numberOfModules,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            initialMaxTransitions = initialMaxTransitions,
            outDir = outDir,
            solverProvider = solverProvider,
            isOnlyC = isOnlyC
        )
    }
}

private class ModularBasicMinTaskImpl(
    override val numberOfModules: Int, // M
    override val scenarioTree: ScenarioTree,
    override val numberOfStates: Int?, // C, search if null
    override val maxOutgoingTransitions: Int?, // K, =C if null
    override val initialMaxTransitions: Int?, // T_init, unconstrained if null
    override val outDir: File,
    private val solverProvider: () -> Solver,
    private val isOnlyC: Boolean
) : ModularBasicMinTask {
    init {
        require(!(numberOfStates == null && maxOutgoingTransitions != null)) {
            "do not specify only K"
        }
    }

    @Suppress("LocalVariableName")
    override fun infer(): ModularAutomaton? {
        var best: ModularAutomaton? = null
        var task: ModularBasicTask by Delegates.notNull()

        if (numberOfStates == null) {
            log.info("ModularBasicMinTask: searching for minimal C...")
            for (C in 1..50) {
                task = ModularBasicTask.create(
                    scenarioTree = scenarioTree,
                    numberOfModules = numberOfModules,
                    numberOfStates = C,
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxTransitions = initialMaxTransitions,
                    outDir = outDir,
                    solverProvider = solverProvider,
                    autoFinalize = false
                )
                val (automaton, runningTime) = timeIt { task.infer() }
                if (automaton != null) {
                    log.success("ModularBasicMinTask: C = $C -> SAT in %.2f s".format(runningTime))
                    log.info("ModularBasicMinTask: minimal C = $C")
                    best = automaton
                    break
                } else {
                    log.failure("ModularBasicMinTask: C = $C -> UNSAT in %.2f s".format(runningTime))
                    task.finalize2()
                }
            }
        } else {
            val C = numberOfStates
            log.info("ModularBasicMinTask: using provided C = $C")
            task = ModularBasicTask.create(
                scenarioTree = scenarioTree,
                numberOfModules = numberOfModules,
                numberOfStates = numberOfStates,
                maxOutgoingTransitions = maxOutgoingTransitions,
                maxTransitions = initialMaxTransitions,
                outDir = outDir,
                solverProvider = solverProvider,
                autoFinalize = false
            )
            val (automaton, runningTime) = timeIt { task.infer() }
            if (automaton != null)
                log.success("ModularBasicMinTask: C = $C -> SAT in %.2f s".format(runningTime))
            else
                log.failure("ModularBasicMinTask: C = $C -> UNSAT in %.2f s".format(runningTime))
            best = automaton
        }

        if (!isOnlyC && best != null) {
            log.info("ModularBasicMinTask: searching for minimal T...")
            while (true) {
                val T = best!!.numberOfTransitions - 1
                task = task.reuse(T)
                val (automaton, runningTime) = timeIt { task.infer() }
                if (automaton != null) {
                    log.success(
                        "ModularBasicMinTask: T = $T -> ${automaton.numberOfTransitions} in %.2f s"
                            .format(runningTime)
                    )
                    best = automaton
                } else {
                    log.failure("ModularBasicMinTask: T = $T -> UNSAT in %.2f".format(runningTime))
                    log.info("ModularBasicMinTask: minimal T = ${best.numberOfTransitions}")
                    break
                }
            }
        }

        task.finalize2()

        return best
    }
}
