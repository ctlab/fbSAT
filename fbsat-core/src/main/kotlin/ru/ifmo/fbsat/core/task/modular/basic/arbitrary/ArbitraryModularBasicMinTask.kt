package ru.ifmo.fbsat.core.task.modular.basic.arbitrary

import com.soywiz.klock.measureTimeWithResult
import ru.ifmo.fbsat.core.automaton.ArbitraryModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.utils.log
import java.io.File
import kotlin.properties.Delegates

@Suppress("MemberVisibilityCanBePrivate")
class ArbitraryModularBasicMinTask(
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
    fun infer(): ArbitraryModularAutomaton? {
        var best: ArbitraryModularAutomaton? = null
        var task: ArbitraryModularBasicTask by Delegates.notNull()

        if (numberOfStates == null) {
            log.info("ArbitraryModularBasicMinTask: searching for minimal C...")
            for (C in 1..50) {
                task = ArbitraryModularBasicTask(
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
                val (automaton, runningTime) = measureTimeWithResult { task.infer() }
                if (automaton != null) {
                    log.success("ArbitraryModularBasicMinTask: C = $C -> SAT in %.2f s.".format(runningTime.seconds))
                    log.info("ArbitraryModularBasicMinTask: minimal C = $C")
                    best = automaton
                    break
                } else {
                    log.failure("ArbitraryModularBasicMinTask: C = $C -> UNSAT in %.2f s.".format(runningTime.seconds))
                    task.finalize2()
                }
            }
        } else {
            val C = numberOfStates
            log.info("ArbitraryModularBasicMinTask: using provided C = $C")
            task = ArbitraryModularBasicTask(
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
            val (automaton, runningTime) = measureTimeWithResult { task.infer() }
            if (automaton != null)
                log.success("ArbitraryModularBasicMinTask: C = $C -> SAT in %.2f s.".format(runningTime.seconds))
            else
                log.failure("ArbitraryModularBasicMinTask: C = $C -> UNSAT in %.2f s.".format(runningTime.seconds))
            best = automaton
        }

        if (!isOnlyC && best != null) {
            log.info("ArbitraryModularBasicMinTask: searching for minimal T...")
            while (true) {
                val T = best!!.numberOfTransitions
                task.updateCardinalityLessThan(T)
                val (automaton, runningTime) = measureTimeWithResult { task.infer() }
                if (automaton != null) {
                    log.success(
                        "ArbitraryModularBasicMinTask: T < $T -> ${automaton.numberOfTransitions} in %.2f s."
                            .format(runningTime.seconds)
                    )
                    best = automaton
                } else {
                    log.failure("ArbitraryModularBasicMinTask: T < $T -> UNSAT in %.2f s.".format(runningTime.seconds))
                    log.info("ArbitraryModularBasicMinTask: minimal T = ${best.numberOfTransitions}")
                    break
                }
            }
        }

        task.finalize2()

        return best
    }
}
