package ru.ifmo.fbsat.core.task.complete

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.declareNegativeColorConstraints
import ru.ifmo.fbsat.core.task.declareNegativeFiringConstraints
import ru.ifmo.fbsat.core.task.declareNegativeGuardConstraints
import ru.ifmo.fbsat.core.task.declareNegativeTransitionConstraints
import ru.ifmo.fbsat.core.task.extended.ExtendedAssignment
import ru.ifmo.fbsat.core.task.extended.ExtendedTask
import ru.ifmo.fbsat.core.task.extended.toAutomaton
import ru.ifmo.fbsat.core.utils.getForce
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.multiarray.IntMultiArray
import java.io.File

interface CompleteTask {
    val scenarioTree: ScenarioTree
    val negativeScenarioTree: NegativeScenarioTree
    val numberOfStates: Int // C
    val maxOutgoingTransitions: Int // K
    val maxGuardSize: Int // P
    val maxTotalGuardsSize: Int? // N, unconstrained if null
    val outDir: File

    fun infer(): Automaton?
    fun reuse(newMaxTotalGuardsSize: Int): CompleteTask
    fun update()
    fun finalize2()

    companion object {
        @JvmStatic
        fun create(
            scenarioTree: ScenarioTree,
            negativeScenarioTree: NegativeScenarioTree?, // empty if null
            numberOfStates: Int, // C
            maxOutgoingTransitions: Int? = null, // K, K=C if null
            maxGuardSize: Int, // P
            maxTotalGuardsSize: Int? = null, // N, unconstrained if null
            outDir: File,
            solverProvider: () -> Solver,
            autoFinalize: Boolean = true
        ): CompleteTask = CompleteTaskImpl(
            scenarioTree = scenarioTree,
            negativeScenarioTree = negativeScenarioTree
                ?: NegativeScenarioTree.empty(scenarioTree.inputNames, scenarioTree.outputNames),
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions ?: numberOfStates,
            maxGuardSize = maxGuardSize,
            maxTotalGuardsSize = maxTotalGuardsSize,
            outDir = outDir,
            solver = solverProvider(),
            autoFinalize = autoFinalize,
            extendedTask = null
        )
    }
}

private class CompleteTaskImpl(
    override val scenarioTree: ScenarioTree,
    override val negativeScenarioTree: NegativeScenarioTree,
    override val numberOfStates: Int, // C
    override val maxOutgoingTransitions: Int, // K
    override val maxGuardSize: Int, // P
    override val maxTotalGuardsSize: Int?, // N, unconstrained if null
    override val outDir: File,
    private val solver: Solver,
    private val autoFinalize: Boolean,
    extendedTask: ExtendedTask?
) : CompleteTask {
    @Suppress("JoinDeclarationAndAssignment")
    private val extendedTask: ExtendedTask
    private var isExecuted = false
    private var isReused = false
    private var isFinalized = false

    init {
        this.extendedTask = extendedTask ?: ExtendedTask.create(
            scenarioTree = scenarioTree,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxGuardSize = maxGuardSize,
            maxTotalGuardsSize = maxTotalGuardsSize,
            outDir = outDir,
            solverProvider = { solver },
            autoFinalize = false,
            isEncodeReverseImplication = false
        )
        solver.declareNegativeReduction()
    }

    override fun infer(): Automaton? {
        check(!isExecuted) { "This task has already been executed. Try using the `reuse()` method." }
        check(!isReused) { "This task has already been reused and can't be executed." }
        check(!isFinalized) { "This task has already been finalized and can't be executed." }
        isExecuted = true

        val rawAssignment = solver.solve()
        if (autoFinalize) finalize2()
        if (rawAssignment == null)
            return null

        val assignment = ExtendedAssignment.fromRaw(rawAssignment)
        @Suppress("UnnecessaryVariable")
        val automaton = assignment.toAutomaton()
        return automaton
    }

    override fun reuse(newMaxTotalGuardsSize: Int): CompleteTask {
        check(!isReused) { "This task has already been reused." }
        check(!isFinalized) { "This task has already been finalized and can't be reused." }
        if (!isExecuted)
            log.warn("Reusing the task that has not been executed yet.")
        isReused = true

        return CompleteTaskImpl(
            scenarioTree = this.scenarioTree,
            negativeScenarioTree = this.negativeScenarioTree,
            numberOfStates = this.numberOfStates,
            maxOutgoingTransitions = this.maxOutgoingTransitions,
            maxGuardSize = this.maxGuardSize,
            maxTotalGuardsSize = newMaxTotalGuardsSize,
            outDir = this.outDir,
            solver = this.solver,
            autoFinalize = this.autoFinalize,
            extendedTask = this.extendedTask.reuse(newMaxTotalGuardsSize)
        )
    }

    override fun update() {
        check(!isReused) { "This task has already been reused and can't be updated." }
        check(!isFinalized) { "This task has already been finalized and can't be updated." }

        solver.declareNegativeReduction()
        isExecuted = false
    }

    override fun finalize2() {
        check(!isFinalized) { "This task has already been finalized." }
        isFinalized = true
        extendedTask.finalize2()
        // Note: extendedTask already finalizes solver, so it is not necessary to do it here again
        // solver.finalize2()
    }

    @Suppress("LocalVariableName", "UNUSED_VARIABLE")
    private fun Solver.declareNegativeReduction() {
        // Constants
        context["negativeScenarioTree"] = negativeScenarioTree
        val C: Int by context
        val K: Int by context
        val P: Int by context
        val E: Int by context
        val O: Int by context
        val X: Int by context
        val Z: Int by context
        val oldNegV: Int = (context["negV"] as Int?) ?: 0
        val negV: Int by context(negativeScenarioTree.size)
        val newNegVs: IntRange by context((oldNegV + 1)..negV)
        val newNegVsActive: List<Int> by context(newNegVs.filter { it in negativeScenarioTree.activeVertices })
        val newNegVsPassive: List<Int> by context(newNegVs.filter { it in negativeScenarioTree.passiveVertices })
        // val negU: Int by context(negativeScenarioTree.uniqueInputs.size)
        val negU: Int = negativeScenarioTree.uniqueInputs.size
        val oldNegUIs: List<String> = context.getForce("negUIs") ?: emptyList()
        val negUIs: List<String> by context(negativeScenarioTree.uniqueInputs)
        val posUIs: List<String> = scenarioTree.uniqueInputs
        fun getNegU(input: String): Int = negUIs.indexOf(input) + 1
        fun getOldNegU(input: String): Int = oldNegUIs.indexOf(input) + 1
        fun getPosU(input: String): Int = posUIs.indexOf(input) + 1
        val oldOnlyNegUIs: List<String> = context.getForce("onlyNegUIs") ?: emptyList()
        val onlyNegUIs: List<String> by context(negUIs - posUIs)
        val newOnlyNegUIs: List<String> = onlyNegUIs - oldOnlyNegUIs
        val newOnlyNegUs: List<Int> by context(newOnlyNegUIs.map(::getNegU))
        context.computeIfAbsent("forbiddenLoops") { mutableSetOf<Pair<Int, Int>>() }

        // Variables
        val transition: IntMultiArray by context
        val actualTransition: IntMultiArray by context
        val inputEvent: IntMultiArray by context
        val outputEvent: IntMultiArray by context
        val algorithm0: IntMultiArray by context
        val algorithm1: IntMultiArray by context
        val color: IntMultiArray by context
        val nodeType: IntMultiArray by context
        val terminal: IntMultiArray by context
        val parent: IntMultiArray by context
        val childLeft: IntMultiArray by context
        val childRight: IntMultiArray by context
        val nodeValue: IntMultiArray by context
        val rootValue: IntMultiArray by context
        val childValueLeft: IntMultiArray by context
        val childValueRight: IntMultiArray by context
        val firstFired: IntMultiArray by context
        val notFired: IntMultiArray by context
        val oldNegActualTransition: IntMultiArray = context.getForce("negActualTransition")
        val negActualTransition by context(
            newArray(C, E, negU, C + 1) { (i, e, u, j) ->
                when (val input = negUIs[u - 1]) {
                    in newOnlyNegUIs -> newVariable()
                    in oldNegUIs -> oldNegActualTransition[i, e, getOldNegU(input), j]
                    else -> actualTransition[i, e, getPosU(input), j]
                }
            }
        )
        val oldSatisfaction: IntMultiArray = context.getForce("satisfaction")
        val satisfaction: IntMultiArray by context(
            newArray(negV, C + 1) { (v, c) ->
                if (v in newNegVs) newVariable()
                else oldSatisfaction[v, c]
            }
        )
        val oldNegNodeValue: IntMultiArray = context.getForce("negNodeValue")
        val negNodeValue: IntMultiArray by context(
            newArray(C, K, P, negU) { (c, k, p, u) ->
                when (val input = negUIs[u - 1]) {
                    in newOnlyNegUIs -> newVariable()
                    in oldNegUIs -> oldNegNodeValue[c, k, p, getOldNegU(input)]
                    else -> nodeValue[c, k, p, getPosU(input)]
                }
            }
        )
        val negRootValue: IntMultiArray by context(
            newArray(C, K, negU) { (c, k, u) ->
                negNodeValue[c, k, 1, u]
            }
        )
        val oldNegChildValueLeft: IntMultiArray = context.getForce("negChildValueLeft")
        val negChildValueLeft: IntMultiArray by context(
            newArray(C, K, P, negU) { (c, k, p, u) ->
                when (val input = negUIs[u - 1]) {
                    in newOnlyNegUIs -> newVariable()
                    in oldNegUIs -> oldNegChildValueLeft[c, k, p, getOldNegU(input)]
                    else -> childValueLeft[c, k, p, getPosU(input)]
                }
            }
        )
        val oldNegChildValueRight: IntMultiArray = context.getForce("negChildValueRight")
        val negChildValueRight: IntMultiArray by context(
            newArray(C, K, P, negU) { (c, k, p, u) ->
                when (val input = negUIs[u - 1]) {
                    in newOnlyNegUIs -> newVariable()
                    in oldNegUIs -> oldNegChildValueRight[c, k, p, getOldNegU(input)]
                    else -> childValueRight[c, k, p, getPosU(input)]
                }
            }
        )
        val oldNegFirstFired: IntMultiArray = context.getForce("negFirstFired")
        val negFirstFired: IntMultiArray by context(
            newArray(C, negU, K + 1) { (c, u, k) ->
                when (val input = negUIs[u - 1]) {
                    in newOnlyNegUIs -> newVariable()
                    in oldNegUIs -> oldNegFirstFired[c, getOldNegU(input), k]
                    else -> firstFired[c, getPosU(input), k]
                }
            }
        )
        val oldNegNotFired: IntMultiArray = context.getForce("negNotFired")
        val negNotFired: IntMultiArray by context(
            newArray(C, negU, K) { (c, u, k) ->
                when (val input = negUIs[u - 1]) {
                    in newOnlyNegUIs -> newVariable()
                    in oldNegUIs -> oldNegNotFired[c, getOldNegU(input), k]
                    else -> notFired[c, getPosU(input), k]
                }
            }
        )

        // Constraints
        declareNegativeColorConstraints()
        declareNegativeTransitionConstraints()
        declareNegativeFiringConstraints()
        declareNegativeGuardConstraints()
    }
}
