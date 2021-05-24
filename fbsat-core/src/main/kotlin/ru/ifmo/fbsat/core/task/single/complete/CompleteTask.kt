package ru.ifmo.fbsat.core.task.single.complete

import com.github.lipen.satlib.card.Cardinality
import com.github.lipen.satlib.card.declareTotalizer
import com.github.lipen.satlib.core.AssumptionsProvider
import com.github.lipen.satlib.core.BoolVarArray
import com.github.lipen.satlib.core.IntVar
import com.github.lipen.satlib.core.IntVarArray
import com.github.lipen.satlib.core.Lit
import com.github.lipen.satlib.core.newBoolVarArray
import com.github.lipen.satlib.core.newIntVar
import com.github.lipen.satlib.op.iff
import com.github.lipen.satlib.op.imply
import com.github.lipen.satlib.solver.Solver
import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.constraints.*
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.NegativeTreeOptimizations
import ru.ifmo.fbsat.core.utils.timeSince
import kotlin.math.min
import kotlin.math.max
import kotlin.reflect.jvm.internal.impl.util.ValueParameterCountCheck

private val logger = MyLogger {}

data class CompleteTask(
    val negativeScenarioTree: NegativeScenarioTree? = null, // empty if null
) : Task() {
    override fun Solver.declare_() {
        val positiveScenarioTree: PositiveScenarioTree = context["positiveScenarioTree"]
        val negativeScenarioTree = context("negativeScenarioTree") {
            this@CompleteTask.negativeScenarioTree ?: NegativeScenarioTree(positiveScenarioTree)
        }

        /* Variables */
        comment("$name: Variables")
        declareCompleteVariables(
            negativeScenarioTree = negativeScenarioTree
        )

        /* Initial negative constraints */
        comment("$name: Initial negative constraints")
        updateNegativeReduction()
    }
}

fun Solver.addHeightConstraints(negV: Int, oldNegV: Int, negativeScenarioTree: NegativeScenarioTree) {
    if (context.getOrNull<heightMarker>("heightTree") == null) return

    logger.info("Applying OPTIMIZATION 3")
    val curHeight: Int = context.getOrNull("curHeight") ?: return
    val maxHeight: Int = context.getOrNull("maxHeight") ?: curHeight.also {
        context["heightTotalizer"] = MutableList(it + 1) { newLiteral() }
    }
    context["maxHeight"] = maxHeight
    val heightTotalizer: MutableList<Lit> = context["heightTotalizer"]
    val negMapping: IntVarArray = context["negMapping"]
    for (i in (oldNegV + 1)..negV) {
        //println("$i -> ${negativeScenarioTree.nodes[i - 1].height}")
        imply(-(negMapping[i] eq 0), heightTotalizer[negativeScenarioTree.nodes[i - 1].height])
    }

    val height: Int = context.getOrNull<Int>("height")?.let { min(it, curHeight) } ?: curHeight
    context["height"] = height
    val oldHeightConstraint = context.getOrNull<() -> List<Lit>>("heightConstraint")
    if (oldHeightConstraint != null) {
        assumptionsObservable.unregister(oldHeightConstraint)
    }
    val heightConstraint = context("heightConstraint") { { listOf(-heightTotalizer[height]) } }
    assumptionsObservable.register(heightConstraint)
    //println("Height -> $height")
}

fun Solver.addNegMappingCardinality(negV: Int, oldNegV: Int) {
    //logger.error("Hay negV = $negV oldNegV = $oldNegV")
    val chunk: Int = context.getOrNull("chunk") ?: return

    logger.info("Applying OPTIMIZATION 2")
    val newNegVs = (oldNegV + 1)..negV
    val negMapping: IntVarArray = context["negMapping"]
    val oldZeroMapping: BoolVarArray = context.getOrNull("zeroMapping") ?: newBoolVarArray(0)
    var oldZeroVertices: Int = context.getOrNull("zeroVertices") ?: 0
    val zeroVertices = context("zeroVertices") { chunk + (negV + (chunk - 1)) / chunk * 2 * chunk }
    val newZeroVertices = (oldZeroVertices + 1)..zeroVertices
    val zeroMapping = context("zeroMapping") {
        newBoolVarArray(zeroVertices) { (v) ->
            if (v in newZeroVertices) newLiteral()
            else oldZeroMapping[v]
        }
    }
    val zeroTotalizer = context("zeroTotalizer") { (1..chunk).map { zeroMapping[it] } }
    oldZeroVertices = max(oldZeroVertices, chunk)
    while (oldZeroVertices != zeroVertices) {
        val a = declareTotalizer(((oldZeroVertices + 1)..(oldZeroVertices + chunk)).map { zeroMapping[it] })
        //println("A = ${((oldZeroVertices + 1)..(oldZeroVertices + chunk)).map { zeroMapping[it] }}")
        val b = ((oldZeroVertices + chunk + 1)..(oldZeroVertices + 2 * chunk)).map { zeroMapping[it] }
        val r = ((oldZeroVertices - chunk + 1)..oldZeroVertices).map { zeroMapping[it] }
        for (alpha in 0..chunk) {
            for (beta in 0..chunk) {
                val sigma = alpha + beta
                if (sigma > chunk) continue
                when {
                    sigma == 0 -> null
                    alpha == 0 -> listOf(-b[beta - 1], r[sigma - 1])
                    beta == 0 -> listOf(-a[alpha - 1], r[sigma - 1])
                    else -> listOf(-a[alpha - 1], -b[beta - 1], r[sigma - 1])
                }?.let { addClause(it) }
                if (sigma != chunk) {
                    addClause(listOf(a[alpha], b[beta], -r[sigma]))
                }
            }
        }
        oldZeroVertices += 2 * chunk
    }
    for (i in newNegVs) {
        iff(-(negMapping[i] eq 0), zeroMapping[chunk + (i - 1) / chunk * chunk * 2 + (i - 1) % chunk + 1])
    }
    //println("Mapping = ${newNegVs.joinToString { "$it<->${zeroMapping[chunk + (it - 1) / chunk * chunk * 2 + (it - 1) % chunk + 1]}" }}")
    val goodNodesCount: Int? = context.getOrNull("goodNodesCount")
    val heat = context.getOrNull<MutableMap<Int, MutableSet<Int>>>("heat") ?: return
    val maxHeat = heat.flatMap { it.value }.maxOrNull() ?: return
    val newGoodNodesCount = heat.filter { maxHeat in it.value }.size + 1 // one for root
    logger.info("Previous good nodes count = $goodNodesCount, current good nodes count = $newGoodNodesCount")
    if (goodNodesCount == null || goodNodesCount > newGoodNodesCount) {
        val goodNodesCard: (() -> List<Lit>)? = context.getOrNull("goodNodesCard")
        if (goodNodesCard != null) {
            assumptionsObservable.unregister(goodNodesCard)
        }
        context("goodNodesCount") { newGoodNodesCount }
        assumptionsObservable.register(context("goodNodesCard") {
            { listOf(-zeroTotalizer[newGoodNodesCount]) }
        })
    }
}

fun Solver.addNegMappingCardinalityNew(negV: Int, oldNegV: Int) {
    if (context.getOrNull<Cegisic>("cegisic") == null) return

    logger.info("Applying OPTIMIZATION 2.5")
    val negMapping: IntVarArray = context["negMapping"]
    val negMappingCardinality: IncrementalCardinality = context.getOrNull<IncrementalCardinality>("negMappingCardinality")?.also {
        if (negV > oldNegV) {
            it.merge(((oldNegV + 1)..negV).map { v -> -(negMapping[v] eq 0) })
        }
    } ?: run {
        val cardinality = IncrementalCardinality(this, ((oldNegV + 1)..negV).map { -(negMapping[it] eq 0) })
        context["negMappingCardinality"] = cardinality
        cardinality
    }
    val heat = context.getOrNull<MutableMap<Int, MutableSet<Int>>>("heat") ?: return
    val maxHeat = heat.flatMap { it.value }.maxOrNull() ?: return
    val goodNodesCount = heat.filter { maxHeat in it.value }.size + 1 // one for root
    val previousBorder = negMappingCardinality.border
    //require(previousBorder == null || goodNodesCount <= previousBorder) { "Previous good nodes count = $previousBorder, current good nodes count = $goodNodesCount" }
    logger.info("Previous good nodes count = $previousBorder, current good nodes count = $goodNodesCount")
    if (previousBorder == null || goodNodesCount < previousBorder) {
        negMappingCardinality.assumeUpperBoundLessOrEqual(goodNodesCount)
    }
}

@Suppress("LocalVariableName")
fun Solver.updateNegativeReduction(
    isForbidLoops: Boolean = true,
) {
    val timeStart = PerformanceCounter.reference
    val nVarsStart = numberOfVariables
    val nClausesStart = numberOfClauses

    /* Constants */
    val positiveScenarioTree: PositiveScenarioTree = context["positiveScenarioTree"]
    val negativeScenarioTree: NegativeScenarioTree = context["negativeScenarioTree"]

    val oldNegV: Int = context["negV"]
    val negV: Int = context("negV", negativeScenarioTree.size)
    val newNegVs = (oldNegV + 1)..negV

    val negU: Int = context("negU", negativeScenarioTree.uniqueInputs.size)

    val oldNegUIs: List<InputValues> = context["negUIs"]
    val negUIs = context("negUIs", negativeScenarioTree.uniqueInputs)
    val posUIs = positiveScenarioTree.uniqueInputs

    fun getOldNegU(input: InputValues): Int = oldNegUIs.indexOf(input) + 1
    fun getNegU(input: InputValues): Int = negUIs.indexOf(input) + 1
    fun getPosU(input: InputValues): Int = posUIs.indexOf(input) + 1

    fun inputChoice(
        u: Int,
        inOldNegUIs: (Int) -> Lit,
        inPosUIs: (Int) -> Lit,
    ): Lit =
        when (val input = negUIs[u - 1]) {
            in oldNegUIs -> inOldNegUIs(getOldNegU(input))
            in posUIs -> inPosUIs(getPosU(input))
            else -> newLiteral()
        }

    fun inputChoice(
        u: Int,
        inOldNegUIs: (Int) -> IntVar,
        inPosUIs: (Int) -> IntVar,
        new: () -> IntVar,
    ): IntVar =
        when (val input = negUIs[u - 1]) {
            in oldNegUIs -> inOldNegUIs(getOldNegU(input))
            in posUIs -> inPosUIs(getPosU(input))
            else -> new()
        }

    val oldOnlyNegUIs: List<InputValues> = context["onlyNegUIs"]
    val onlyNegUIs = context("onlyNegUIs", negUIs - posUIs)
    val newOnlyNegUIs = onlyNegUIs - oldOnlyNegUIs
    val newOnlyNegUs = newOnlyNegUIs.map(::getNegU)

    /* Variables */
    val C: Int = context["C"]
    val K: Int = context["K"]
    val P: Int = context["P"]
    val E: Int = context["E"]
    val actualTransitionFunction: IntVarArray = context["actualTransitionFunction"]
    val transitionTruthTable: BoolVarArray = context["transitionTruthTable"]
    val transitionFiring: BoolVarArray = context["transitionFiring"]
    val firstFired: IntVarArray = context["firstFired"]
    val notFired: BoolVarArray = context["notFired"]
    val nodeValue: BoolVarArray = context["nodeValue"]
    val oldNegActualTransitionFunction: IntVarArray = context["negActualTransitionFunction"]
    val oldNegTransitionTruthTable: BoolVarArray = context["negTransitionTruthTable"]
    val oldNegTransitionFiring: BoolVarArray = context["negTransitionFiring"]
    val oldNegFirstFired: IntVarArray = context["negFirstFired"]
    val oldNegNotFired: BoolVarArray = context["negNotFired"]
    val oldNegNodeValue: BoolVarArray = context["negNodeValue"]
    val oldNegMapping: IntVarArray = context["negMapping"]

    val negActualTransitionFunction = context("negActualTransitionFunction") {
        IntVarArray.new(C, E, negU) { (c, e, u) ->
            inputChoice(u,
                { oldNegActualTransitionFunction[c, e, it] },
                { actualTransitionFunction[c, e, it] },
                { newIntVar(0..C) })
        }
    }
    val negTransitionTruthTable = context("negTransitionTruthTable") {
        newBoolVarArray(C, K, negU) { (c, k, u) ->
            inputChoice(u,
                { oldNegTransitionTruthTable[c, k, it] },
                { transitionTruthTable[c, k, it] })
        }
    }
    val negTransitionFiring = context("negTransitionFiring") {
        newBoolVarArray(C, K, E, negU) { (c, k, e, u) ->
            inputChoice(u,
                { oldNegTransitionFiring[c, k, e, it] },
                { transitionFiring[c, k, e, it] })
        }
    }
    val negFirstFired = context("negFirstFired") {
        IntVarArray.new(C, E, negU) { (c, e, u) ->
            inputChoice(u,
                { oldNegFirstFired[c, e, it] },
                { firstFired[c, e, it] },
                { newIntVar(0..K) })
        }
    }
    val negNotFired = context("negNotFired") {
        newBoolVarArray(C, K, E, negU) { (c, k, e, u) ->
            inputChoice(u,
                { oldNegNotFired[c, k, e, it] },
                { notFired[c, k, e, it] })
        }
    }
    val negNodeValue = context("negNodeValue") {
        newBoolVarArray(C, K, P, negU) { (c, k, p, u) ->
            if (p == 1) negTransitionTruthTable[c, k, u]
            else inputChoice(u,
                { oldNegNodeValue[c, k, p, it] },
                { nodeValue[c, k, p, it] })
        }
    }
    val negMapping = context("negMapping") {
        IntVarArray.new(negV) { (v) ->
            if (v in newNegVs) newIntVar(0..C)
            else oldNegMapping[v]
        }
    }

    if (Globals.IS_DUMP_VARS_IN_CNF) {
        logger.warn("Dumping of CompleteVariables to CNF is not implemented yet")
    }

    if (Globals.NEGATIVE_TREE_OPTIMIZATIONS == NegativeTreeOptimizations.OPT1) {
        logger.info("Applying optimization with one node mapped to zero")
        val oldObserver: (() -> List<Lit>)? = context.getOrNull("observer")
        val canMapTreeToZero: Boolean = context["canMapTreeToZero"]
        if (oldObserver != null && assumptionsObservable.listeners.contains(oldObserver)) {
            assumptionsObservable.unregister(oldObserver)
        }
        val nullVertex: NegativeScenarioTree.Node? = context.getOrNull("nullVertex")
        if (nullVertex != null && canMapTreeToZero) {
            assumptionsObservable.register(context("observer") {
                { listOf(negMapping[nullVertex.id] eq 0) }
            })
        }
    }

    /* Constraints */
    declareNegativeAutomatonStructureConstraints(Us = newOnlyNegUs)
    declareNegativeGuardConditionsConstraints(Us = newOnlyNegUs)
    declareNegativeMappingConstraints(Vs = newNegVs, isForbidLoops = isForbidLoops)
    addNegMappingCardinality(negV, oldNegV)
    addNegMappingCardinalityNew(negV, oldNegV)
    addHeightConstraints(negV, oldNegV, negativeScenarioTree)

    val nVarsDiff = numberOfVariables - nVarsStart
    val nClausesDiff = numberOfClauses - nClausesStart
    logger.debug {
        "updateNegativeReduction: declared $nVarsDiff variables and $nClausesDiff clauses in %.3f s."
            .format(timeSince(timeStart).seconds)
    }
}
