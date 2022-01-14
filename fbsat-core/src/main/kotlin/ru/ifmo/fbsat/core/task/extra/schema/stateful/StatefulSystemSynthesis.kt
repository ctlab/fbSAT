@file:Suppress("DuplicatedCode")

package ru.ifmo.fbsat.core.task.extra.schema.stateful

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import com.github.lipen.satlib.core.BoolVarArray
import com.github.lipen.satlib.core.IntVarArray
import com.github.lipen.satlib.core.Lit
import com.github.lipen.satlib.core.Model
import com.github.lipen.satlib.core.convert
import com.github.lipen.satlib.core.eq
import com.github.lipen.satlib.core.neq
import com.github.lipen.satlib.core.newBoolVarArray
import com.github.lipen.satlib.core.newDomainVarArray
import com.github.lipen.satlib.core.newIntVarArray
import com.github.lipen.satlib.core.sign
import com.github.lipen.satlib.op.iffAnd
import com.github.lipen.satlib.op.iffOr
import com.github.lipen.satlib.op.imply
import com.github.lipen.satlib.op.implyIff
import com.github.lipen.satlib.op.implyIffAnd
import com.github.lipen.satlib.op.implyIffOr
import com.github.lipen.satlib.op.implyImplyImplyAnd
import com.github.lipen.satlib.solver.GlucoseSolver
import com.soywiz.klock.PerformanceCounter
import okio.buffer
import okio.sink
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.guard.BooleanExpressionGuard
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.clause
import ru.ifmo.fbsat.core.solver.implyIffXor
import ru.ifmo.fbsat.core.solver.solveAndGetModel
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.boolexpr.BinaryOperation
import ru.ifmo.fbsat.core.utils.boolexpr.BooleanExpression
import ru.ifmo.fbsat.core.utils.boolexpr.Constant
import ru.ifmo.fbsat.core.utils.boolexpr.UnaryOperation
import ru.ifmo.fbsat.core.utils.boolexpr.Variable
import ru.ifmo.fbsat.core.utils.exhaustive
import ru.ifmo.fbsat.core.utils.timeSince
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.useWith
import ru.ifmo.fbsat.core.utils.withIndex
import ru.ifmo.fbsat.core.utils.writeln
import java.io.File

private val logger = MyLogger {}

sealed interface SystemBlock {
    val name: String
    val numberOfInputs: Int
    val numberOfOutputs: Int
    val numberOfStates: Int
}

val SystemBlock.hasStates: Boolean get() = numberOfStates > 0

enum class GateBlock(
    override val numberOfInputs: Int,
    override val numberOfOutputs: Int,
) : SystemBlock {
    AND(2, 1),
    OR(2, 1),
    XOR(2, 1),
    NOT(1, 1),
    HA(2, 2),
    ;

    override val numberOfStates: Int = 0
}

data class AutomatonBlock(
    override val name: String,
    val automaton: Automaton,
) : SystemBlock {
    override val numberOfInputs: Int = automaton.inputNames.size
    override val numberOfOutputs: Int = automaton.outputNames.size
    override val numberOfStates: Int = automaton.numberOfStates

    override fun toString(): String {
        return name
    }

    companion object {
        val CNT = AutomatonBlock("CNT", counterAutomaton)
    }
}

@Suppress("LocalVariableName", "UnnecessaryVariable")
fun synthesizeStatefulSystem(
    tree: PositiveScenarioTree,
    M: Int,
    X: Int,
    Z: Int,
    blockTypes: List<SystemBlock>,
): Boolean {
    // TODO: input should be a list of traces (in SMV sense)

    val solver = GlucoseSolver()
    val outDir = File("out/system-synthesis")
    outDir.mkdirs()

    fun <T : SystemBlock> encodeBlock(blockType: T, encode: (T) -> Unit) {
        if (blockType in blockTypes) {
            encode(blockType)
        }
    }

    with(solver) {
        val V = tree.size
        // val SM = 3
        // val XM = 2
        // val ZM = 2
        val SM = blockTypes.maxOf { it.numberOfStates }
        val XM = blockTypes.maxOf { it.numberOfInputs }
        val ZM = blockTypes.maxOf { it.numberOfOutputs }

        logger.info("SM = $SM")
        logger.info("XM = $XM")
        logger.info("ZM = $ZM")
        logger.info("X = $X")
        logger.info("Z = $Z")

        // Note:
        //  - Inbound = input for module, but output for external
        //  - Outbound = output for module, but input for external

        var _inbound = 0
        var _outbound = 0
        val modularInputVarPin: IntMultiArray = MultiArray.new(M, XM) { ++_inbound }
        val modularOutputVarPin: IntMultiArray = MultiArray.new(M, ZM) { ++_outbound }
        val externalInputVarPin: IntMultiArray = MultiArray.new(X) { ++_outbound }
        val externalOutputVarPin: IntMultiArray = MultiArray.new(Z) { ++_inbound }
        check(_inbound == M * XM + Z)
        check(_outbound == M * ZM + X)

        val modularInputVarPins: MultiArray<List<Int>> =
            MultiArray.new(M) { (m) -> List(XM) { x -> modularInputVarPin[m, x + 1] } }
        val modularOutputVarPins: MultiArray<List<Int>> =
            MultiArray.new(M) { (m) -> List(ZM) { z -> modularOutputVarPin[m, z + 1] } }
        val externalInputVarPins: List<Int> =
            List(X) { x -> externalInputVarPin[x + 1] }
        val externalOutputVarPins: List<Int> =
            List(Z) { z -> externalOutputVarPin[z + 1] }
        val allInboundVarPins = 1.._inbound
        val allOutboundVarPins = 1.._outbound
        check(allInboundVarPins.toList() == modularInputVarPins.values.flatten() + externalOutputVarPins)
        check(allOutboundVarPins.toList() == modularOutputVarPins.values.flatten() + externalInputVarPins)

        logger.info("Modules inbound (input var) pins = ${(1..M).map { m -> modularInputVarPins[m] }}")
        logger.info("External inbound (output var) pins = $externalOutputVarPins")
        logger.info("Modules outbound (output var) pins = ${(1..M).map { m -> modularOutputVarPins[m] }}")
        logger.info("External outbound (input var) pins = $externalInputVarPins")

        fun pin2x(pin: Int): Int {
            // Note: `x` is 1-based
            require(pin in externalInputVarPins)
            val x = externalInputVarPins.indexOf(pin) + 1
            return x
        }

        fun pin2z(pin: Int): Int {
            require(pin in externalOutputVarPins)
            // Note: `z` is 1-based
            val z = externalOutputVarPins.indexOf(pin) + 1
            return z
        }

        fun pin2mx(pin: Int): Pair<Int, Int> {
            require(pin !in externalOutputVarPins)
            // Note: `m` and `x` are 1-based
            val m = (pin - 1) / XM + 1
            val x = modularInputVarPins[m].indexOf(pin) + 1
            return Pair(m, x)
        }

        fun pin2mz(pin: Int): Pair<Int, Int> {
            require(pin !in externalInputVarPins)
            // Note: `m` and `z` are 1-based
            val m = (pin - 1) / ZM + 1
            val z = modularOutputVarPins[m].indexOf(pin) + 1
            return Pair(m, z)
        }

        for (m in 1..M)
            for (x in 1..XM)
                check(pin2mx(modularInputVarPin[m, x]) == Pair(m, x))
        for (m in 1..M)
            for (z in 1..ZM)
                check(pin2mz(modularOutputVarPin[m, z]) == Pair(m, z))

        // =================

        logger.info("Declaring variables...")
        val timeStartVariables = PerformanceCounter.reference
        val nVarsStart = numberOfVariables

        val falseVar by lazy { newLiteral().also { clause(-it) } }

        val blockType = newDomainVarArray(M) { blockTypes }

        val modularInputVarParent = newIntVarArray(M, XM) { (m, _) ->
            (1 until m).flatMap { m_ -> modularOutputVarPins[m_] } + externalInputVarPins + 0
        }
        val externalOutputVarParent = newIntVarArray(Z) {
            modularOutputVarPins.values.flatten() + externalInputVarPins + 0
        }

        val modularState = newIntVarArray(M, V) { 0..SM }

        val modularInputVarValue = newBoolVarArray(M, XM, V)
        val modularOutputVarValue = newBoolVarArray(M, ZM, V)
        val externalInputVarValue = newBoolVarArray(X, V)
        val externalOutputVarValue = newBoolVarArray(Z, V)

        val inboundVarPinParent = IntVarArray.new(_inbound) { (pin) ->
            if (pin in externalOutputVarPins) {
                val z = pin2z(pin)
                externalOutputVarParent[z]
            } else {
                val (m, x) = pin2mx(pin)
                modularInputVarParent[m, x]
            }
        }
        val outboundVarPinChild = newIntVarArray(_outbound) { (pin) ->
            allInboundVarPins.filter { pin in inboundVarPinParent[it].domain } + 0
        }
        val inboundVarPinValue = BoolVarArray.new(_inbound, V) { (pin, v) ->
            if (pin in externalOutputVarPins) {
                val z = pin2z(pin)
                externalOutputVarValue[z, v]
            } else {
                val (m, x) = pin2mx(pin)
                modularInputVarValue[m, x, v]
            }
        }
        val outboundVarPinValue = BoolVarArray.new(_outbound, V) { (pin, v) ->
            if (pin in externalInputVarPins) {
                val x = pin2x(pin)
                externalInputVarValue[x, v]
            } else {
                val (m, z) = pin2mz(pin)
                modularOutputVarValue[m, z, v]
            }
        }

        val nVarsDiff = numberOfVariables - nVarsStart
        logger.debug {
            "Done declaring $nVarsDiff variables (total $numberOfVariables) in %.2f s."
                .format(timeSince(timeStartVariables).seconds)
        }

        // =================

        logger.info("Declaring constraints...")
        val timeStartConstraints = PerformanceCounter.reference
        val nClausesStart = numberOfClauses

        comment("Inbound pins have the same value as their parents")
        for (pin in allInboundVarPins)
            for (par in inboundVarPinParent[pin].domain - 0)
                for (v in 1..V)
                    implyIff(
                        inboundVarPinParent[pin] eq par,
                        inboundVarPinValue[pin, v],
                        outboundVarPinValue[par, v]
                    )

        comment("Parent-less pins have false values")
        for (pin in allInboundVarPins)
            for (v in 1..V)
                imply(
                    inboundVarPinParent[pin] eq 0,
                    -inboundVarPinValue[pin, v]
                )

        comment("Parent absence propagation")
        for (m in 1..M)
            for (x in 1 until XM)
                imply(
                    modularInputVarParent[m, x] eq 0,
                    modularInputVarParent[m, x + 1] eq 0
                )

        // FIXME: rewrite
        // Note: this constraint is big and only slows everything down
        // comment("AND/OR pins' parents order")
        // for (t in listOf(BlockType.AND, BlockType.OR))
        //     for (m in 1..M) {
        //         check(modularInboundVarPins[m].size == 2)
        //         val (pin1, pin2) = modularInboundVarPins[m]
        //         for (par1 in inboundVarPinParent[pin1].domain - 0)
        //             for (par2 in inboundVarPinParent[pin2].domain - 0)
        //                 if (par2 <= par1)
        //                     implyImply(
        //                         blockType[m] eq t,
        //                         inboundVarPinParent[pin1] eq par1,
        //                         inboundVarPinParent[pin2] neq par2
        //                     )
        //     }

        comment("Pin parent-child relation")
        for (pin in allInboundVarPins)
            for (par in inboundVarPinParent[pin].domain - 0)
                imply(
                    outboundVarPinChild[par] eq pin,
                    inboundVarPinParent[pin] eq par
                )
        for (pin in allOutboundVarPins)
            for (ch in outboundVarPinChild[pin].domain - 0)
                imply(
                    outboundVarPinChild[pin] eq 0,
                    inboundVarPinParent[ch] neq pin
                )

        comment("Number of input pins")
        for (t in blockTypes)
            for (m in 1..M)
                for (x in (t.numberOfInputs + 1)..XM)
                    imply(
                        blockType[m] eq t,
                        modularInputVarParent[m, x] eq 0
                    )

        comment("Number of output pins")
        for (t in blockTypes)
            for (m in 1..M)
                for (z in (t.numberOfOutputs + 1)..ZM) {
                    val pin = modularOutputVarPin[m, z]
                    imply(
                        blockType[m] eq t,
                        outboundVarPinChild[pin] eq 0
                    )
                }

        comment("Stateless blocks don't have states")
        for (t in blockTypes.filter { !it.hasStates })
            for (m in 1..M)
                for (v in 1..V)
                    imply(
                        blockType[m] eq t,
                        modularState[m, v] eq 0
                    )

        comment("Stateful blocks have states")
        for (t in blockTypes.filter { it.hasStates })
            for (m in 1..M)
                for (v in 1..V)
                    imply(
                        blockType[m] eq t,
                        modularState[m, v] neq 0
                    )

        comment("Number of states in stateful blocks")
        for (t in blockTypes.filter { it.hasStates })
            for (m in 1..M)
                for (v in 1..V)
                    for (s in (t.numberOfStates + 1)..SM)
                        imply(
                            blockType[m] eq t,
                            modularState[m] neq s
                        )

        comment("Initial state (at v=1) of stateful blocks is s1")
        for (t in blockTypes.filter { it.hasStates })
            for (m in 1..M)
                imply(
                    blockType[m] eq t,
                    modularState[m, 1] eq 1
                )

        fun encodeGate(t: GateBlock) {
            require(t in blockTypes)
            for (m in 1..M)
                for (v in 1..V)
                    when (t) {
                        GateBlock.AND -> implyIffAnd(
                            blockType[m] eq t,
                            modularOutputVarValue[m, 1, v],
                            modularInputVarValue[m, 1, v],
                            modularInputVarValue[m, 2, v]
                        )
                        GateBlock.OR -> implyIffOr(
                            blockType[m] eq t,
                            modularOutputVarValue[m, 1, v],
                            modularInputVarValue[m, 1, v],
                            modularInputVarValue[m, 2, v]
                        )
                        GateBlock.XOR -> implyIffXor(
                            blockType[m] eq t,
                            modularOutputVarValue[m, 1, v],
                            modularInputVarValue[m, 1, v],
                            modularInputVarValue[m, 2, v]
                        )
                        GateBlock.NOT -> implyIff(
                            blockType[m] eq t,
                            modularOutputVarValue[m, 1, v],
                            -modularInputVarValue[m, 1, v]
                        )
                        GateBlock.HA -> {
                            val A = 1
                            val B = 2
                            val SUM = 1
                            val CARRY = 2

                            // sum := a xor b;
                            implyIffXor(
                                blockType[m] eq t,
                                modularOutputVarValue[m, SUM, v],
                                modularInputVarValue[m, A, v],
                                modularInputVarValue[m, B, v]
                            )
                            // carry := a & b;
                            implyIffAnd(
                                blockType[m] eq t,
                                modularOutputVarValue[m, CARRY, v],
                                modularInputVarValue[m, A, v],
                                modularInputVarValue[m, B, v]
                            )
                        }
                        else -> TODO("Gate $t encoding not implemented yet")
                    }
        }

        for (t in blockTypes.filterIsInstance<GateBlock>()) {
            comment("Gate $t semantics")
            encodeGate(t)
        }

        val _modularReifiedValue: MultiArray<MutableMap<BooleanExpression, Lit>> =
            MultiArray.new(M, V) { mutableMapOf() }

        fun getModularReifiedValue(m: Int, v: Int, expr: BooleanExpression): Lit {
            return _modularReifiedValue[m, v][expr] ?: expr.let {
                when (it) {
                    is Constant -> error("Constants are not supported, yet")
                    is Variable -> {
                        modularInputVarValue[m, it.index + 1, v]
                    }
                    is UnaryOperation -> {
                        val lit = getModularReifiedValue(m, v, it.arg)
                        check(it.kind == UnaryOperation.Kind.Not)
                        -lit
                    }
                    is BinaryOperation -> {
                        val left = getModularReifiedValue(m, v, it.lhs)
                        val right = getModularReifiedValue(m, v, it.rhs)
                        val aux = newLiteral()
                        when (it.kind) {
                            BinaryOperation.Kind.And -> {
                                iffAnd(aux, left, right)
                            }
                            BinaryOperation.Kind.Or -> {
                                iffOr(aux, left, right)
                            }
                        }.exhaustive
                        aux
                    }
                }
            }.also {
                _modularReifiedValue[m, v][expr] = it
            }
        }

        fun encodeAutomaton(t: AutomatonBlock) {
            require(t in blockTypes)
            val automaton = t.automaton
            for (m in 1..M) {
                // Initial output values
                for (z in 1..automaton.outputNames.size) {
                    imply(
                        blockType[m] eq t,
                        modularOutputVarValue[m, z, 1] sign automaton.initialOutputValues[z - 1]
                    )
                }

                for (v in 2..V) {
                    val p = tree.parent(v)

                    // forbid unused states
                    for (s in (automaton.numberOfStates + 1)..SM)
                        imply(
                            blockType[m] eq t,
                            modularState[m, v] neq s
                        )

                    // NEXT
                    for (state in automaton.states) {
                        for (transition in state.transitions) {
                            val s1 = transition.source.id
                            val s2 = transition.destination.id
                            val guard = transition.guard
                            check(guard is BooleanExpressionGuard) {
                                "Only BooleanExpressionGuard is supported here"
                            }
                            val g = getModularReifiedValue(m, v, guard.expr)

                            // (state=S1) & guard_S1_S2 => next(state=S2)
                            implyImplyImplyAnd(
                                blockType[m] eq t,
                                modularState[m, p] eq s1,
                                g,
                                modularState[m, v] eq s2,
                            )

                            // (state=S1) & guard_S1_S2 => next(algorithms...)
                            val a = transition.destination.algorithm as BinaryAlgorithm
                            for (z in 1..automaton.outputNames.size) {
                                implyImplyImplyAnd(
                                    blockType[m] eq t,
                                    modularState[m, p] eq s1,
                                    g,
                                    modularOutputVarValue[m, z, v] sign if (tree.outputValue(p, z)) {
                                        a.algorithm1[z - 1]
                                    } else {
                                        a.algorithm0[z - 1]
                                    }
                                )
                            }
                        }
                    }
                }
            }
        }

        // Encode all automata
        for (t in blockTypes.filterIsInstance<AutomatonBlock>()) {
            comment("Automaton $t semantics")
            encodeAutomaton(t)
        }

        comment("Input values")
        for (v in 2..V)
            for (x in 1..X) {
                val value = tree.inputValue(v, x)
                clause(externalInputVarValue[x, v] sign value)
            }

        comment("Output values")
        for (v in 2..V)
            for (z in 1..Z) {
                val value = tree.outputValue(v, z)
                clause(externalOutputVarValue[z, v] sign value)
            }

        comment("Initial (root) input values are false")
        for (x in 1..X) {
            clause(-externalInputVarValue[x, 1])
        }

        val nClausesDiff = numberOfClauses - nClausesStart
        logger.debug {
            "Done declaring $nClausesDiff constraints (total $numberOfClauses) in %.2f s."
                .format(timeSince(timeStartConstraints).seconds)
        }

        // =================

        // val fileCnf = outDir.resolve("system_X${X}_Z${Z}_M${M}.cnf")
        // fileCnf.parentFile.mkdirs()
        // dumpDimacs(fileCnf)

        val model: Model? = solveAndGetModel()

        @Suppress("NAME_SHADOWING")
        if (model != null) {
            val blockType = blockType.convert(model)
            logger.info("blockType = ${blockType.values}")

            val parent = inboundVarPinParent.convert(model)
            for (m in 1..M) {
                logger.info("(m = $m) parent = ${modularInputVarPins[m].map { parent[it] }}")
            }
            logger.info("(external) parent = ${externalOutputVarPins.map { parent[it] }}")

            val inboundValue = inboundVarPinValue.convert(model)
            val outboundValue = outboundVarPinValue.convert(model)
            val modularState = modularState.convert(model)
            logger.just("        v   tree.input -> tree.output :: input > [input modular] > [state modular] > [output modular] > output ext")
            for (v in 1..V) {
                logger.just(
                    "[fold]  v=$v" +
                        " ${
                            if (v == 1) "-".repeat(X)
                            else (1..X).map { x -> tree.inputValue(v, x) }.toBinaryString()
                        }" +
                        " -> ${(1..Z).map { z -> tree.outputValue(v, z) }.toBinaryString()}" +
                        " :: ${externalInputVarPins.map { outboundValue[it, v] }.toBinaryString()}" +
                        " >> ${
                            modularInputVarPins.values.map { pins ->
                                pins.map { inboundValue[it, v] }.toBinaryString()
                            }
                        }" +
                        " >> ${
                            (1..M).map { m ->
                                modularState[m, v]
                            }
                        }" +
                        " >> ${
                            modularOutputVarPins.values.map { pins ->
                                pins.map { outboundValue[it, v] }.toBinaryString()
                            }
                        }" +
                        " >> ${externalOutputVarPins.map { inboundValue[it, v] }.toBinaryString()}"
                )
            }

            val fileSystem = outDir.resolve("system.gv")
            fileSystem.sink().buffer().useWith {
                writeln("digraph {")
                writeln("    // Gates")
                for (m in 1..M) {
                    writeln("    g$m [label=\"$m: ${blockType[m]}\"];")
                }
                writeln("    // Input variables")
                writeln("    { rank=sink; rankdir=LR;")
                for (x in 1..X) {
                    writeln("        X$x [label=\"X$x\"];")
                }
                writeln("    edge [style=invis];")
                writeln("    ${(1..X).joinToString(" -> ") { x -> "X$x" }};")
                writeln("    }")
                writeln("    // Output variables")
                writeln("    { rank=source; rankdir=LR;")
                for (z in 1..Z) {
                    writeln("        Z$z [label=\"Z$z\"];")
                }
                writeln("    edge [style=invis];")
                writeln("    ${(1..Z).joinToString(" -> ") { z -> "Z$z" }};")
                writeln("    }")
                writeln("")
                writeln("    // System")
                for (m in 1..M) {
                    val ts: Sequence<Pair<String, String>> = sequence {
                        for (pin in modularInputVarPins[m]) {
                            when (val par = parent[pin]) {
                                0 -> {
                                    break
                                }
                                in externalInputVarPins -> {
                                    val x = pin2x(par)
                                    yield("X$x" to "X$x")
                                }
                                else -> {
                                    val (mp, zp) = pin2mz(par)
                                    yield("g$mp" to "z$zp")
                                }
                            }
                        }
                    }
                    for ((i, t) in ts.withIndex(start = 1)) {
                        val (a, b) = t
                        writeln("    g$m -> $a [label=\"x$i-$b\"];")
                    }
                }
                for (z in 1..Z) {
                    val pin = externalOutputVarPin[z]
                    val par = parent[pin]
                    if (par == 0) continue
                    val t = if (par in externalInputVarPins) {
                        val x = pin2x(par)
                        "X$x" to "X$x"
                    } else {
                        val (mp, zp) = pin2mz(par)
                        "g$mp" to "z$zp"
                    }
                    val (a, b) = t
                    writeln("    Z$z -> $a [label=\"Z$z-$b\"];")
                }
                writeln("}")
            }
            Runtime.getRuntime().exec("dot -Tpdf -O $fileSystem")
            Runtime.getRuntime().exec("dot -Tpng -O $fileSystem")

            return true
        }
    }

    return false
}

fun synthesizeStatefulSystemIterativelyBottomUp(
    tree: PositiveScenarioTree,
    X: Int,
    Z: Int,
    minM: Int = 1,
    maxM: Int = 30,
    blockTypes: List<SystemBlock>,
) {
    for (M in minM..maxM) {
        logger.just("brrr...")
        logger.info("Trying M = $M...")
        val timeStart = PerformanceCounter.reference

        if (synthesizeStatefulSystem(tree, M = M, X = X, Z = Z, blockTypes)) {
            logger.info("M = $M: SAT after %.2f s!".format(timeSince(timeStart).seconds))
            break
        } else {
            logger.info("M = $M: UNSAT after %.2f s.".format(timeSince(timeStart).seconds))
        }
    }
}