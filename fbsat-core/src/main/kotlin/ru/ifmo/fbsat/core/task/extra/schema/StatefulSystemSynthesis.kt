@file:Suppress("DuplicatedCode")

package ru.ifmo.fbsat.core.task.extra.schema

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import com.github.lipen.satlib.core.BoolVarArray
import com.github.lipen.satlib.core.IntVarArray
import com.github.lipen.satlib.core.Model
import com.github.lipen.satlib.core.convert
import com.github.lipen.satlib.core.newBoolVarArray
import com.github.lipen.satlib.core.newDomainVarArray
import com.github.lipen.satlib.core.newIntVarArray
import com.github.lipen.satlib.core.sign
import com.github.lipen.satlib.op.iff
import com.github.lipen.satlib.op.iffAnd
import com.github.lipen.satlib.op.imply
import com.github.lipen.satlib.op.implyIff
import com.github.lipen.satlib.op.implyIffAnd
import com.github.lipen.satlib.op.implyIffOr
import com.github.lipen.satlib.op.implyImplyAnd
import com.github.lipen.satlib.op.implyImplyIff
import com.github.lipen.satlib.op.implyImplyImply
import com.github.lipen.satlib.solver.MiniSatSolver
import com.soywiz.klock.PerformanceCounter
import okio.buffer
import okio.sink
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.clause
import ru.ifmo.fbsat.core.solver.solveAndGetModel
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.mylog
import ru.ifmo.fbsat.core.utils.scope
import ru.ifmo.fbsat.core.utils.timeSince
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.useWith
import ru.ifmo.fbsat.core.utils.withIndex
import ru.ifmo.fbsat.core.utils.writeln
import java.io.File

@Suppress("LocalVariableName", "UnnecessaryVariable")
fun synthesizeStatefulSystem(
    tree: PositiveScenarioTree,
    M: Int,
    X: Int,
    Z: Int,
): Boolean {
    // TODO: input should be a list of traces (in SMV sense)

    val solver = MiniSatSolver()
    val outDir = File("out/system-synthesis")
    outDir.mkdirs()

    with(solver) {
        val V = tree.size
        val SM = 2
        val XM = 2
        val ZM = 2

        // Note:
        //  - Inbound = input for module, but output for external
        //  - Outbound = output for module, but input for external

        var _inboundVarPins = 0
        var _outboundVarPins = 0
        val modularInputVarPin: IntMultiArray = MultiArray.new(M, XM) { ++_inboundVarPins }
        val modularOutputVarPin: IntMultiArray = MultiArray.new(M, ZM) { ++_outboundVarPins }
        val externalInputVarPin: IntMultiArray = MultiArray.new(X) { ++_outboundVarPins }
        val externalOutputVarPin: IntMultiArray = MultiArray.new(Z) { ++_inboundVarPins }
        check(_inboundVarPins == M * XM + Z)
        check(_outboundVarPins == M * ZM + X)

        val modularInputVarPins: MultiArray<List<Int>> =
            MultiArray.new(M) { (m) -> List(XM) { x -> modularInputVarPin[m, x + 1] } }
        val modularOutputVarPins: MultiArray<List<Int>> =
            MultiArray.new(M) { (m) -> List(ZM) { z -> modularOutputVarPin[m, z + 1] } }
        val externalInputVarPins: List<Int> =
            List(X) { x -> externalInputVarPin[x + 1] }
        val externalOutputVarPins: List<Int> =
            List(Z) { z -> externalOutputVarPin[z + 1] }
        val allInboundVarPins = 1.._inboundVarPins
        val allOutboundVarPins = 1.._outboundVarPins
        check(allInboundVarPins.toList() == modularInputVarPins.values.flatten() + externalOutputVarPins)
        check(allOutboundVarPins.toList() == modularOutputVarPins.values.flatten() + externalInputVarPins)

        fun pin2x(pin: Int): Int {
            // Note: `x` is 1-based
            require(pin in externalInputVarPins)
            val x = externalInputVarPins.indexOf(pin) + 1
            return x
        }

        fun pin2z(pin: Int): Int {
            // Note: `z` is 1-based
            require(pin in externalOutputVarPins)
            val z = externalOutputVarPins.indexOf(pin) + 1
            return z
        }

        fun pin2mx(pin: Int): Pair<Int, Int> {
            // Note: `m` and `x` are 1-based
            require(pin !in externalInputVarPins)
            val m = (pin - 1) / XM + 1
            val x = modularInputVarPins[m].indexOf(pin) + 1
            return Pair(m, x)
        }

        fun pin2mz(pin: Int): Pair<Int, Int> {
            // Note: `m` and `z` are 1-based
            require(pin !in externalOutputVarPins)
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

        mylog.info("Modules inbound (input var) pins = ${(1..M).map { m -> modularInputVarPins[m] }}")
        mylog.info("External inbound (output var) pins = $externalOutputVarPins")
        mylog.info("Modules outbound (output var) pins = ${(1..M).map { m -> modularOutputVarPins[m] }}")
        mylog.info("External outbound (input var) pins = $externalInputVarPins")

        // =================

        mylog.info("Declaring variables...")
        val timeStartVariables = PerformanceCounter.reference
        val nVarsStart = numberOfVariables

        val falseVar by lazy { newLiteral().also { clause(-it) } }

        val blockType = newDomainVarArray(M) {
            SystemBlockType.values().asIterable()
        }

        val modularInputVarParent = newIntVarArray(M, XM) { (m, _) ->
            (1 until m).flatMap { m_ -> modularOutputVarPins[m_] } + externalInputVarPins + 0
        }
        val externalOutputVarParent = newIntVarArray(Z) {
            modularOutputVarPins.values.flatten() + externalInputVarPins + 0
        }

        val modularState = newIntVarArray(M, V) { 0..SM }

        val modularInputVarValue = newBoolVarArray(M, XM, V)
        val modularOutputVarValue = newBoolVarArray(M, ZM, V)
        // val modularOutputVarCurrentValue = newBoolVarArray(M, ZM, V) { (m, z, v) ->
        //     if (v == 1) falseVar
        //     else modularOutputVarValue[m, z, tree.parent(v)]
        // }
        val externalInputVarValue = newBoolVarArray(X, V)
        val externalOutputVarValue = newBoolVarArray(Z, V)

        val inboundVarPinParent = IntVarArray.new(_inboundVarPins) { (pin) ->
            if (pin in externalOutputVarPins) {
                val z = pin2z(pin)
                externalOutputVarParent[z]
            } else {
                val (m, x) = pin2mx(pin)
                modularInputVarParent[m, x]
            }
        }
        val outboundVarPinChild = newIntVarArray(_outboundVarPins) { (pin) ->
            // if (pin in externalInputVarPins) {
            //     val x = pin2x(pin)
            //     allInboundVarPins + 0
            // } else {
            //     val (m,z) = pin2mz(pin)
            //     ...
            // }
            allInboundVarPins.filter { pin in inboundVarPinParent[it].domain } + 0
        }
        val inboundVarPinValue = BoolVarArray.new(_inboundVarPins, V) { (pin, v) ->
            if (pin in externalOutputVarPins) {
                val z = pin2z(pin)
                externalOutputVarValue[z, v]
            } else {
                val (m, x) = pin2mx(pin)
                modularInputVarValue[m, x, v]
            }
        }
        val outboundVarPinValue = BoolVarArray.new(_outboundVarPins, V) { (pin, v) ->
            if (pin in externalInputVarPins) {
                val x = pin2x(pin)
                externalInputVarValue[x, v]
            } else {
                val (m, z) = pin2mz(pin)
                modularOutputVarValue[m, z, v]
            }
        }

        val nVarsDiff = numberOfVariables - nVarsStart
        mylog.debug {
            "Done declaring $nVarsDiff variables (total $numberOfVariables) in %.2f s."
                .format(timeSince(timeStartVariables).seconds)
        }

        // =================

        mylog.info("Declaring constraints...")
        val timeStartConstraints = PerformanceCounter.reference
        val nClausesStart = numberOfClauses

        // comment("ADHOCs")
        // check(M == 2)
        // clause(blockType[1] eq BlockType.AND)
        // clause(blockType[2] eq BlockType.OR)
        // clause(modularInputVarParent[1, 1] eq externalInputVarPins[1 - 1])
        // clause(modularInputVarParent[1, 2] eq externalInputVarPins[2 - 1])
        // clause(modularInputVarParent[2, 1] eq modularOutputVarPins[1][1 - 1])
        // clause(modularInputVarParent[2, 2] eq externalInputVarPins[3 - 1])
        // clause(externalOutputVarParent[1] eq modularOutputVarPins[2][1 - 1])

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

        comment("Unary blocks use only the first input pin")
        for (t in SystemBlockType.values().filter { it.isUnary })
            for (m in 1..M)
                for (x in @Suppress("EmptyRange") 2..XM)
                    imply(
                        blockType[m] eq t,
                        modularInputVarParent[m, x] eq 0
                    )

        comment("Binary blocks use only the first two input pins")
        for (t in SystemBlockType.values().filter { it.isBinary })
            for (m in 1..M)
                for (x in 3..XM)
                    imply(
                        blockType[m] eq t,
                        modularInputVarParent[m, x] eq 0
                    )

        comment("FF block use only the first input pin")
        for (m in 1..M)
            for (x in 2..XM)
                imply(
                    blockType[m] eq SystemBlockType.FF,
                    modularInputVarParent[m, x] eq 0
                )

        comment("Stateless blocks use only one output")
        for (t in SystemBlockType.values().filter { !it.hasState })
            for (m in 1..M)
                for (z in 2..ZM) {
                    val pin = modularOutputVarPin[m, z]
                    imply(
                        blockType[m] eq t,
                        outboundVarPinChild[pin] eq 0
                    )
                }

        comment("FF block only use only one output")
        for (m in 1..M)
            for (z in 2..ZM) {
                val pin = modularOutputVarPin[m, z]
                imply(
                    blockType[m] eq SystemBlockType.FF,
                    outboundVarPinChild[pin] eq 0
                )
            }

        comment("Stateless blocks don't have states")
        for (t in SystemBlockType.values().filter { !it.hasState })
            for (m in 1..M)
                for (v in 1..V)
                    imply(
                        blockType[m] eq t,
                        modularState[m, v] eq 0
                    )

        comment("Stateful blocks have states")
        for (t in SystemBlockType.values().filter { it.hasState })
            for (m in 1..M)
                for (v in 1..V)
                    imply(
                        blockType[m] eq t,
                        modularState[m, v] neq 0
                    )

        comment("Initial state (at v=1) of stateful blocks is s1")
        for (t in SystemBlockType.values().filter { it.hasState })
            for (m in 1..M)
                imply(
                    blockType[m] eq t,
                    modularState[m, 1] eq 1
                )

        comment("AND block semantics")
        for (m in 1..M)
            for (v in 1..V)
                implyIffAnd(
                    blockType[m] eq SystemBlockType.AND,
                    modularOutputVarValue[m, 1, v],
                    modularInputVarValue[m, 1, v],
                    modularInputVarValue[m, 2, v]
                )

        comment("OR block semantics")
        for (m in 1..M)
            for (v in 1..V)
                implyIffOr(
                    blockType[m] eq SystemBlockType.OR,
                    modularOutputVarValue[m, 1, v],
                    modularInputVarValue[m, 1, v],
                    modularInputVarValue[m, 2, v]
                )

        comment("NOT block semantics")
        for (m in 1..M)
            for (v in 1..V)
                implyIff(
                    blockType[m] eq SystemBlockType.NOT,
                    modularOutputVarValue[m, 1, v],
                    -modularInputVarValue[m, 1, v]
                )

        comment("FF block semantics")
        // guard_s1_s2 := (_state = s1) & (x1);
        // guard_s2_s1 := (_state = s2) & (!x1);
        // next(_state) = case
        //   guard_s1_s2 : s2;
        //   guard_s2_s1 : s1;
        //   TRUE: _state;
        // esac
        // next(z1) = case
        //   guard_s1_s2 : TRUE;
        //   guard_s2_s1 : FALSE;
        //   TRUE: z1;
        // esac
        for (m in 1..M) {
            val guards = listOf(
                "s1_s2",
                "s2_s1",
            )
            val G = guards.size
            val guardValue: Map<String, BoolVarArray> = guards.associateWith { newBoolVarArray(V) }
            check(guardValue.size == guards.size)
            val guardFirstFired = newDomainVarArray(V) { (v) ->
                if (v > 1) guards + "TRUE" else emptyList()
            }
            // Note: (guardFirstFired = 0) corresponds to "else" (`TRUE` in SMV) case
            val guardNotFired = newBoolVarArray(G, V)

            for (v in 2..V) {
                val p = tree.parent(v)
                // guardFirstFired definition
                scope {
                    val guard = guards.first()
                    iff(
                        guardFirstFired[v] eq guard,
                        guardValue[guard]!![v]
                    )
                }
                for (g in 2..G) {
                    val guard = guards[g - 1]
                    iffAnd(
                        guardFirstFired[v] eq guard,
                        guardValue[guard]!![v],
                        guardNotFired[g - 1, v]
                    )
                }
                // guardNotFired definition
                scope {
                    val guard = guards.first()
                    iff(
                        guardNotFired[1, v],
                        -guardValue[guard]!![v]
                    )
                }
                for (g in 2..G) {
                    val guard = guards[g - 1]
                    iffAnd(
                        guardNotFired[g, v],
                        -guardValue[guard]!![v],
                        guardNotFired[g - 1, v]
                    )
                }
                // (FF=0) <=> NF[G] shortcut
                iff(
                    guardFirstFired[v] eq "TRUE",
                    guardNotFired[G, v]
                )
                // not-NF propagation
                for (g in 1 until G)
                    imply(
                        -guardNotFired[g, v],
                        -guardNotFired[g + 1, v]
                    )
                // Guard definition
                iffAnd(
                    guardValue["s1_s2"]!![v],
                    modularState[m, p] eq 1,
                    modularInputVarValue[m, 1, v]
                )
                iffAnd(
                    guardValue["s2_s1"]!![v],
                    modularState[m, p] eq 2,
                    -modularInputVarValue[m, 1, v]
                )
                // guardValue has no meaning outside the stateful block
                for (guard in guards)
                    imply(
                        blockType[m] neq SystemBlockType.FF,
                        -guardValue[guard]!![v]
                    )
                // else case for _state
                for (s in 1..SM)
                    implyImplyImply(
                        blockType[m] eq SystemBlockType.FF,
                        guardFirstFired[v] eq "TRUE",
                        modularState[m, p] eq s,
                        modularState[m, v] eq s
                    )
                // else case for output variables
                for (z in 1..ZM)
                    implyImplyIff(
                        blockType[m] eq SystemBlockType.FF,
                        guardFirstFired[v] eq "TRUE",
                        modularOutputVarValue[m, z, v],
                        modularOutputVarValue[m, z, p]
                    )
                // Block semantics
                implyImplyAnd(
                    blockType[m] eq SystemBlockType.FF,
                    guardFirstFired[v] eq "s1_s2",
                    modularState[m, v] eq 2,
                    modularOutputVarValue[m, 1, v]
                )
                implyImplyAnd(
                    blockType[m] eq SystemBlockType.FF,
                    guardFirstFired[v] eq "s2_s1",
                    modularState[m, v] eq 1,
                    -modularOutputVarValue[m, 1, v]
                )
            }
        }

        // for (m in 1..M)
        //     for (v in 2..V) {
        //         val p = tree.parent(v)
        //         andImplyAnd(
        //             listOf(
        //                 modularState[m, p] eq 1,
        //                 modularInputVarValue[m, 1, v],
        //             ),
        //             listOf(
        //                 modularState[m, v] eq 2,
        //                 modularOutputVarValue[m, 1, v],
        //             )
        //         )
        //         andImplyAnd(
        //             listOf(
        //                 modularState[m, p] eq 2,
        //                 -modularInputVarValue[m, 1, v],
        //             ),
        //             listOf(
        //                 modularState[m, v] eq 1,
        //                 -modularOutputVarValue[m, 1, v],
        //             )
        //         )
        //     }

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

        // comment("Initial (root) output values are false")
        // for (z in 1..Z) {
        //     clause(-externalOutputVarValue[z, 1])
        // }

        val nClausesDiff = numberOfClauses - nClausesStart
        mylog.debug {
            "Done declaring $nClausesDiff constraints (total $numberOfClauses) in %.2f s."
                .format(timeSince(timeStartConstraints).seconds)
        }

        // =================

        val fileCnf = outDir.resolve("system_X${X}_Z${Z}_M${M}.cnf")
        dumpDimacs(fileCnf)

        val model: Model? = solveAndGetModel()

        @Suppress("NAME_SHADOWING")
        if (model != null) {
            val blockType = blockType.convert(model)
            mylog.info("blockType = ${blockType.values}")

            val parent = inboundVarPinParent.convert(model)
            for (m in 1..M) {
                mylog.info("(m = $m) parent = ${modularInputVarPins[m].map { parent[it] }}")
            }
            mylog.info("(external) parent = ${externalOutputVarPins.map { parent[it] }}")

            val inboundValue = inboundVarPinValue.convert(model)
            val outboundValue = outboundVarPinValue.convert(model)
            mylog.just("  v   tree.input -> tree.output :: input > input modular > output modular > output ext")
            for (v in 1..V) {
                mylog.just(
                    "  v=$v" +
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
                    val ts = sequence {
                        for (pin in modularInputVarPins[m]) {
                            when (val par = parent[pin]) {
                                0 -> {
                                    break
                                }
                                in externalInputVarPins -> {
                                    yield("X${externalInputVarPins.indexOf(par) + 1}")
                                }
                                else -> {
                                    yield("g${(par - 1) / ZM + 1}")
                                }
                            }
                        }
                    }
                    for ((i, t) in ts.withIndex(start = 1)) {
                        writeln("    g$m -> $t [label=\"$i\"];")
                    }
                }
                for (z in 1..Z) {
                    val pin = externalOutputVarPin[z]
                    val par = parent[pin]
                    if (par == 0) continue
                    val t = if (par in externalInputVarPins) {
                        "X${externalInputVarPins.indexOf(par) + 1}"
                    } else {
                        "g${(par - 1) / ZM + 1}"
                    }
                    writeln("    Z$z -> $t;")
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
    maxM: Int = 30,
) {
    for (M in 1..maxM) {
        mylog.br()
        mylog.info("Trying M = $M...")

        if (synthesizeStatefulSystem(tree, M = M, X = X, Z = Z)) {
            mylog.success("M = $M: Success")
            break
        } else {
            mylog.failure("M = $M: Could not infer schema.")
        }
    }
}

@Suppress("UnnecessaryVariable")
private fun main() {
    val timeStart = PerformanceCounter.reference

    val X = 5
    val Z = 1

    Globals.IS_DEBUG = true
    Globals.INITIAL_OUTPUT_VALUES = OutputValues.zeros(Z)

    val tree = PositiveScenarioTree(
        inputEvents = listOf(InputEvent("REQ")),
        outputEvents = listOf(OutputEvent("CNF")),
        inputNames = (1..X).map { x -> "x$x" },
        outputNames = (1..Z).map { z -> "z$z" },
        isTrie = false
    )

    // TODO: populate the tree

    fun elem(inputValues: InputValues, outputValues: OutputValues): ScenarioElement =
        ScenarioElement(
            InputAction(InputEvent("REQ"), inputValues),
            OutputAction(OutputEvent("CNF"), outputValues)
        )

    fun elem(inputValues: String, outputValues: String): ScenarioElement =
        elem(InputValues(inputValues), OutputValues(outputValues))

    fun elem(values: Pair<String, String>): ScenarioElement =
        elem(values.first, values.second)

    // val tt = listOf(
    //     "000" to "0",
    //     "001" to "1",
    //     "010" to "0",
    //     "011" to "1",
    //     "100" to "0",
    //     "101" to "1",
    //     "110" to "1",
    //     "111" to "1",
    // )
    // val es = tt.map { elem(it) }

    val tt = "00000000000000000000000010110110"
    val es = tt.toTruthTable(X = X).map { (input, output) ->
        elem(InputValues(input.values), OutputValues(listOf(output)))
    }

    val elements = es
    tree.addScenario(PositiveScenario(elements))
    tree.addScenario(PositiveScenario(es.shuffled()))
    tree.addScenario(PositiveScenario(es.shuffled()))

    val M = 10
    if (synthesizeStatefulSystem(tree, M = M, X = X, Z = Z)) {
        mylog.success("M = $M: SAT")
    } else {
        mylog.failure("M = $M: UNSAT")
    }

    // synthesizeIterativeBottomUp(tree, X = X, Z = Z, maxM = 12)

    mylog.success("All done in %.3f s.".format(timeSince(timeStart).seconds))
}
