@file:Suppress("LocalVariableName")

package ru.ifmo.fbsat.core.task.extra.circuit

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.satlib.core.Model
import com.github.lipen.satlib.core.convert
import com.github.lipen.satlib.core.eq
import com.github.lipen.satlib.core.neq
import com.github.lipen.satlib.core.newBoolVarArray
import com.github.lipen.satlib.core.newDomainVarArray
import com.github.lipen.satlib.core.newIntVarArray
import com.github.lipen.satlib.core.sign
import com.github.lipen.satlib.op.imply
import com.github.lipen.satlib.op.implyIff
import com.github.lipen.satlib.op.implyIffAnd
import com.github.lipen.satlib.op.implyIffOr
import com.github.lipen.satlib.solver.MiniSatSolver
import com.soywiz.klock.PerformanceCounter
import okio.buffer
import okio.sink
import ru.ifmo.fbsat.core.solver.clause
import ru.ifmo.fbsat.core.solver.solveAndGetModel
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.timeSince
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.useWith
import ru.ifmo.fbsat.core.utils.writeln
import java.io.File

private val logger = MyLogger {}

@Suppress("LocalVariableName")
fun synthesizeBooleanCircuit(tt: Map<Input, Output>, M: Int, X: Int, Z: Int): Boolean {
    val solver = MiniSatSolver()
    val outDir = File("out/circuit-synthesis")
    outDir.mkdirs()

    with(solver) {
        val XM = 2
        val ZM = 1

        val modularInboundVarPins: MultiArray<List<Int>> =
            MultiArray.new(M) { (m) -> (1..XM).map { (m - 1) * XM + it } }
        val modularOutboundVarPins: MultiArray<List<Int>> =
            MultiArray.new(M) { (m) -> (1..ZM).map { (m - 1) * ZM + it } }
        val externalInboundVarPins: List<Int> =
            (1..Z).map { modularInboundVarPins.values.flatten().size + it }
        val externalOutboundVarPins: List<Int> =
            (1..X).map { modularOutboundVarPins.values.flatten().size + it }
        val allInboundVarPins: List<Int> =
            modularInboundVarPins.values.flatten() + externalInboundVarPins
        val allOutboundVarPins: List<Int> =
            modularOutboundVarPins.values.flatten() + externalOutboundVarPins

        logger.info("Modules inbound pins = ${modularInboundVarPins.values}")
        logger.info("External inbound pins = $externalInboundVarPins")
        logger.info("Modules outbound pins = ${modularOutboundVarPins.values}")
        logger.info("External outbound pins = $externalOutboundVarPins")

        val uniqueInputs: List<Input> = tt.keys.sortedBy { it.values.toBinaryString() }
        val U = uniqueInputs.size

        // =================

        logger.info("Declaring variables...")
        val timeStartVariables = PerformanceCounter.reference
        val nVarsStart = numberOfVariables

        val blockType = newDomainVarArray(M) {
            BlockType.values().asIterable()
        }
        val inboundVarPinParent = newIntVarArray(allInboundVarPins.size) { (p) ->
            // (0..allOutboundVarPins.size)
            if (p in externalInboundVarPins) {
                (0..allOutboundVarPins.size)
            } else {
                val m = (p - 1) / XM + 1
                (1 until m).flatMap { m_ -> modularOutboundVarPins[m_] } + externalOutboundVarPins + 0
            }
        }
        val inboundVarPinValue = newBoolVarArray(allInboundVarPins.size, U)
        val outboundVarPinValue = newBoolVarArray(allOutboundVarPins.size, U)

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
            // for (par in 1..allOutboundVarPins.size)
                for (u in 1..U)
                    implyIff(
                        inboundVarPinParent[pin] eq par,
                        inboundVarPinValue[pin, u],
                        outboundVarPinValue[par, u]
                    )

        comment("Parent-less pins have false values")
        for (pin in allInboundVarPins)
            for (u in 1..U)
                imply(
                    inboundVarPinParent[pin] eq 0,
                    -inboundVarPinValue[pin, u]
                )

        // comment("AND/OR/XOR pins' parents order")
        // for (t in listOf(BlockType.AND, BlockType.OR, BlockType.XOR))
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

        // comment("AND/OR/XOR inbound pin parents are different")
        // for (t in listOf(BlockType.AND, BlockType.OR, BlockType.XOR))
        //     for (m in 1..M) {
        //         check(modularInboundVarPins[m].size == 2)
        //         val (pin1, pin2) = modularInboundVarPins[m]
        //         for (par in inboundVarPinParent[pin1].domain - 0)
        //             implyImply(
        //                 blockType[m] eq t,
        //                 inboundVarPinParent[pin1] eq par,
        //                 inboundVarPinParent[pin2] neq par
        //             )
        //     }

        comment("NOT block has its second pin unused")
        for (m in 1..M)
            imply(
                blockType[m] eq BlockType.NOT,
                inboundVarPinParent[modularInboundVarPins[m][1]] eq 0
            )

        comment("AND block semantics")
        for (m in 1..M)
            for (pin in modularOutboundVarPins[m])
                for (u in 1..U)
                    implyIffAnd(
                        blockType[m] eq BlockType.AND,
                        outboundVarPinValue[pin, u],
                        inboundVarPinValue[modularInboundVarPins[m][0], u],
                        inboundVarPinValue[modularInboundVarPins[m][1], u]
                    )

        comment("OR block semantics")
        for (m in 1..M)
            for (pin in modularOutboundVarPins[m])
                for (u in 1..U)
                    implyIffOr(
                        blockType[m] eq BlockType.OR,
                        outboundVarPinValue[pin, u],
                        inboundVarPinValue[modularInboundVarPins[m][0], u],
                        inboundVarPinValue[modularInboundVarPins[m][1], u]
                    )

        // comment("XOR block semantics")
        // for (m in 1..M)
        //     for (pin in modularOutboundVarPins[m])
        //         for (u in 1..U)
        //             implyIffXor(
        //                 blockType[m] eq BlockType.XOR,
        //                 outboundVarPinValue[pin, u],
        //                 inboundVarPinValue[modularInboundVarPins[m][0], u],
        //                 inboundVarPinValue[modularInboundVarPins[m][1], u]
        //             )

        // comment("IMPLY block semantics")
        // for (m in 1..M)
        //     for (pin in modularOutboundVarPins[m])
        //         for (u in 1..U)
        //             implyIffOr(
        //                 blockType[m] eq BlockType.IMPLY,
        //                 outboundVarPinValue[pin, u],
        //                 -inboundVarPinValue[modularInboundVarPins[m][0], u],
        //                 inboundVarPinValue[modularInboundVarPins[m][1], u]
        //             )

        comment("NOT block semantics")
        for (m in 1..M)
            for (pin in modularOutboundVarPins[m])
                for (u in 1..U)
                    implyIff(
                        blockType[m] eq BlockType.NOT,
                        outboundVarPinValue[pin, u],
                        -inboundVarPinValue[modularInboundVarPins[m][0], u]
                    )

        comment("Input values")
        for (x in 1..X)
            for (u in 1..U) {
                val pin = externalOutboundVarPins[x - 1]
                val value = uniqueInputs[u - 1].values[x - 1]
                clause(outboundVarPinValue[pin, u] sign value)
            }

        comment("Output values")
        for (z in 1..Z)
            for (u in 1..U) {
                val pin = externalInboundVarPins[z - 1]
                val value = tt.getValue(uniqueInputs[u - 1]).values[z - 1]
                clause(inboundVarPinValue[pin, u] sign value)
            }

        val nClausesDiff = numberOfClauses - nClausesStart
        logger.debug {
            "Done declaring $nClausesDiff constraints (total $numberOfClauses) in %.2f s."
                .format(timeSince(timeStartConstraints).seconds)
        }

        // =================

        // ADHOCs

        comment("Adhoc: All pins have parents")
        // for (t in listOf(BlockType.NOT, BlockType.AND, BlockType.OR, BlockType.XOR))
        //     for (m in 1..M)
        //         imply(
        //             blockType[m] eq t,
        //             inboundVarPinParent[modularInboundVarPins[m][0]] neq 0
        //         )
        // for (t in listOf(BlockType.AND, BlockType.OR, BlockType.XOR))
        //     for (m in 1..M)
        //         imply(
        //             blockType[m] eq BlockType.AND,
        //             inboundVarPinParent[modularInboundVarPins[m][1]] neq 0
        //         )
        for (pin in externalInboundVarPins)
            clause(inboundVarPinParent[pin] neq 0)

        // comment("Adhoc: All modular outbound pins must be used")
        // for (m in 1..M)
        //     for (par in modularOutboundVarPins[m])
        //         atLeastOne {
        //             for (pin in allInboundVarPins)
        //                 if (par in inboundVarPinParent[pin].domain)
        //                     yield(inboundVarPinParent[pin] eq par)
        //         }

        // comment("Adhoc: at least some number of input variables must be used")
        // val cardUnusedVars = declareCardinality {
        //     for (x in 1..X) {
        //         val p = externalOutboundVarPins[x - 1]
        //         val aux = newLiteral()
        //         iffOr(aux) {
        //             for (m in 1..M)
        //                 for (pin in modularInboundVarPins[m])
        //                     yield(inboundVarPinParent[pin] eq p)
        //         }
        //         yield(-aux)
        //     }
        // }
        // cardUnusedVars.updateUpperBoundLessThanOrEqual(X - 16)

        // =================

        val fileCnf = outDir.resolve("circuit_X${X}_Z${Z}_M${M}.cnf")
        dumpDimacs(fileCnf)

        val model: Model? = solveAndGetModel()
        // val model: Model? = null

        @Suppress("NAME_SHADOWING")
        if (model != null) {
            val blockType = blockType.convert(model)
            logger.info("blockType = ${blockType.values}")

            val parent = inboundVarPinParent.convert(model)
            for (m in 1..M) {
                logger.info("(m = $m) parent = ${modularInboundVarPins[m].map { parent[it] }}")
            }
            logger.info("(external) parent = ${externalInboundVarPins.map { parent[it] }}")

            val inboundValue = inboundVarPinValue.convert(model)
            val outboundValue = outboundVarPinValue.convert(model)
            logger.info("values (input -> external outbound -> modules inbound -> modules outbound -> external inbound):")
            for (u in 1..U) {
                logger.just(
                    "  u=${u.toString().padEnd(3)}" +
                        " ${uniqueInputs[u - 1]}" +
                        " -> ${
                            (1..X).map { x ->
                                outboundValue[externalOutboundVarPins[x - 1], u].toInt()
                            }
                        }" +
                        " -> ${
                            (1..M).map { m ->
                                modularInboundVarPins[m].map { inboundValue[it, u] }.toBinaryString()
                            }
                        }" +
                        " -> ${
                            (1..M).map { m ->
                                outboundValue[modularOutboundVarPins[m][0], u].toInt()
                            }
                        }" +
                        " -> ${
                            (1..Z).map { z ->
                                inboundValue[externalInboundVarPins[z - 1], u].toInt()
                            }
                        }"
                )
            }

            val fileCircuit = outDir.resolve("circuit.gv")
            fileCircuit.sink().buffer().useWith {
                writeln("digraph {")
                writeln("    // Gates")
                for (m in 1..M) {
                    writeln("    g$m [label=\"$m: ${blockType[m]}\"];")
                }
                writeln("    // Input variables")
                writeln("    { rank=max")
                for (x in 1..X) {
                    writeln("    X$x [label=\"X$x\"];")
                }
                writeln("    }")
                writeln("    // Output variables")
                writeln("    { rank=min")
                for (z in 1..Z) {
                    writeln("    Z$z [label=\"Z$z\"];")
                }
                writeln("  }")
                writeln("")
                writeln("    // Circuit")
                for (m in 1..M) {
                    val ts = mutableListOf<String>()
                    for (pin in modularInboundVarPins[m]) {
                        val par = parent[pin]
                        if (par == 0) {
                            break
                        } else if (par in externalOutboundVarPins) {
                            ts.add("X${externalOutboundVarPins.indexOf(par) + 1}")
                        } else {
                            ts.add("g${(par - 1) / ZM + 1}")
                        }
                    }
                    writeln("    g$m -> {${ts.joinToString(",")}};")
                }
                for (z in 1..Z) {
                    val pin = externalInboundVarPins[z - 1]
                    val par = parent[pin]
                    if (par == 0) continue
                    val t = if (par in externalOutboundVarPins) {
                        "X${externalOutboundVarPins.indexOf(par) + 1}"
                    } else {
                        "g${(par - 1) / ZM + 1}"
                    }
                    writeln("    Z$z -> $t;")
                }
                writeln("}")
            }
            Runtime.getRuntime().exec("dot -Tpdf -O $fileCircuit")
            Runtime.getRuntime().exec("dot -Tpng -O $fileCircuit")

            return true
        }
    }

    return false
}

fun synthesizeBooleanCircuitIterativelyBottomUp(tt: Map<Input, Output>, X: Int, Z: Int) {
    for (M in 1..30) {
        logger.just("brrr...")
        logger.info("Trying M = $M for X=$X, Z=$Z...")

        if (synthesizeBooleanCircuit(tt, M = M, X = X, Z = Z)) {
            logger.info("M = $M: Success")
            break
        } else {
            logger.info("M = $M: Could not infer circuit.")
        }
    }
}

private fun main() {
    val timeStart = PerformanceCounter.reference
    Globals.IS_DEBUG = true

    val Z = 1
    // val X = 3
    // val values = "10010110"
    // val X = 4
    // val values = "0000001101101001" // 11 sat (28s), 10 unknown
    val X = 5
    val values = "00000000000000000000000010110110" // 10 sat (1s w/ ms, 55s w/ cms), 9 unknown (>10m)
    val tt = values.toTruthTable(X).mapValues { Output(listOf(it.value)) }

    // synthesizeBooleanCircuitIterativelyBottomUp(tt, X = X, Z = Z)
    synthesizeBooleanCircuit(tt, M = 10, X = X, Z = Z)

    // val X = 32
    // val Z = 16
    // val out1 = Output("1011100110110101")
    // val out2 = Output("0100111010001101")
    // val out3 = Output("0101101010000110")
    // val out4 = Output("0110010001110100")
    // val dataAll_ =
    //     data1.map { it to out1 } +
    //         data2.map { it to out2 } +
    //         data3.map { it to out3 } +
    //         data4.map { it to out4 }
    // val dataAll = dataAll_.map { (i, o) -> i[0 until X] to o }
    // check(dataAll.map { it.first }.distinct().size == dataAll.size)
    // check(dataAll.distinctBy { it.first + it.second.values.toBinaryString() }.size == dataAll.size)
    // val tt = dataAll.toMap().mapKeys { (i, _) -> Input(i) }
    // check(tt.size == dataAll.size)

    logger.info("All done in %.3f s.".format(timeSince(timeStart).seconds))
}
