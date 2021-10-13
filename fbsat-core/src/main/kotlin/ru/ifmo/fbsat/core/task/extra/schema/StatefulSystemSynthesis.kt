@file:Suppress("DuplicatedCode")

package ru.ifmo.fbsat.core.task.extra.schema

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import com.github.lipen.satlib.core.BoolVarArray
import com.github.lipen.satlib.core.IntVarArray
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
import com.github.lipen.satlib.op.implyImply
import com.github.lipen.satlib.op.implyImplyIff
import com.github.lipen.satlib.op.implyImplyImply
import com.github.lipen.satlib.op.implyImplyImplyAnd
import com.github.lipen.satlib.solver.GlucoseSolver
import com.soywiz.klock.PerformanceCounter
import okio.buffer
import okio.sink
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.negative.readCounterexamplesFromFile
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.clause
import ru.ifmo.fbsat.core.solver.implyIffXor
import ru.ifmo.fbsat.core.solver.solveAndGetModel
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.timeSince
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.useWith
import ru.ifmo.fbsat.core.utils.withIndex
import ru.ifmo.fbsat.core.utils.writeln
import java.io.File

private val logger = MyLogger {}

@Suppress("LocalVariableName", "UnnecessaryVariable")
fun synthesizeStatefulSystem(
    tree: PositiveScenarioTree,
    M: Int,
    X: Int,
    Z: Int,
): Boolean {
    // TODO: input should be a list of traces (in SMV sense)

    val solver = GlucoseSolver()
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

        logger.info("Modules inbound (input var) pins = ${(1..M).map { m -> modularInputVarPins[m] }}")
        logger.info("External inbound (output var) pins = $externalOutputVarPins")
        logger.info("Modules outbound (output var) pins = ${(1..M).map { m -> modularOutputVarPins[m] }}")
        logger.info("External outbound (input var) pins = $externalInputVarPins")

        // =================

        logger.info("Declaring variables...")
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
        for (t in SystemBlockType.values())
            for (m in 1..M)
                for (x in (t.numberOfInputs + 1)..XM)
                    imply(
                        blockType[m] eq t,
                        modularInputVarParent[m, x] eq 0
                    )

        comment("Number of output pins")
        for (t in SystemBlockType.values())
            for (m in 1..M)
                for (z in (t.numberOfOutputs + 1)..ZM) {
                    val pin = modularOutputVarPin[m, z]
                    imply(
                        blockType[m] eq t,
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

        // comment("FF block semantics")
        // // guard_s1_s2 := (_state = s1) & (x1);
        // // guard_s2_s1 := (_state = s2) & (!x1);
        // // next(_state) = case
        // //   guard_s1_s2 : s2;
        // //   guard_s2_s1 : s1;
        // //   TRUE: _state;
        // // esac
        // // next(z1) = case
        // //   guard_s1_s2 : TRUE;
        // //   guard_s2_s1 : FALSE;
        // //   TRUE: z1;
        // // esac
        // for (m in 1..M) {
        //     val guards = listOf(
        //         "s1_s2",
        //         "s2_s1",
        //     )
        //     val G = guards.size
        //     val guardValue: Map<String, BoolVarArray> = guards.associateWith { newBoolVarArray(V) }
        //     check(guardValue.size == guards.size)
        //     val guardFirstFired = newDomainVarArray(V) { (v) ->
        //         if (v > 1) guards + "TRUE" else emptyList()
        //     }
        //     // Note: (guardFirstFired = 0) corresponds to "else" (`TRUE` in SMV) case
        //     val guardNotFired = newBoolVarArray(G, V)
        //
        //     for (v in 2..V) {
        //         val p = tree.parent(v)
        //         // guardFirstFired definition
        //         scope {
        //             val guard = guards.first()
        //             iff(
        //                 guardFirstFired[v] eq guard,
        //                 guardValue[guard]!![v]
        //             )
        //         }
        //         for (g in 2..G) {
        //             val guard = guards[g - 1]
        //             iffAnd(
        //                 guardFirstFired[v] eq guard,
        //                 guardValue[guard]!![v],
        //                 guardNotFired[g - 1, v]
        //             )
        //         }
        //         // guardNotFired definition
        //         scope {
        //             val guard = guards.first()
        //             iff(
        //                 guardNotFired[1, v],
        //                 -guardValue[guard]!![v]
        //             )
        //         }
        //         for (g in 2..G) {
        //             val guard = guards[g - 1]
        //             iffAnd(
        //                 guardNotFired[g, v],
        //                 -guardValue[guard]!![v],
        //                 guardNotFired[g - 1, v]
        //             )
        //         }
        //         // (FF=0) <=> NF[G] shortcut
        //         iff(
        //             guardFirstFired[v] eq "TRUE",
        //             guardNotFired[G, v]
        //         )
        //         // not-NF propagation
        //         for (g in 1 until G)
        //             imply(
        //                 -guardNotFired[g, v],
        //                 -guardNotFired[g + 1, v]
        //             )
        //         // Guard definition
        //         iffAnd(
        //             guardValue["s1_s2"]!![v],
        //             modularState[m, p] eq 1,
        //             modularInputVarValue[m, 1, v]
        //         )
        //         iffAnd(
        //             guardValue["s2_s1"]!![v],
        //             modularState[m, p] eq 2,
        //             -modularInputVarValue[m, 1, v]
        //         )
        //         // guardValue has no meaning outside the stateful block
        //         for (guard in guards)
        //             imply(
        //                 blockType[m] neq SystemBlockType.FF,
        //                 -guardValue[guard]!![v]
        //             )
        //         // else case for _state
        //         for (s in 1..SM)
        //             implyImplyImply(
        //                 blockType[m] eq SystemBlockType.FF,
        //                 guardFirstFired[v] eq "TRUE",
        //                 modularState[m, p] eq s,
        //                 modularState[m, v] eq s
        //             )
        //         // else case for output variables
        //         for (z in 1..ZM)
        //             implyImplyIff(
        //                 blockType[m] eq SystemBlockType.FF,
        //                 guardFirstFired[v] eq "TRUE",
        //                 modularOutputVarValue[m, z, v],
        //                 modularOutputVarValue[m, z, p]
        //             )
        //         // Block semantics
        //         implyImplyAnd(
        //             blockType[m] eq SystemBlockType.FF,
        //             guardFirstFired[v] eq "s1_s2",
        //             modularState[m, v] eq 2,
        //             modularOutputVarValue[m, 1, v]
        //         )
        //         implyImplyAnd(
        //             blockType[m] eq SystemBlockType.FF,
        //             guardFirstFired[v] eq "s2_s1",
        //             modularState[m, v] eq 1,
        //             -modularOutputVarValue[m, 1, v]
        //         )
        //     }
        // }

        comment("Half adder block semantics")
        for (m in 1..M)
            for (v in 1..V) {
                val A = 1
                val B = 2
                val SUM = 1
                val CARRY = 2

                // sum := a xor b;
                implyIffXor(
                    blockType[m] eq SystemBlockType.HA,
                    modularOutputVarValue[m, SUM, v],
                    modularInputVarValue[m, A, v],
                    modularInputVarValue[m, B, v]
                )
                // carry := a & b;
                implyIffAnd(
                    blockType[m] eq SystemBlockType.HA,
                    modularOutputVarValue[m, CARRY, v],
                    modularInputVarValue[m, A, v],
                    modularInputVarValue[m, B, v]
                )
            }

        comment("Counter block semantics")
        // MODULE Counter(carry_in)
        // -- input[1]: carry_in
        // -- output[2]: value, carry_out
        // VAR
        //     value: boolean;
        // ASSIGN
        //     init(value) := FALSE;
        //     next(value) := value xor carry_in;
        // DEFINE
        //     carry_out := value & carry_in;
        for (m in 1..M)
            for (v in 2..V) {
                val p = tree.parent(v)

                val S1 = 1
                val S2 = 2
                val VALUE = 1
                val CARRY_OUT = 2

                // =========
                //  INPUT=1
                // =========
                // (prevState=1) & (input=1) => (nextState=2) & (value=1) & (carry_out=0)
                implyImplyImplyAnd(
                    blockType[m] eq SystemBlockType.CNT,
                    modularState[m, p] eq S1,
                    modularInputVarValue[m, VALUE, v],
                    modularState[m, v] eq S2,
                    modularOutputVarValue[m, VALUE, v],
                    -modularOutputVarValue[m, CARRY_OUT, v]
                )
                // (prevState=2) & (input=1) => (nextState=1) & (value=0) & (carry_out=1)
                implyImplyImplyAnd(
                    blockType[m] eq SystemBlockType.CNT,
                    modularState[m, p] eq S2,
                    modularInputVarValue[m, VALUE, v],
                    modularState[m, v] eq S1,
                    -modularOutputVarValue[m, VALUE, v],
                    modularOutputVarValue[m, CARRY_OUT, v]
                )

                // =========
                //  INPUT=0
                // =========
                // (input=0) => (nextState=prevState)
                for (s in 1..2) {
                    implyImplyImply(
                        blockType[m] eq SystemBlockType.CNT,
                        -modularInputVarValue[m, VALUE, v],
                        modularState[m, p] eq s,
                        modularState[m, v] eq s
                    )
                }
                // (input=0) => (value=value)
                implyImplyIff(
                    blockType[m] eq SystemBlockType.CNT,
                    -modularInputVarValue[m, VALUE, v],
                    modularOutputVarValue[m, VALUE, p],
                    modularOutputVarValue[m, VALUE, v]
                )
                // (input=0) => (carry_out=0)
                implyImply(
                    blockType[m] eq SystemBlockType.CNT,
                    -modularInputVarValue[m, VALUE, v],
                    -modularOutputVarValue[m, CARRY_OUT, v]
                )

                // forbid 3+ states
                @Suppress("EmptyRange")
                for (s in 3..SM)
                    imply(
                        blockType[m] eq SystemBlockType.CNT,
                        modularState[m, v] neq s
                    )
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

        // =================

        comment("ADHOCs")

        // // Minor hardcode: only first CNTs
        // if (M >= 3) {
        //     clause(blockType[1] eq SystemBlockType.CNT)
        //     clause(blockType[3] eq SystemBlockType.CNT)
        //     clause(modularInputVarParent[1, 1] eq externalOutputVarPin[1])
        //     clause(modularInputVarParent[3, 1] eq externalOutputVarPin[2])
        // }
        // // Hardcode CNTs (2-bit counters for each of two inputs)
        // if (M >= 4) {
        //     clause(blockType[1] eq SystemBlockType.CNT)
        //     clause(blockType[2] eq SystemBlockType.CNT)
        //     clause(blockType[3] eq SystemBlockType.CNT)
        //     clause(blockType[4] eq SystemBlockType.CNT)
        //     clause(modularInputVarParent[1, 1] eq externalOutputVarPin[1])
        //     clause(modularInputVarParent[2, 1] eq modularOutputVarPin[1, 2])
        //     clause(modularInputVarParent[3, 1] eq externalOutputVarPin[2])
        //     clause(modularInputVarParent[4, 1] eq modularOutputVarPin[3, 2])
        // }
        // // Hardcode FAs (HA+HA+OR + HA+HA)
        // if (M >= 9) {
        //     clause(blockType[5] eq SystemBlockType.HA)
        //     clause(blockType[6] eq SystemBlockType.HA)
        //     clause(blockType[7] eq SystemBlockType.OR)
        //     clause(blockType[8] eq SystemBlockType.HA)
        //     clause(blockType[9] eq SystemBlockType.HA)
        //     // First HA for bit0
        //     clause(modularInputVarParent[5, 1] eq modularOutputVarPin[1, 1])
        //     clause(modularInputVarParent[5, 2] eq modularOutputVarPin[3, 1])
        //     // Second HA for bit0
        //     clause(modularInputVarParent[6, 1] eq modularOutputVarPin[5, 1])
        //     clause(modularInputVarParent[6, 2] eq 0)
        //     // OR for bit0
        //     clause(modularInputVarParent[7, 1] eq modularOutputVarPin[5, 2])
        //     clause(modularInputVarParent[7, 2] eq modularOutputVarPin[6, 2])
        //     // First HA for bit1
        //     clause(modularInputVarParent[8, 1] eq modularOutputVarPin[2, 1])
        //     clause(modularInputVarParent[8, 2] eq modularOutputVarPin[4, 1])
        //     // Second HA for bit1
        //     clause(modularInputVarParent[9, 1] eq modularOutputVarPin[8, 1])
        //     clause(modularInputVarParent[9, 2] eq modularOutputVarPin[7, 1])
        //     // Z1 is from second HA for bit0
        //     clause(externalOutputVarParent[1] eq modularOutputVarPin[6, 1])
        //     // Z2 is from second HA for bit1
        //     clause(externalOutputVarParent[2] eq modularOutputVarPin[9, 1])
        // }

        comment("Outputs of stateless blocks are always connected")
        for (t in SystemBlockType.values().filter { !it.hasState })
            for (z in 1..t.numberOfOutputs)
                for (m in 1..M)
                    imply(
                        blockType[m] eq t,
                        outboundVarPinChild[modularOutputVarPin[m, z]] neq 0
                    )

        comment("Value-outputs of CNT blocks are always connected")
        for (m in 1..M)
            imply(
                blockType[m] eq SystemBlockType.CNT,
                outboundVarPinChild[modularOutputVarPin[m, 1]] neq 0
            )

        // =================

        val nClausesDiff = numberOfClauses - nClausesStart
        logger.debug {
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
            logger.info("blockType = ${blockType.values}")

            val parent = inboundVarPinParent.convert(model)
            for (m in 1..M) {
                logger.info("(m = $m) parent = ${modularInputVarPins[m].map { parent[it] }}")
            }
            logger.info("(external) parent = ${externalOutputVarPins.map { parent[it] }}")

            val inboundValue = inboundVarPinValue.convert(model)
            val outboundValue = outboundVarPinValue.convert(model)
            val modularState = modularState.convert(model)
            logger.just("        v   tree.input -> tree.output(rev) :: input > [input modular] > [state modular] > [output modular] > output ext")
            for (v in 1..V) {
                logger.just(
                    "[fold]  v=$v" +
                        " ${
                            if (v == 1) "-".repeat(X)
                            else (1..X).map { x -> tree.inputValue(v, x) }.toBinaryString()
                        }" +
                        " -> ${(1..Z).reversed().map { z -> tree.outputValue(v, z) }.toBinaryString()}" +
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
) {
    for (M in minM..maxM) {
        logger.just("brrr...")
        logger.info("Trying M = $M...")
        val timeStart = PerformanceCounter.reference

        if (synthesizeStatefulSystem(tree, M = M, X = X, Z = Z)) {
            logger.info("M = $M: Success after %.2f s!".format(timeSince(timeStart).seconds))
            break
        } else {
            logger.info("M = $M: Could not infer system after %.2f s.".format(timeSince(timeStart).seconds))
        }
    }
}

@Suppress("UnnecessaryVariable")
private fun main() {
    val timeStart = PerformanceCounter.reference

    // val X = 5
    // val Z = 1

    // val inputNames = listOf("in_x", "in_y")
    // val outputNames = listOf("out0", "out1", "out2")
    // val outputNames = listOf("out0", "out1", "out2", "out3")
    // val outputNames = listOf("counter_x_bit0", "counter_x_bit1", "counter_x_bit2")

    // val inputNames = listOf("in_x")
    // val outputNames = listOf("counter_x_bit0", "counter_x_bit1")
    // val outputNames = listOf("counter_x_bit0", "counter_x_bit1", "counter_x_bit2")

    val inputNames = listOf("in_x", "in_y")
    val outputNames = listOf("out0", "out1")
    // val outputNames = listOf("out0", "out1", "out2")
    // val outputNames = listOf("out0", "out1", "out2", "out3")
    val X = inputNames.size
    val Z = outputNames.size

    Globals.IS_DEBUG = true

    val tree = PositiveScenarioTree(
        inputEvents = listOf(InputEvent("REQ")),
        outputEvents = listOf(OutputEvent("CNF")),
        // inputNames = (1..X).map { x -> "x$x" },
        // outputNames = (1..Z).map { z -> "z$z" },
        inputNames = inputNames,
        outputNames = outputNames,
        isTrie = false
    )

    // val traceFilename = "data/counter/trace_counter2_10x30.xml"
    // val traceFilename = "data/counter/trace_counter3_10x30.xml"

    // val traceFilename = "data/counter/trace_half-adding2_10x30.xml"
    // val traceFilename = "data/counter/trace_half-adding2_20x50.xml"
    // val traceFilename = "data/counter/trace_half-adding2_100x20.xml"

    val traceFilename = "data/counter/trace_adding2_10x30.xml"
    // val traceFilename = "data/counter/trace_adding2_20x50.xml"
    // val traceFilename = "data/counter/trace_adding2_100x20.xml"
    val traces = readCounterexamplesFromFile(File(traceFilename))
    logger.info("Traces: ${traces.size}")
    val scenarios = traces.map { trace ->
        PositiveScenario.fromTrace(trace, inputNames, outputNames)
    }
    for (scenario in scenarios) {
        logger.info("Adding scenario of length ${scenario.elements.size}")
        tree.addScenario(scenario)
        if (tree.scenarios.size == 1) {
            for (element in scenario.elements) {
                println("[fold] $element")
            }
        }
    }
    logger.info("Tree size: ${tree.size}")
    logger.info("Tree input names: ${tree.inputNames}")
    logger.info("Tree output names: ${tree.outputNames}")

    // val M = 10
    // if (synthesizeStatefulSystem(tree, M = M, X = X, Z = Z)) {
    //     logger.success("M = $M: SAT")
    // } else {
    //     logger.failure("M = $M: UNSAT")
    // }

    synthesizeStatefulSystemIterativelyBottomUp(tree, X = X, Z = Z, minM = 1, maxM = 10)
    // synthesizeStatefulSystemIterativelyBottomUp(tree, X = X, Z = Z, minM = 8, maxM = 10)
    // synthesizeStatefulSystemIterativelyBottomUp(tree, X = X, Z = Z, minM = 9, maxM = 9)
    // synthesizeStatefulSystemIterativelyBottomUp(tree, X = X, Z = Z, minM = 10, maxM = 10)
    // synthesizeStatefulSystem(tree, M = 10, X = X, Z = Z)

    logger.info("All done in %.3f s.".format(timeSince(timeStart).seconds))
}

// TODO: populate the tree

// fun elem(inputValues: InputValues, outputValues: OutputValues): ScenarioElement =
//     ScenarioElement(
//         InputAction(InputEvent("REQ"), inputValues),
//         OutputAction(OutputEvent("CNF"), outputValues)
//     )
//
// fun elem(inputValues: String, outputValues: String): ScenarioElement =
//     elem(InputValues(inputValues), OutputValues(outputValues))
//
// fun elem(values: Pair<String, String>): ScenarioElement =
//     elem(values.first, values.second)
//
// // val tt = listOf(
// //     "000" to "0",
// //     "001" to "1",
// //     "010" to "0",
// //     "011" to "1",
// //     "100" to "0",
// //     "101" to "1",
// //     "110" to "1",
// //     "111" to "1",
// // )
// // val es = tt.map { elem(it) }
//
// val tt = "00000000000000000000000010110110"
// val es = tt.toTruthTable(X = X).map { (input, output) ->
//     elem(InputValues(input.values), OutputValues(listOf(output)))
// }
//
// val elements = es
// tree.addScenario(PositiveScenario(elements))
// tree.addScenario(PositiveScenario(es.shuffled()))
// tree.addScenario(PositiveScenario(es.shuffled()))
