package ru.ifmo.fbsat.core.task.distributed.basic

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import com.github.lipen.satlib.solver.Solver
import com.github.lipen.satlib.utils.BoolVarArray
import com.github.lipen.satlib.utils.IntVarArray
import com.github.lipen.satlib.utils.newBoolVarArray
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.solver.declareModularContext
import ru.ifmo.fbsat.core.solver.forEachModularContext
import ru.ifmo.fbsat.core.task.single.basic.declareBasicVariables
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.mylog

@Suppress("LocalVariableName")
fun Solver.declareDistributedBasicVariables(
    M: Int,
    compoundScenarioTree: PositiveCompoundScenarioTree,
    modularScenarioTree: MultiArray<PositiveScenarioTree> = compoundScenarioTree.modular,
    modularC: MultiArray<Int>,
    modularK: MultiArray<Int>,
    modularV: MultiArray<Int> = compoundScenarioTree.modular.map { it.size },
    modularE: MultiArray<Int> = compoundScenarioTree.modular.map { it.inputEvents.size },
    modularO: MultiArray<Int> = compoundScenarioTree.modular.map { it.outputEvents.size },
    modularX: MultiArray<Int> = compoundScenarioTree.modular.map { it.inputNames.size },
    modularZ: MultiArray<Int> = compoundScenarioTree.modular.map { it.outputNames.size },
    modularU: MultiArray<Int> = compoundScenarioTree.modular.map { it.uniqueInputs.size },
) {
    context["M"] = M
    context["compoundScenarioTree"] = compoundScenarioTree
    context["compoundTree"] = compoundScenarioTree
    context["modularScenarioTree"] = modularScenarioTree
    context["modularTree"] = modularScenarioTree
    context["modularC"] = modularC
    context["modularK"] = modularK
    context["modularV"] = modularV
    context["modularE"] = modularE
    context["modularO"] = modularO
    context["modularX"] = modularX
    context["modularZ"] = modularZ
    context["modularU"] = modularU

    val falseVar = newLiteral()
    addClause(-falseVar)

    /* Modular */
    declareModularContext(M)
    forEachModularContext { m ->
        declareBasicVariables(
            positiveScenarioTree = modularScenarioTree[m],
            C = modularC[m],
            K = modularK[m],
            V = modularV[m],
            E = modularE[m],
            O = modularO[m],
            X = modularX[m],
            Z = modularZ[m],
            U = modularU[m]
        )
        val stateUsed = context("stateUsed") {
            newBoolVarArray(modularC.values.maxOrNull()!!) { (c) ->
                if (c <= modularC[m]) newLiteral()
                else falseVar
            }
        }
    }

    /* Cardinality */
    // require(modularC.values.all { it == modularC[1] }) { "All C must be equal" }
    val cardinalityC = context("cardinalityC") {
        declareCardinality {
            forEachModularContext {
                // check(C == modularC[1])
                val C: Int = context["C"]
                val stateUsed: BoolVarArray = context["stateUsed"]
                for (c in 1..C)
                    yield(stateUsed[c])
            }
        }
    }
    val cardinalityT = context("cardinalityT") {
        declareCardinality {
            forEachModularContext {
                val C: Int = context["C"]
                val K: Int = context["K"]
                val transitionDestination: IntVarArray = context["transitionDestination"]
                for (c in 1..C)
                    for (k in 1..K)
                        yield(transitionDestination[c, k] neq 0)
            }
        }
    }

    if (Globals.IS_DUMP_VARS_IN_CNF) {
        mylog.warn("Dumping of DistributedBasicVariables to CNF is not implemented yet")
    }
}
