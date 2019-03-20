package ru.ifmo.fbsat.task.extended

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.utils.toBooleanArray

internal fun Automaton.checkNegativeAssignment(negAssignment: NegativeAssignment, scenarioTree: ScenarioTree) {
    val negTree = negAssignment.negativeScenarioTree
    val C = negAssignment.C
    val K = negAssignment.K
    val P = negAssignment.P
    val someOutputValues = negTree.uniqueOutputs.first()
    var isOk = true

    for (input in negTree.uniqueInputs) {
        val u = negTree.uniqueInputs.indexOf(input) + 1

        for (i in 1..C) {
            val (newState, outputEvent, newValues) = go(getState(i), "REQ", input, someOutputValues)
            val j = if (outputEvent != null) newState.id else 0

            if (negAssignment.actualTransition[i, 1, u] == j) {
                // println("[+] go(i = $i, input = $input) -> $j")
            } else {
                println("[-] go(i = $i, input = $input) -> $j  !=  actualTransition[i,REQ,u]=${negAssignment.actualTransition[i, 1, u]}")
                println(" >  u = $u, input = $input, u2input = ${negTree.uniqueInputs.indexOf(input) + 1}")
                for (k in 1..K) {
                    val guard = getState(i).transitions.getOrNull(k - 1)?.guard
                    println(" >  guard ${guard?.toSimpleString()} evaluates on input=$input to: ${guard?.eval(input.toBooleanArray())}")
                    println(" >  nodeValue[i,k,p,u = ($i,$k,${1..P},$u)] = ${(1..P).map { p -> negAssignment.nodeValue[i, k, p, u] }}")
                    println(" >  nodeType[i,k,p = ($i,$k,${1..P})] = ${(1..P).map { p -> negAssignment.nodeType[i, k, p] }}")
                    println(" >  terminal[i = $i, k = $k, 1..P] = ${(1..P).map { p ->
                        val x = negAssignment.terminal[i, k, p]
                        if (x != 0)
                            "${negTree.inputNames[x - 1]}(${negTree.uniqueInputs[u - 1][x - 1]})"
                        else
                            "-"
                    }}")
                }
                isOk = false
            }
        }
    }

    if (isOk)
        println("[+] checkNegativeAssignment: OK")
    else
        println("[-] checkNegativeAssignment: FAILED")
}
