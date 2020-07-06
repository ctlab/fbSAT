package ru.ifmo.fbsat.core.automaton

import com.github.lipen.multiarray.MultiArray

class DistributedAutomaton(
    val modules : MultiArray<Automaton>
){
    val M: Int = modules.shape[0]

    init {
        check(M > 0)
    }
}
