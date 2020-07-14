package ru.ifmo.fbsat.cli

enum class ModularMethod(val s: String) {
    ParallelBasic("parallel-basic"),
    ParallelBasicMin("parallel-basic-min"),
    ParallelExtended("parallel-extended"),
    ParallelExtendedMin("parallel-extended-min"),
    ParallelExtendedMinUB("parallel-extended-min-ub"),
    ConsecutiveBasic("consecutive-basic"),
    ConsecutiveBasicMin("consecutive-basic-min"),
    ConsecutiveExtended("consecutive-extended"),
    ConsecutiveExtendedMin("consecutive-extended-min"),
    ConsecutiveExtendedMinUB("consecutive-extended-min-ub"),
    ArbitraryBasic("arbitrary-basic"),
    ArbitraryBasicMin("arbitrary-basic-min"),
}
