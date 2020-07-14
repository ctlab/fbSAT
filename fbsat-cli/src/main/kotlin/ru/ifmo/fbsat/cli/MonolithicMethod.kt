package ru.ifmo.fbsat.cli

enum class MonolithicMethod(val s: String) {
    Basic("basic"),
    BasicMin("basic-min"),
    Extended("extended"),
    ExtendedMin("extended-min"),
    ExtendedMinUB("extended-min-ub"),
    ExtForest("extforest"),
    ExtForestMin("extforest-min"),
    Complete("complete"),
    CompleteMin("complete-min"),
    Cegis("cegis"),
    CegisMin("cegis-min"),
}
