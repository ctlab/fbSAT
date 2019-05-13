package ru.ifmo.fbsat.core.automaton

inline class InputEvent(val name: String) {
    companion object {
        // @JvmStatic
        // val empty: InputEvent = InputEvent("ε")

        @JvmStatic
        fun of(name: String?): InputEvent? = name?.let { InputEvent(it) }
    }
}

inline class OutputEvent(val name: String) {
    companion object {
        // @JvmStatic
        // val empty: OutputEvent = OutputEvent("ε")

        @JvmStatic
        fun of(name: String?): OutputEvent? = name?.let { OutputEvent(it) }
    }
}

inline class InputValues(val values: List<Boolean>) {
    // constructor(s: String) : this(s.toBooleanList())
    // constructor(s: BooleanArray) : this(s.toList())

    operator fun get(index: Int): Boolean {
        return values[index]
    }

    companion object {
        fun empty(): InputValues = InputValues(emptyList())
        fun zeros(size: Int): InputValues = InputValues(List(size) { false })
    }
}

inline class OutputValues(val values: List<Boolean>) {
    // constructor(s: String) : this(s.toBooleanList())
    // constructor(s: BooleanArray) : this(s.toList())

    operator fun get(index: Int): Boolean {
        return values[index]
    }

    companion object {
        fun empty(): OutputValues = OutputValues(emptyList())
        fun zeros(size: Int): OutputValues = OutputValues(List(size) { false })
    }
}

// inline class InputVariable(val name: String)

// inline class OutputVariable(val name: String)
