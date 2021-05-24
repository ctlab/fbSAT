package ru.ifmo.fbsat.core.constraints

import com.github.lipen.satlib.core.Lit
import com.github.lipen.satlib.solver.Solver
import kotlin.math.min
import kotlin.math.max

interface IncrementalTotalizer {
    val solver: Solver
    fun expand(newBorder: Int)
    val literals: List<Lit>
    val curSize: Int
    val maxSize: Int
}

class LeafTotalizer(override val solver: Solver, val lit: Lit) : IncrementalTotalizer {
    override fun expand(newBorder: Int) {}
    override val literals
        get() = listOf(lit)
    override val curSize
        get() = 1
    override val maxSize
        get() = 1
}

class BinaryIncrementalTotalizer(override val solver: Solver, border: Int, totalizers: List<IncrementalTotalizer>) : IncrementalTotalizer {
    init {
        check(totalizers.size > 1) { "Wrong cardinalities elements number ${totalizers.size}" }
    }

    private val left = if (totalizers.size / 2 == 1) totalizers[0]
    else BinaryIncrementalTotalizer(solver, border, totalizers.subList(0, totalizers.size / 2))

    private val right = if (totalizers.size - totalizers.size / 2 == 1) totalizers[1]
    else BinaryIncrementalTotalizer(solver, border, totalizers.subList(totalizers.size / 2, totalizers.size))

    override val maxSize = left.maxSize + right.maxSize

    override var curSize = 0
        private set

    private val _literals = MutableList(curSize) { solver.newLiteral() }
    override val literals: List<Lit>
        get() = _literals

    init {
        expand(border)
    }

    override fun expand(newBorder: Int) {
        if (newBorder <= curSize || curSize == maxSize) return

        left.expand(newBorder)
        right.expand(newBorder)
        val newCurSize = min(newBorder, maxSize)
        for (i in curSize until newCurSize) {
            _literals += solver.newLiteral()
        }
        for (i in (curSize + 1)..newCurSize) {
            val from = max(0, i - right.curSize)
            val to = min(i, left.curSize)
            require(from <= to) { "Strange borders: [$from, $to]" }
            for (j in from..to) {
                solver.addClause(when {
                    j == 0 -> listOf(-right.literals[i - 1], literals[i - 1])
                    i - j == 0 -> listOf(-left.literals[i - 1], literals[i - 1])
                    else -> listOf(-left.literals[j - 1], -right.literals[i - j - 1], literals[i - 1])
                })
            }
        }
        curSize = newCurSize
    }
}

class IncrementalCardinality(val solver: Solver, totalizer: IncrementalTotalizer) {
    var totalizer = totalizer
        private set

    var border: Int? = null
        private set

    constructor(solver: Solver, literals: List<Lit>, border: Int? = null) :
        this(solver, if (literals.size > 1)
            BinaryIncrementalTotalizer(solver, border ?: 0, literals.map { LeafTotalizer(solver, it) })
        else LeafTotalizer(solver, literals[0]))

    private var callback: (() -> List<Lit>)? = null

    fun merge(otherTotalizer: IncrementalTotalizer) {
        totalizer = BinaryIncrementalTotalizer(solver, border ?: 0, listOf(totalizer, otherTotalizer))
    }

    fun merge(literals: List<Lit>) {
        require(literals.isNotEmpty()) { "Merge empty literals list" }
        val otherTotalizer =
            if (literals.size == 1) LeafTotalizer(solver, literals[0])
            else BinaryIncrementalTotalizer(solver, border ?: 0, literals.map { LeafTotalizer(solver, it) })
        merge(otherTotalizer)
    }

    fun assumeUpperBoundLessThan(newBorder: Int) {
        require(newBorder > 0) { "assumeUpperBoundLessThan $newBorder" }
        if (newBorder > totalizer.maxSize) {
            println("Skipped because $newBorder ${totalizer.maxSize}")
            return
        }
        if (border == null || newBorder > border!!) {
            totalizer.expand(newBorder)
        }
        unassume()
        callback = { listOf(-totalizer.literals[newBorder - 1]) }.also {
            solver.assumptionsObservable.register(it)
        }
        border = newBorder
    }

    fun assumeUpperBoundLessOrEqual(newBorder: Int) {
        if (newBorder >= totalizer.maxSize) return
        assumeUpperBoundLessThan(newBorder + 1)
    }

    fun unassume() {
        callback?.let { solver.assumptionsObservable.unregister(it) }
        callback = null
    }
}
