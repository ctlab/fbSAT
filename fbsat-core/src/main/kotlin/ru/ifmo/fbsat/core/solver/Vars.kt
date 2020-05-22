package ru.ifmo.fbsat.core.solver

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray

// inline class Literal(val number: Int) {
//     operator fun unaryMinus(): Literal = Literal(-number)
// }

typealias Literal = Int

infix fun Literal.sign(b: Boolean): Literal = if (b) this else -this

enum class VarEncoding {
    ONEHOT,
    ONEHOT_BINARY,
}

interface DomainVar<T> {
    val domain: Set<T>
    val literals: Collection<Literal> // Note: proper order is *not* guaranteed
    val bits: List<Literal> // Note: bits[0] is LSB

    infix fun eq(value: T): Literal
    infix fun neq(value: T): Literal
    fun bit(index: Int): Literal

    fun convert(raw: RawAssignment): T?

    companion object Factory {
        @JvmStatic
        fun <T> create(
            domain: Iterable<T>,
            solver: Solver,
            encoding: VarEncoding,
            init: (T) -> Literal
        ): DomainVar<T> = when (encoding) {
            VarEncoding.ONEHOT -> OneHotDomainVar(domain, solver, init)
            VarEncoding.ONEHOT_BINARY -> OneHotBinaryDomainVar(domain, solver, init)
        }
    }
}

private abstract class AbstractDomainVar<T>(
    private val storage: Map<T, Literal>
) : DomainVar<T> {
    final override val domain: Set<T> = storage.keys
    final override val literals: Collection<Literal> = storage.values

    constructor(domain: Iterable<T>, init: (T) -> Literal) :
        this(domain.associateWith { init(it) })

    final override fun eq(value: T): Literal = storage.getValue(value)
    final override fun neq(value: T): Literal = -eq(value)
    final override fun bit(index: Int): Literal = bits[index]

    final override fun convert(raw: RawAssignment): T? =
        storage.entries.firstOrNull { raw[it.value] }?.key
}

private class OneHotDomainVar<T>(
    domain: Iterable<T>,
    solver: Solver,
    init: (T) -> Literal
) : AbstractDomainVar<T>(domain, init) {
    // Note: LSP is violated intentionally
    override val bits: List<Literal>
        get() = error("OneHotVar does not have associated bitwise representation")

    init {
        @Suppress("LeakingThis")
        solver.encodeOneHot(this)
    }

    override fun toString(): String {
        return "OneHotDomainVar(domain = $domain)"
    }
}

private class OneHotBinaryDomainVar<T>(
    domain: Iterable<T>,
    solver: Solver,
    init: (T) -> Literal
) : AbstractDomainVar<T>(domain, init) {
    @Suppress("LeakingThis")
    override val bits: List<Literal> = solver.encodeOneHotBinary(this)

    override fun toString(): String {
        return "OneHotBinaryDomainVar(domain = $domain)"
    }
}

class BoolVarArray private constructor(
    private val backend: IntMultiArray
) : MultiArray<Literal> by backend {
    companion object Factory {
        @JvmStatic
        fun create(
            shape: IntArray,
            init: (IntArray) -> Literal
        ): BoolVarArray = BoolVarArray(IntMultiArray.create(shape, init))

        @JvmStatic
        @JvmName("createVararg")
        fun create(
            vararg shape: Int,
            init: (IntArray) -> Literal
        ): BoolVarArray = create(shape, init)
    }
}

typealias IntVar = DomainVar<Int>

typealias IntVarArray = MultiArray<IntVar>

typealias DomainVarArray<T> = MultiArray<DomainVar<T>>

val BoolVarArray.literals: List<Literal> get() = values

val <T> DomainVarArray<T>.literals: List<Literal> get() = values.flatMap { it.literals }
