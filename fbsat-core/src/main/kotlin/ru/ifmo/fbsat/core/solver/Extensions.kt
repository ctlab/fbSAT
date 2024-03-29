package ru.ifmo.fbsat.core.solver

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import com.github.lipen.satlib.core.BoolVarArray
import com.github.lipen.satlib.core.Context
import com.github.lipen.satlib.core.DomainVar
import com.github.lipen.satlib.core.DomainVarArray
import com.github.lipen.satlib.core.IntVar
import com.github.lipen.satlib.core.IntVarArray
import com.github.lipen.satlib.core.Lit
import com.github.lipen.satlib.core.LitArray
import com.github.lipen.satlib.core.Model
import com.github.lipen.satlib.core.SequenceScopeLit
import com.github.lipen.satlib.core.convert
import com.github.lipen.satlib.core.newContext
import com.github.lipen.satlib.op.implyIff
import com.github.lipen.satlib.op.implyImply
import com.github.lipen.satlib.op.implyImplyAnd
import com.github.lipen.satlib.op.implyImplyImply
import com.github.lipen.satlib.op.implyImplyOr
import com.github.lipen.satlib.op.runWithTimeout
import com.github.lipen.satlib.solver.CadicalSolver
import com.github.lipen.satlib.solver.GlucoseSolver
import com.github.lipen.satlib.solver.MiniSatSolver
import com.github.lipen.satlib.solver.Solver
import com.github.lipen.satlib.solver.switchContext
import ru.ifmo.fbsat.core.utils.ModularContext
import ru.ifmo.fbsat.core.utils.exhaustive
import ru.ifmo.fbsat.core.utils.toList_

@Deprecated("Do not add empty clauses!")
fun Solver.clause() {
    @Suppress("deprecation")
    addClause()
}

fun Solver.clause(lit: Lit) {
    addClause(lit)
}

fun Solver.clause(lit1: Lit, lit2: Lit) {
    addClause(lit1, lit2)
}

fun Solver.clause(lit1: Lit, lit2: Lit, lit3: Lit) {
    addClause(lit1, lit2, lit3)
}

fun Solver.clause(literals: LitArray) {
    addClause(literals)
}

@JvmName("clauseVararg")
fun Solver.clause(vararg literals: Lit) {
    addClause(literals)
}

fun Solver.clause(literals: List<Lit>) {
    addClause(literals)
}

fun Solver.clause(literals: Sequence<Lit>) {
    clause(literals.toList())
}

fun Solver.clause(literals: SequenceScopeLit) {
    clause(sequence(literals))
}

fun Solver.clause(literals: Iterable<Lit>) {
    clause(literals.toList())
}

fun Solver.solveAndGetModel(): Model? =
    if (solve()) getModel() else null

fun Solver.solveAndGetModel(timeout: Long?): Model? =
    runWithTimeout(timeout) { solveAndGetModel() }

fun <T> Solver.runWithTimeout(timeMillis: Long?, block: Solver.() -> T): T =
    if (timeMillis == null) block()
    else runWithTimeout(timeMillis, block)

fun Solver.declareModularContext(M: Int): ModularContext =
    context("modularContext") {
        MultiArray.new(M) { newContext() }
    }

@Suppress("LocalVariableName")
inline fun Solver.forEachModularContext(block: (m: Int) -> Unit) {
    val M: Int = context["M"]
    val modularContext: ModularContext = context["modularContext"]
    for (m in 1..M) switchContext(modularContext[m]) {
        block(m)
    }
}

fun <T> Context.autoneg(name: String, isPositive: Boolean): T =
    get(
        if (isPositive) name
        else "neg" + name.replaceFirstChar { if (it.isLowerCase()) it.titlecase() else it.toString() }
    )

fun Context.convertLiteral(name: String, model: Model): Boolean =
    model[get(name)]

fun <T : Any> Context.convertDomainVar(name: String, model: Model): T? =
    get<DomainVar<T>>(name).convert(model)

fun Context.convertIntVar(name: String, model: Model): Int? =
    get<IntVar>(name).convert(model)

inline fun <reified T> Context.convertDomainVarArray(name: String, model: Model): MultiArray<T> =
    get<DomainVarArray<T>>(name).convert(model)

fun Context.convertIntVarArray(name: String, model: Model): IntMultiArray =
    get<IntVarArray>(name).convert(model)

fun Context.convertBoolVarArray(name: String, model: Model): BooleanMultiArray =
    get<BoolVarArray>(name).convert(model)

val BoolVarArray.literals: List<Lit> get() = values

val <T> DomainVarArray<T>.literals: List<Lit> get() = values.flatMap { it.literals }

/** [x1] => ([x2] <=> `XOR`([xs])) */
fun Solver.implyIffXor(x1: Lit, x2: Lit, xs: Iterable<Lit>) {
    val pool = xs.toList_()
    when (pool.size) {
        0 -> error("Iterable is empty")
        1 -> implyIff(x1, x2, pool[0])
        2 -> {
            val (x3, x4) = pool
            clause(-x1, -x2, -x3, -x4)
            clause(-x1, -x2, x3, x4)
            clause(-x1, x2, -x3, x4)
            clause(-x1, x2, x3, -x4)
        }
        else -> TODO()
    }.exhaustive
}

fun Solver.andImplyAnd(lhs: Iterable<Lit>, rhs: Iterable<Lit>) {
    val pool = lhs.toList_()
    val negPool = pool.map { -it }
    for (x in rhs) {
        clause(negPool + x)
    }
}

/** [x1] => ([x2] <=> `XOR`([xs])) */
fun Solver.implyIffXor(x1: Lit, x2: Lit, xs: Sequence<Lit>) {
    implyIffXor(x1, x2, xs.asIterable())
}

/** [x1] => ([x2] <=> `XOR`([xs])) */
fun Solver.implyIffXor(x1: Lit, x2: Lit, xs: SequenceScopeLit) {
    implyIffXor(x1, x2, sequence(xs))
}

/** [x1] => ([x2] <=> `XOR`([xs])) */
fun Solver.implyIffXor(x1: Lit, x2: Lit, vararg xs: Lit) {
    implyIffXor(x1, x2, xs.asIterable())
}

/** [x1] & [x2] => [x3] */
fun Solver.imply2(x1: Lit, x2: Lit, x3: Lit) {
    implyImply(x1, x2, x3)
}

/** [x1] & [x2] => `OR`([xs]) */
fun Solver.imply2Or(x1: Lit, x2: Lit, xs: Iterable<Lit>) {
    implyImplyOr(x1, x2, xs)
}

/** [x1] & [x2] => `OR`([xs]) */
fun Solver.imply2Or(x1: Lit, x2: Lit, xs: Sequence<Lit>) {
    imply2Or(x1, x2, xs.asIterable())
}

/** [x1] & [x2] => `OR`([xs]) */
fun Solver.imply2Or(x1: Lit, x2: Lit, xs: SequenceScopeLit) {
    imply2Or(x1, x2, sequence(xs))
}

/** [x1] & [x2] => `OR`([xs]) */
fun Solver.imply2Or(x1: Lit, x2: Lit, vararg xs: Lit) {
    imply2Or(x1, x2, xs.asIterable())
}

/** [x1] & [x2] => `AND`([xs]) */
fun Solver.imply2And(x1: Lit, x2: Lit, xs: Iterable<Lit>) {
    implyImplyAnd(x1, x2, xs)
}

/** [x1] & [x2] => `AND`([xs]) */
fun Solver.imply2And(x1: Lit, x2: Lit, xs: Sequence<Lit>) {
    imply2And(x1, x2, xs.asIterable())
}

/** [x1] & [x2] => `AND`([xs]) */
fun Solver.imply2And(x1: Lit, x2: Lit, xs: SequenceScopeLit) {
    imply2And(x1, x2, sequence(xs))
}

/** [x1] & [x2] => `AND`([xs]) */
fun Solver.imply2And(x1: Lit, x2: Lit, vararg xs: Lit) {
    imply2And(x1, x2, xs.asIterable())
}

/** [x1] & [x2] & [x3] => [x4] */
fun Solver.imply3(x1: Lit, x2: Lit, x3: Lit, x4: Lit) {
    implyImplyImply(x1, x2, x3, x4)
}

fun Solver.isSupportStats(): Boolean =
    this is MiniSatSolver || this is GlucoseSolver || this is CadicalSolver

fun Solver.numberOfPropagations(): Long = when (this) {
    is MiniSatSolver -> backend.numberOfPropagations
    is GlucoseSolver -> backend.numberOfPropagations
    is CadicalSolver -> backend.numberOfPropagations
    else -> error("$this does not support querying the number of propagations")
}

fun Solver.numberOfConflicts(): Long = when (this) {
    is MiniSatSolver -> backend.numberOfConflicts
    is GlucoseSolver -> backend.numberOfConflicts
    is CadicalSolver -> backend.numberOfConflicts
    else -> error("$this does not support querying the number of conflicts")
}

fun Solver.numberOfDecisions(): Long = when (this) {
    is MiniSatSolver -> backend.numberOfDecisions
    is GlucoseSolver -> backend.numberOfDecisions
    is CadicalSolver -> backend.numberOfDecisions
    else -> error("$this does not support querying the number of decisions")
}

fun Solver.numberOfRestarts(): Long = when (this) {
    is CadicalSolver -> backend.numberOfRestarts
    else -> error("$this does not support querying the number of restarts")
}
