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
import com.github.lipen.satlib.solver.Solver
import com.github.lipen.satlib.solver.switchContext
import ru.ifmo.fbsat.core.utils.ModularContext

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
        else "neg" + name.capitalize()
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
