package ru.ifmo.fbsat.utils

class Solver {
    private val _clauses: MutableList<IntArray> = mutableListOf()
    val clauses: List<IntArray> = _clauses

    fun addClause(vararg literals: Int) {
        println("[.] Adding clause ${literals.toList()}")
        _clauses.add(literals)
    }
}

fun Solver.declare(block: Declaration.() -> Unit) = Declaration(this).apply(block)

class Declaration(val solver: Solver) {
    fun clause(literals: Sequence<Int>): Clause = Clause(literals)
    fun clause(vararg literals: Int) = Clause(literals.asSequence())

    fun imply(lhs: Int, vararg rhs: Int) {
        solver.addClause(-lhs, *rhs)
    }

    fun imply(lhs: Int, rhs: Expr) = when (rhs) {
        is Or -> solver.addClause(-lhs, *rhs.literals)
        is And -> rhs.literals.forEach { solver.addClause(-lhs, it) }
        else -> throw IllegalStateException()
    }

    fun or(vararg literals: Int) = Or(literals)
    fun and(vararg literals: Int) = And(literals)

    interface Expr
    inner class Or(val literals: IntArray) : Expr
    inner class And(val literals: IntArray) : Expr
    inner class Imply(val lhs: Int, val rhs: Expr) : Expr
}

class Clause(val literals: Sequence<Int>)

fun main(args: Array<String>) {
    println("[@] Hello, world!")

    val solver = Solver()

    solver.declare {
//        clause(1, 2, 3)
//        clause(42)
//        imply(4, 5)
//        imply(10, or(3, 4, 5))
//        imply(11, and(7, 4))
        clause(1,2,3)
        imply(5, and(7,8,9))
    }

    println("[.] Total clauses in solver: ${solver.clauses.size}")

    println("[+] All done!")
}
