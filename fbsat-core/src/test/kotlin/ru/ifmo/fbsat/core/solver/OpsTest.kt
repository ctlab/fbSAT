package ru.ifmo.fbsat.core.solver

import org.amshove.kluent.shouldEqual
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestInstance
import kotlin.math.absoluteValue

@TestInstance(TestInstance.Lifecycle.PER_METHOD)
class OpsTest {
    private val clauses: MutableList<List<Literal>> = mutableListOf()

    private val solver: Solver =
        Solver.mock(
            _clause = { literals: List<Literal> ->
                clauses.add(literals)
            }
        )

    @Test
    fun clause() {
        solver.clause(1, 2)
        solver.clause(3, 4)
        clauses shouldEqual listOf(
            listOf(1, 2),
            listOf(3, 4)
        )
    }

    @Test
    fun imply() {
        solver.imply(1, 2)
        solver.imply(3, 4)
        clauses shouldEqual listOf(
            listOf(-1, 2),
            listOf(-3, 4)
        )
    }

    @Test
    fun implyAnd() {
        solver.implyAnd(10, 1, 2, 3)
        clauses shouldEqual listOf(
            listOf(-10, 1),
            listOf(-10, 2),
            listOf(-10, 3)
        )
    }

    @Test
    fun implyOr() {
        solver.implyOr(10, 1, 2, 3)
        clauses shouldEqual listOf(listOf(-10, 1, 2, 3))
    }

    @Test
    fun implyImplyIffAnd() {
        solver.implyImplyIffAnd(1, 2, 5, 10, 20, 30)
        clauses.map { clause -> clause.sortedBy { it.absoluteValue } } shouldEqual listOf(
            listOf(-1, -2, -5, 10),
            listOf(-1, -2, -5, 20),
            listOf(-1, -2, -5, 30),
            listOf(-1, -2, 5, -10, -20, -30)
        )
    }

    @Test
    fun implyImplyIffOr() {
        solver.implyImplyIffOr(1, 2, 5, 10, 20, 30)
        clauses.map { clause -> clause.sortedBy { it.absoluteValue } } shouldEqual listOf(
            listOf(-1, -2, 5, -10),
            listOf(-1, -2, 5, -20),
            listOf(-1, -2, 5, -30),
            listOf(-1, -2, -5, 10, 20, 30)
        )
    }

    @Test
    fun implyIffAnd() {
        solver.implyIffAnd(1, 5, 10, 20, 30)
        clauses.map { clause -> clause.sortedBy { it.absoluteValue } } shouldEqual listOf(
            listOf(-1, -5, 10),
            listOf(-1, -5, 20),
            listOf(-1, -5, 30),
            listOf(-1, 5, -10, -20, -30)
        )
    }

    @Test
    fun implyIffOr() {
        solver.implyIffOr(1, 5, 10, 20, 30)
        clauses.map { clause -> clause.sortedBy { it.absoluteValue } } shouldEqual listOf(
            listOf(-1, 5, -10),
            listOf(-1, 5, -20),
            listOf(-1, 5, -30),
            listOf(-1, -5, 10, 20, 30)
        )
    }
}
