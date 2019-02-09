package ru.ifmo.fbsat.scenario

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestInstance


@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class CounterExampleTest {
    private val file = createTempFile()

    init {
        println("[*] Writing to tempfile: $file...")
        file.writeText(
            """
            Trace Description: LTL Counterexample
            Trace Type: Counterexample
              -> State: 1.1 <-
                REQ = TRUE
                C.CNF = FALSE
                pp1 = FALSE
                C.vcExtend = FALSE
              -- Loop starts here
              -> State: 1.2 <-
                REQ = TRUE
                C.CNF = TRUE
                pp1 = TRUE
                C.vcExtend = FALSE
              -> State: 1.3 <-
                REQ = TRUE
                C.CNF = FALSE
                pp1 = FALSE
                C.vcExtend = TRUE
              -> State: 1.4 <-
                REQ = TRUE
                C.CNF = TRUE
                pp1 = TRUE
                C.vcExtend = FALSE
            """.trimIndent()
        )
    }

    @Test
    fun `counter-example from file`() {
        val ces = CounterExample.fromFile(file)
        assertEquals(1, ces.size)

        val ce = ces.first()

        assertEquals(4, ce.states.size)
        assertEquals(2, ce.loopPosition)
        assertTrue(ce.states[1].isLoop)

        for ((i, state) in ce.states.withIndex())
            assertEquals("1.${i + 1}", state.name)

        for (state in ce.states)
            assertEquals(4, state.variables.size)
    }

    @Test
    fun `negative scenario from counter-example`() {
        val inputEvents = listOf("REQ")
        val outputEvents = listOf("CNF")
        val inputNames = listOf("pp1")
        val outputNames = listOf("vcExtend")

        val ce = CounterExample.fromFile(file).first()
        val ns = ce.toNegativeScenario(inputEvents, outputEvents, inputNames, outputNames)

        assertEquals(4, ns.elements.size)
        assertEquals(2, ns.loopPosition)
    }
}
