package ru.ifmo.fbsat.core.scenario.single.negative

import org.amshove.kluent.shouldBeTrue
import org.amshove.kluent.shouldEqual
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestInstance
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.negative.Counterexample
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenario
import ru.ifmo.fbsat.core.scenario.negative.OldNegativeScenarioTree

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class NegativeTest {
    private val file = createTempFile()
    private val inputEvents = listOf("REQ").map(::InputEvent)
    private val outputEvents = listOf("CNF").map(::OutputEvent)
    private val inputNames = listOf("pp1")
    private val outputNames = listOf("vcExtend")

    init {
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
        val ces = Counterexample.fromFile(file)
        ces.size shouldEqual 1
        val ce = ces.single()

        ce.states.size shouldEqual 4
        ce.loopPosition shouldEqual 2
        ce.states[1].isLoop.shouldBeTrue()

        for ((i, state) in ce.states.withIndex())
            state.name shouldEqual "1.${i + 1}"

        for (state in ce.states)
            state.variables.size shouldEqual 4
    }

    @Test
    fun `negative scenario from file`() {
        val nss = NegativeScenario.fromFile(
            file,
            inputEvents,
            outputEvents,
            inputNames,
            outputNames
        )
        nss.size shouldEqual 1
        val ns = nss.single()

        ns.elements.size shouldEqual 3
        ns.loopPosition shouldEqual 1
    }

    @Test
    fun `negative scenario tree from file`() {
        val nst =
            OldNegativeScenarioTree.fromFile(
                file, inputEvents, outputEvents, inputNames, outputNames
            )
        nst.size shouldEqual 4
    }
}
