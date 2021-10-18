package ru.ifmo.fbsat.core.scenario.single.negative

import org.amshove.kluent.shouldBeEqualTo
import org.amshove.kluent.shouldBeTrue
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
        val ces = Counterexample.from(file)
        ces.size shouldBeEqualTo 1
        val ce = ces.single()

        ce.states.size shouldBeEqualTo 4
        ce.loopPosition shouldBeEqualTo 2

        for (state in ce.states)
            state.data.size shouldBeEqualTo 4
    }

    @Test
    fun `negative scenario from file`() {
        val nss = NegativeScenario.from(
            file,
            inputEvents,
            outputEvents,
            inputNames,
            outputNames
        )
        nss.size shouldBeEqualTo 1
        val ns = nss.single()

        ns.elements.size shouldBeEqualTo 3
        ns.loopPosition shouldBeEqualTo 1
    }

    @Test
    fun `negative scenario tree from file`() {
        val nst =
            OldNegativeScenarioTree.fromFile(
                file, inputEvents, outputEvents, inputNames, outputNames
            )
        nst.size shouldBeEqualTo 4
    }
}
