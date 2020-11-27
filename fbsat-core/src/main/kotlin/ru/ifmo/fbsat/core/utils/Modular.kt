package ru.ifmo.fbsat.core.utils

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.satlib.utils.Context
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.positive.OldPositiveScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree

internal typealias ModularContext = MultiArray<Context>
internal typealias ModularOldScenarioTree = MultiArray<OldPositiveScenarioTree>
internal typealias ModularScenarioTree = MultiArray<PositiveScenarioTree>
internal typealias ModularAutomaton = MultiArray<Automaton>
internal typealias ModularState = MultiArray<Automaton.State>
internal typealias ModularEvalState = MultiArray<Automaton.EvalState>
internal typealias ModularEvalResult = MultiArray<Automaton.EvalResult>
internal typealias ModularEvalResult_adhoc = ImmutableMultiArray<Automaton.EvalResult>
internal typealias ModularInputAction = MultiArray<InputAction>
internal typealias ModularInputEvent = MultiArray<InputEvent>
internal typealias ModularInputValues = MultiArray<InputValues>
internal typealias ModularOutputAction = MultiArray<OutputAction>
internal typealias ModularOutputEvent = MultiArray<OutputEvent>
internal typealias ModularOutputValues = MultiArray<OutputValues>
