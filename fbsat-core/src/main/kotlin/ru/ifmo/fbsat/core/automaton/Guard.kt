package ru.ifmo.fbsat.core.automaton

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import kotlinx.serialization.KSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.Transient
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.modules.SerializersModule
import kotlinx.serialization.modules.polymorphic
import kotlinx.serialization.modules.subclass
import kotlinx.serialization.serializer
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.makeDnfString
import ru.ifmo.fbsat.core.utils.pow
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.toBooleanList
import kotlin.math.absoluteValue

private val logger = MyLogger {}

@Suppress("PublicApiImplicitType")
val guardModule = SerializersModule {
    polymorphic(Guard::class) {
        subclass(UnconditionalGuard::class)
        subclass(TruthTableGuard::class)
        subclass(ParseTreeGuard::class)
    }
}

interface Guard {
    val size: Int
    val truthTableString: String
    fun eval(inputValues: InputValues): Boolean
    fun toSimpleString(): String
    fun toGraphvizString(): String
    fun toFbtString(): String
    fun toSmvString(): String
}

@Serializable
@SerialName("UnconditionalGuard")
class UnconditionalGuard : Guard {
    override val size: Int
        get() {
            logger.warn("UnconditionalGuard has no meaningful size")
            return 0
        }

    @Transient
    override val truthTableString: String = "1"

    override fun eval(inputValues: InputValues): Boolean {
        return true
    }

    override fun toSimpleString(): String {
        return "1"
    }

    override fun toGraphvizString(): String {
        return "1"
    }

    override fun toFbtString(): String {
        return "1"
    }

    override fun toSmvString(): String {
        return "1"
    }

    override fun toString(): String {
        return "UnconditionalGuard()"
    }
}

@Deprecated("old unused code")
object TruthTableGuardSerializer : KSerializer<TruthTableGuard> {
    override val descriptor: SerialDescriptor = serializer<Map<InputValues, Boolean?>>().descriptor

    override fun serialize(encoder: Encoder, value: TruthTableGuard) {
        encoder.encodeSerializableValue(serializer(), value.truthTable)
    }

    override fun deserialize(decoder: Decoder): TruthTableGuard {
        val truthTable: Map<InputValues, Boolean?> = decoder.decodeSerializableValue(serializer())
        return TruthTableGuard(truthTable)
    }
}

@Serializable
@SerialName("TruthTableGuard")
class TruthTableGuard(
    val truthTable: Map<InputValues, Boolean?>,
) : Guard {
    override val truthTableString: String = truthTable.values.filterNotNull().toBinaryString()
    override val size: Int = 0

    override fun eval(inputValues: InputValues): Boolean {
        // return truthTable[uniqueInputs.indexOf(inputValues)] in "1x"
        // return truthTable.getValue(inputValues) ?: true
        return truthTable.getValue(inputValues)!!
    }

    override fun toSimpleString(): String {
        val s = truthTable.values.toBinaryString()
        return when {
            s.length <= 32 -> "[$s]"
            else -> "[${s.substring(0..15)}...]"
        }
    }

    override fun toGraphvizString(): String = toSimpleString()

    override fun toFbtString(): String = "TRUTH_TABLE"

    override fun toSmvString(): String = "TRUTH_TABLE"

    override fun toString(): String = "TruthTableGuard(truthTable = $truthTable)"
}

@Serializable
class Box<T>(val value: T)

object ParseTreeGuardSerializer : KSerializer<ParseTreeGuard> {
    override val descriptor: SerialDescriptor = serializer<Box<String>>().descriptor

    override fun serialize(encoder: Encoder, value: ParseTreeGuard) {
        val box = Box(value.toSimpleString())
        encoder.encodeSerializableValue(serializer(), box)
    }

    override fun deserialize(decoder: Decoder): ParseTreeGuard {
        TODO("Deserialization of ParseTreeGuard from string")
    }
}

@Serializable(with = ParseTreeGuardSerializer::class)
class ParseTreeGuard(
    nodeType: MultiArray<NodeType>,
    terminal: IntMultiArray,
    parent: IntMultiArray,
    childLeft: IntMultiArray,
    childRight: IntMultiArray,
    private val inputNames: List<String>? = null,
) : Guard {
    val nodes: List<Node>

    val root: Node
        get() = nodes.first()

    override val size: Int
        get() = root.size

    init {
        val (P) = nodeType.shape
        this.nodes = List(P) { p0 -> Node(nodeType[p0 + 1], terminal[p0 + 1]) }

        for (p in 1..P) {
            val node = nodes[p - 1]
            if (parent[p] != 0)
                node.parent = nodes[parent[p] - 1]
            if (childLeft[p] != 0)
                node.childLeft = nodes[childLeft[p] - 1]
            if (childRight[p] != 0)
                node.childRight = nodes[childRight[p] - 1]
        }
    }

    override val truthTableString: String
        get() = (0 until 2.pow(inputNames!!.size)).map { i ->
            eval(InputValues(i.toString(2).padStart(inputNames.size, '0').toBooleanList()))
        }.toBinaryString()

    inner class Node(
        val nodeType: NodeType,
        val terminalNumber: Int, // 1..X, or 0 if non-terminal
    ) {
        var parent: Node? = null
            internal set
        var childLeft: Node? = null
            internal set
        var childRight: Node? = null
            internal set

        val size: Int
            get() = 1 + (childLeft?.size ?: 0) + (childRight?.size ?: 0)

        init {
            if (nodeType == NodeType.TERMINAL)
                require(terminalNumber > 0) { "Terminal number of terminal node must be > 0" }
            else
                require(terminalNumber == 0) { "Terminal number of non-terminal node must be = 0" }
        }

        fun eval(inputValues: InputValues): Boolean {
            return when (nodeType) {
                NodeType.TERMINAL -> inputValues[terminalNumber - 1]
                NodeType.AND -> childLeft!!.eval(inputValues) && childRight!!.eval(inputValues)
                NodeType.OR -> childLeft!!.eval(inputValues) || childRight!!.eval(inputValues)
                NodeType.NOT -> !childLeft!!.eval(inputValues)
                NodeType.NONE -> error("Can't eval none-typed node")
            }
        }

        fun toSimpleString(): String {
            return when (nodeType) {
                NodeType.TERMINAL -> this@ParseTreeGuard.inputNames
                    ?.let { it[terminalNumber - 1] }
                    ?: "x$terminalNumber"
                NodeType.AND -> {
                    var left = childLeft!!.toSimpleString()
                    var right = childRight!!.toSimpleString()
                    if (childLeft!!.nodeType == NodeType.OR)
                        left = "($left)"
                    if (childRight!!.nodeType == NodeType.OR)
                        right = "($right)"
                    "$left & $right"
                }
                NodeType.OR -> {
                    var left = childLeft!!.toSimpleString()
                    var right = childRight!!.toSimpleString()
                    if (childLeft!!.nodeType == NodeType.AND)
                        left = "($left)"
                    if (childRight!!.nodeType == NodeType.AND)
                        right = "($right)"
                    "$left | $right"
                }
                NodeType.NOT -> {
                    var left = childLeft!!.toSimpleString()
                    if (childLeft!!.nodeType !in setOf(
                            NodeType.TERMINAL,
                            NodeType.NOT
                        )
                    )
                        left = "($left)"
                    "~$left"
                }
                NodeType.NONE -> error("Why are you trying to display none-typed node?")
            }
        }

        fun toGraphvizString(): String {
            return toSimpleString()
        }

        fun toFbtString(): String {
            return when (nodeType) {
                NodeType.TERMINAL -> this.toSimpleString()
                NodeType.AND -> {
                    var left = childLeft!!.toFbtString()
                    var right = childRight!!.toFbtString()
                    if (childLeft!!.nodeType == NodeType.OR)
                        left = "($left)"
                    if (childRight!!.nodeType == NodeType.OR)
                        right = "($right)"
                    "$left AND $right"
                }
                NodeType.OR -> {
                    var left = childLeft!!.toFbtString()
                    var right = childRight!!.toFbtString()
                    // if (childLeft!!.nodeType == NodeType.AND)
                    //     left = "($left)"
                    // if (childRight!!.nodeType == NodeType.AND)
                    //     right = "($right)"
                    "$left | $right"
                }
                NodeType.NOT -> {
                    val left = childLeft!!.toFbtString()
                    if (childLeft!!.nodeType in setOf(
                            NodeType.TERMINAL,
                            NodeType.NOT
                        )
                    )
                        "NOT $left"
                    else
                        "NOT($left)"
                }
                NodeType.NONE -> error("Why are you trying to display none-typed node?")
            }
        }

        fun toSmvString(): String {
            return when (nodeType) {
                NodeType.TERMINAL -> this.toSimpleString()
                NodeType.AND -> {
                    var left = childLeft!!.toSmvString()
                    var right = childRight!!.toSmvString()
                    if (childLeft!!.nodeType == NodeType.OR)
                        left = "($left)"
                    if (childRight!!.nodeType == NodeType.OR)
                        right = "($right)"
                    "$left & $right"
                }
                NodeType.OR -> {
                    var left = childLeft!!.toSmvString()
                    var right = childRight!!.toSmvString()
                    if (childLeft!!.nodeType == NodeType.AND)
                        left = "($left)"
                    if (childRight!!.nodeType == NodeType.AND)
                        right = "($right)"
                    "$left | $right"
                }
                NodeType.NOT -> {
                    var left = childLeft!!.toSmvString()
                    if (childLeft!!.nodeType !in setOf(
                            NodeType.TERMINAL,
                            NodeType.NOT
                        )
                    )
                        left = "($left)"
                    "!$left"
                }
                NodeType.NONE -> error("Why are you trying to display none-typed node?")
            }
        }

        override fun toString(): String {
            return "Node(nodeType=$nodeType, terminalNumber=$terminalNumber, parent=$parent, childLeft=$childLeft, childRight=$childRight)"
        }
    }

    override fun eval(inputValues: InputValues): Boolean {
        return root.eval(inputValues)
    }

    override fun toSimpleString(): String {
        return root.toSimpleString()
    }

    override fun toGraphvizString(): String {
        return root.toGraphvizString()
    }

    override fun toFbtString(): String {
        return root.toFbtString()
    }

    override fun toSmvString(): String {
        return root.toSmvString()
    }

    override fun toString(): String {
        return "ParseTreeGuard(size=${nodes.size})"
    }
}

class StringGuard(val expr: String, val inputNames: List<String>) : Guard {
    private val literals = expr.splitToSequence('&').map(String::trim).toList()

    override val size: Int =
        2 * literals.size - 1 + literals.count { it.startsWith("!") || it.startsWith("~") }

    override val truthTableString: String
        get() = TODO()

    override fun eval(inputValues: InputValues): Boolean {
        return literals.all {
            if (it.startsWith("!") || it.startsWith("~"))
                !inputValues[inputNames.indexOf(it.substring(1))]
            else
                inputValues[inputNames.indexOf(it)]
        }
    }

    override fun toSimpleString(): String {
        return expr
    }

    override fun toGraphvizString(): String {
        return expr
    }

    override fun toFbtString(): String {
        return expr
            .replace("&", " AND ")
            .replace("~", "NOT ")
            .replace("!", "NOT ")
    }

    override fun toSmvString(): String {
        return expr
    }

    override fun toString(): String {
        return "StringGuard(expr = $expr)"
    }
}

class DnfGuard(
    val dnf: List<List<String>>,
    val inputNames: List<String>,
) : Guard {
    // Note: `_dnf` is one-based and signed
    private val _dnf: List<List<Int>> =
        dnf.map { term ->
            term.map { s ->
                if (s.startsWith('!') || s.startsWith('~')) {
                    -(inputNames.indexOf(s.drop(1)) + 1)
                } else {
                    inputNames.indexOf(s) + 1
                }
            }
        }

    override val size: Int = dnf.sumBy { it.size }

    override val truthTableString: String
        get() = TODO()

    override fun eval(inputValues: InputValues): Boolean =
        if (_dnf.isEmpty()) {
            check(toSimpleString() == "0")
            false
        } else {
            _dnf.any { term ->
                term.all { Lit ->
                    (Lit < 0) xor inputValues.values[Lit.absoluteValue - 1]
                }
            }
        }

    override fun toSimpleString(): String {
        return makeDnfString(dnf, conjunction = "&", disjunction = " | ")
    }

    override fun toGraphvizString(): String {
        return toSimpleString()
    }

    override fun toFbtString(): String {
        return toSimpleString()
            .replace("|", " OR ")
            .replace("&", " AND ")
            .replace("~", "NOT ")
            .replace("!", "NOT ")
    }

    override fun toSmvString(): String {
        return toSimpleString()
    }

    override fun toString(): String {
        return "DnfGuard(dnf = $dnf)"
    }
}

enum class NodeType(val value: Int) {
    TERMINAL(1),
    AND(2),
    OR(3),
    NOT(4),
    NONE(5);

    companion object {
        private val lookup: Map<Int, NodeType> = values().associateBy(NodeType::value)

        fun from(value: Int): NodeType = lookup.getValue(value)
    }
}
