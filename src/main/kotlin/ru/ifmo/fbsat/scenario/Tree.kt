package ru.ifmo.fbsat.scenario

class Tree<T> {
    private val _nodes: MutableList<Node> = mutableListOf()
    val nodes: List<Node> = _nodes
    val root: Node?
        get() = nodes.firstOrNull()
    val size
        get() = nodes.size

//    operator fun get(i: Int): Node {
//        return nodes[i]
//    }
//    operator fun set(i: Int, n: Node) {
//        _nodes[i] = n
//    }

    inner class Node(val data: T, val parent: Node?) {
        private val _children: MutableList<Node> = mutableListOf()
        val children: List<Node> = _children
        val id: Int = size

        init {
            require(id == size)  // TODO: remove
            _nodes.add(this)
            require(id == size - 1)  // TODO: remove
            parent?._children?.add(this)
        }

        override fun toString(): String {
            return "Node(id=$id, parent=${parent?.id}, children=${children.map { it.id }}, data=$data)"
        }
    }
}
