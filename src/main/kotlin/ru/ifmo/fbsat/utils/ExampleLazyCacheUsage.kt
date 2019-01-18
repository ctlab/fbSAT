package ru.ifmo.fbsat.utils

class ExampleLazyCacheUsage(state: Int?) {
    private val lazyCache = LazyCache()

    var state: Int? = state
        set(value) {
            println(">>> state = $value")
            field = value
        }

    val x: Int by lazyCache {
        (this.state?.let { it + 10 } ?: 0).also { println(">>> x = $it") }
    }

    fun invalidateCache() = lazyCache.invalidate()
}

fun main(args: Array<String>) {
    val example = ExampleLazyCacheUsage(42)
    println("example.x = ${example.x}")
    require(example.x == 52)

    example.state = -37
    println("example.x = ${example.x}")
    require(example.x == 52)

    example.invalidateCache()
    println("example.x = ${example.x}")
    require(example.x == -27)

    example.state = null
    example.invalidateCache()
    println("example.x = ${example.x}")
    require(example.x == 0)
}
