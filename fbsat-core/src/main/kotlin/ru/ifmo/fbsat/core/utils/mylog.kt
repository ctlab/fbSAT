package ru.ifmo.fbsat.core.utils

import com.github.ajalt.mordant.TermColors

@Suppress("ClassName")
object mylog {
    private val t = TermColors()

    @Suppress("FunctionName")
    fun _debug(msg: String) {
        println(t.white("[.] $msg"))
    }

    inline fun debug(msg: () -> String) {
        if (Globals.IS_DEBUG) _debug(msg())
    }

    fun info(msg: String) {
        println(t.brightBlue("[*] $msg"))
    }

    fun warn(msg: String) {
        // println(t.brightMagenta("[!] $msg"))
        println(t.brightYellow("[!] $msg"))
    }

    fun error(msg: String) {
        println(t.brightRed("[!] $msg"))
    }

    fun success(msg: String) {
        println(t.brightGreen("[+] $msg"))
    }

    fun failure(msg: String) {
        println(t.brightRed("[-] $msg"))
    }

    fun br() {
        println(t.white("=".repeat(42)))
    }

    fun br(msg: String) {
        println(t.white("===== $msg"))
    }

    fun just(msg: String) {
        println(msg)
    }
}
