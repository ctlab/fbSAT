package ru.ifmo.fbsat.core.utils

import com.github.ajalt.mordant.TermColors

@Suppress("ClassName")
object log {
    private val t = TermColors()

    private fun debug(msg: String) {
        println(t.white("[.] $msg"))
    }

    fun debug(msg: () -> String) {
        if (Globals.IS_DEBUG) debug(msg())
    }

    fun info(msg: String) {
        println(t.brightBlue("[*] $msg"))
    }

    fun warn(msg: String) {
        println(t.brightMagenta("[!] $msg"))
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
