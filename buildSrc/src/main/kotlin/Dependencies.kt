@file:Suppress("PublicApiImplicitType")

object Versions {
    const val kotlin = "1.3.41"
    const val jgitver = "0.8.0"
    const val ktlint = "8.2.0"
    const val clikt = "2.1.0"
    const val junit = "5.5.1"
    const val kluent = "1.53"
    const val multiarray = "0.3.0"
    const val shadow = "5.1.0"
    const val okio = "2.2.2"
    const val gradle_versions = "0.21.0"
    const val mordant = "1.2.1"
    const val kotlin_xml_builder = "1.5.1"
    const val lazycache = "0.3.0"
    const val klock = "1.4.0"
}

object Libs {
    const val clikt = "com.github.ajalt:clikt:${Versions.clikt}"
    const val junit_jupiter_api = "org.junit.jupiter:junit-jupiter-api:${Versions.junit}"
    const val junit_jupiter_engine = "org.junit.jupiter:junit-jupiter-engine:${Versions.junit}"
    const val kluent = "org.amshove.kluent:kluent:${Versions.kluent}"
    const val multiarray = "com.github.Lipen:MultiArray:${Versions.multiarray}"
    const val okio = "com.squareup.okio:okio:${Versions.okio}"
    const val mordant = "com.github.ajalt:mordant:${Versions.mordant}"
    const val kotlin_xml_builder = "org.redundent:kotlin-xml-builder:${Versions.kotlin_xml_builder}"
    const val lazycache = "com.github.Lipen:kotlin-lazycache:${Versions.lazycache}"
    const val klock = "com.soywiz.korlibs.klock:klock-jvm:${Versions.klock}"
}
