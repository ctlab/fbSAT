@file:Suppress("PublicApiImplicitType")

object Versions {
    const val kotlin = "1.3.72"
    const val jgitver = "0.8.0"
    const val ktlint = "0.37.2"
    const val ktlint_gradle = "9.3.0"
    const val clikt = "2.8.0"
    const val junit = "5.5.1"
    const val kluent = "1.56"
    const val multiarray = "0.6.0"
    const val shadow = "5.1.0"
    const val okio = "2.4.3"
    const val gradle_versions = "0.27.0"
    const val mordant = "1.2.1"
    const val xml_builder = "1.5.3"
    const val lazycache = "0.3.0"
    const val klock = "1.8.6"
    const val jnisat = "0.7.0"
    const val pretty_print = "2.0.2"
    const val kotlinx_serialization = "0.20.0"
    const val xmlutil = "$kotlinx_serialization.1"
}

object Libs {
    const val clikt = "com.github.ajalt:clikt:${Versions.clikt}"
    const val junit_jupiter_api = "org.junit.jupiter:junit-jupiter-api:${Versions.junit}"
    const val junit_jupiter_engine = "org.junit.jupiter:junit-jupiter-engine:${Versions.junit}"
    const val junit_jupiter_params = "org.junit.jupiter:junit-jupiter-params:${Versions.junit}"
    const val kluent = "org.amshove.kluent:kluent:${Versions.kluent}"
    const val multiarray = "com.github.Lipen:MultiArray:${Versions.multiarray}"
    const val okio = "com.squareup.okio:okio:${Versions.okio}"
    const val mordant = "com.github.ajalt:mordant:${Versions.mordant}"
    const val xml_builder = "org.redundent:kotlin-xml-builder:${Versions.xml_builder}"
    const val lazycache = "com.github.Lipen:kotlin-lazycache:${Versions.lazycache}"
    const val klock = "com.soywiz.korlibs.klock:klock-jvm:${Versions.klock}"
    const val jnisat = "com.github.Lipen:kotlin-jnisat:${Versions.jnisat}"
    const val pretty_print = "com.tylerthrailkill.helpers:pretty-print:${Versions.pretty_print}"
    const val kotlinx_serialization = "org.jetbrains.kotlinx:kotlinx-serialization-runtime:${Versions.kotlinx_serialization}"
    const val xmlutil_jvm = "net.devrieze:xmlutil-jvm:${Versions.xmlutil}"
    const val xmlutil_serialization_jvm = "net.devrieze:xmlutil-serialization-jvm:${Versions.xmlutil}"
}
