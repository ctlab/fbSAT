@file:Suppress("PublicApiImplicitType", "MemberVisibilityCanBePrivate")

object Versions {
    const val kotlin = "1.4.10"
    const val jgitver = "0.9.1"
    const val clikt = "2.8.0"
    const val mordant = "1.2.1"
    const val junit = "5.7.0"
    const val kluent = "1.63"
    const val klock = "1.12.1"
    const val okio = "2.9.0"
    const val multiarray = "0.6.0"
    const val lazycache = "0.3.0"
    const val jnisat = "0.7.0"
    const val pretty_print = "2.0.2"
    const val xml_builder = "1.5.3"
    const val xmlutil = "0.80.1"
    const val kotlinx_serialization = "1.0.1"
    const val kotlinx_coroutines = "1.4.1"
    const val arrow = "0.11.0"
    const val ktlint = "0.39.0"
    const val ktlint_gradle_plugin = "9.4.1"
    const val shadow = "6.1.0"
    const val gradle_versions = "0.36.0"
}

object Libs {
    // https://github.com/ajalt/clikt
    object Clikt {
        const val version = Versions.clikt
        const val clikt = "com.github.ajalt:clikt:$version"
    }

    // https://github.com/ajalt/mordant
    object Mordant {
        const val version = Versions.mordant
        const val mordant = "com.github.ajalt:mordant:$version"
    }

    // https://github.com/junit-team/junit5
    object JUnit {
        const val version = Versions.junit
        const val jupiter_api = "org.junit.jupiter:junit-jupiter-api:$version"
        const val jupiter_engine = "org.junit.jupiter:junit-jupiter-engine:$version"
        const val jupiter_params = "org.junit.jupiter:junit-jupiter-params:$version"
    }

    // https://github.com/MarkusAmshove/Kluent
    object Kluent {
        const val version = Versions.kluent
        const val kluent = "org.amshove.kluent:kluent:$version"
    }

    // https://github.com/snowe2010/pretty-print
    object PrettyPrint {
        const val version = Versions.pretty_print
        const val pretty_print = "com.tylerthrailkill.helpers:pretty-print:$version"
    }

    // https://github.com/korlibs/klock
    object Klock {
        const val version = Versions.klock
        const val klock_jvm = "com.soywiz.korlibs.klock:klock-jvm:$version"
    }

    // https://github.com/square/okio
    object Okio {
        const val version = Versions.okio
        const val okio = "com.squareup.okio:okio:$version"
    }

    // https://github.com/Lipen/MultiArray
    object MultiArray {
        const val version = Versions.multiarray
        const val multiarray = "com.github.lipen:multiarray:$version"
    }

    // https://github.com/Lipen/kotlin-lazycache
    object LazyCache {
        const val version = Versions.lazycache
        const val lazycache = "com.github.lipen:kotlin-lazycache:$version"
    }

    // https://github.com/Lipen/kotlin-jnisat
    object JniSat {
        const val version = Versions.jnisat
        const val jnisat = "com.github.Lipen:kotlin-jnisat:$version"
    }

    // https://github.com/redundent/kotlin-xml-builder
    object XmlBuilder {
        const val version = Versions.xml_builder
        const val xml_builder = "org.redundent:kotlin-xml-builder:$version"
    }

    // https://github.com/pdvrieze/xmlutil
    object XmlUtil {
        const val version = Versions.xmlutil
        const val xmlutil_jvm = "net.devrieze:xmlutil-jvm:$version"
        const val xmlutil_serialization_jvm = "net.devrieze:xmlutil-serialization-jvm:$version"
    }

    // https://github.com/Kotlin/kotlinx.serialization
    object KotlinxSerialization {
        const val version = Versions.kotlinx_serialization
        const val serialization_json = "org.jetbrains.kotlinx:kotlinx-serialization-json:$version"
    }

    // https://github.com/Kotlin/kotlinx.coroutines
    object KotlinxCoroutines {
        const val version = Versions.kotlinx_coroutines
    }

    // https://github.com/arrow-kt/arrow
    object Arrow {
        const val version = Versions.arrow
        const val arrow_core = "io.arrow-kt:arrow-core:$version"
        const val arrow_syntax = "io.arrow-kt:arrow-syntax:$version"
        const val arrow_meta = "io.arrow-kt:arrow-meta:$version"
    }
}

object Plugins {
    // https://github.com/jgitver/gradle-jgitver-plugin
    object Jgitver {
        const val version = Versions.jgitver
        const val id = "fr.brouillard.oss.gradle.jgitver"
    }

    // https://github.com/ben-manes/gradle-versions-plugin
    object GradleVersions {
        const val version = Versions.gradle_versions
        const val id = "com.github.ben-manes.versions"
    }

    // https://github.com/JLLeitschuh/ktlint-gradle
    object Ktlint {
        const val version = Versions.ktlint_gradle_plugin
        const val id = "org.jlleitschuh.gradle.ktlint"
    }

    // https://github.com/johnrengelman/shadow
    object Shadow {
        const val version = Versions.shadow
        const val id = "com.github.johnrengelman.shadow"
    }
}
