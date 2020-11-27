import org.gradle.api.tasks.testing.logging.TestLogEvent
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("plugin.serialization")
}

dependencies {
    implementation(kotlin("reflect"))
    implementation(Libs.XmlBuilder.xml_builder)
    implementation(Libs.MultiArray.multiarray)
    implementation(Libs.Mordant.mordant)
    implementation(Libs.Okio.okio)
    implementation(Libs.LazyCache.lazycache)
    implementation(Libs.Klock.klock_jvm)
    implementation(Libs.Satlib.satlib_core)
    implementation(Libs.KotlinxSerialization.serialization_json)
    implementation(Libs.XmlUtil.xmlutil_jvm)
    implementation(Libs.XmlUtil.xmlutil_serialization_jvm)
    implementation(Libs.Log4j.log4j_api)
    implementation(Libs.Log4j.log4j_core)
}

tasks.withType<KotlinCompile> {
    kotlinOptions.freeCompilerArgs += "-Xinline-classes"
}

tasks.withType<Test> {
    @Suppress("UnstableApiUsage")
    useJUnitPlatform()
    testLogging.events(
        TestLogEvent.FAILED,
        TestLogEvent.PASSED,
        TestLogEvent.SKIPPED,
        TestLogEvent.STANDARD_ERROR
    )
}
