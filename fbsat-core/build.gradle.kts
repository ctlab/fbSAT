import org.gradle.api.tasks.testing.logging.TestLogEvent
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("plugin.serialization")
}

dependencies {
    implementation(kotlin("reflect"))
    implementation(Libs.xml_builder)
    implementation(Libs.multiarray)
    implementation(Libs.mordant)
    implementation(Libs.okio)
    implementation(Libs.lazycache)
    implementation(Libs.klock)
    implementation(Libs.jnisat)
    implementation(Libs.kotlinx_serialization)
    implementation(Libs.xmlutil_jvm)
    implementation(Libs.xmlutil_serialization_jvm)
}

tasks.withType<KotlinCompile> {
    kotlinOptions.freeCompilerArgs += "-XXLanguage:+InlineClasses"
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
