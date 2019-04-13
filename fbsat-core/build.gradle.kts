import org.gradle.api.tasks.testing.logging.TestLogEvent

dependencies {
    implementation(kotlin("reflect")) // for kotlin-xml-builder
    implementation(Libs.kotlin_xml_builder)
    implementation(Libs.multiarray)
    implementation(Libs.mordant)
    implementation(Libs.okio)

    testImplementation(Libs.junit_jupiter_api)
    testRuntimeOnly(Libs.junit_jupiter_engine)
    testImplementation(Libs.kluent)
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
