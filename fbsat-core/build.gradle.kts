import org.gradle.api.tasks.testing.logging.TestLogEvent
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

// plugins {
//     `java-library`
// }
//
// sourceSets.create("jmh") {
//     java.setSrcDirs(listOf("src/jmh/kotlin"))
//     resources.setSrcDirs(listOf("src/jmh/resources"))
// }

plugins {
    id("me.champeau.gradle.jmh") version "0.5.0"
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

    testImplementation(Libs.junit_jupiter_api)
    testRuntimeOnly(Libs.junit_jupiter_engine)
    testImplementation(Libs.kluent)

    // "jmhImplementation"(kotlin("stdlib"))
    // "jmhImplementation"(project)
    // "jmhImplementation"("org.openjdk.jmh:jmh-core:1.23")
    // "jmhAnnotationProcessor"("org.openjdk.jmh:jmh-generator-annprocess:1.23")
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

// tasks.register("jmh", type = JavaExec::class) {
//     dependsOn("jmhClasses")
//     group = "benchmark"
//     main = "org.openjdk.jmh.Main"
//     classpath = sourceSets["jmh"].compileClasspath + sourceSets["jmh"].runtimeClasspath
//     // To pass parameters ("-h" gives a list of possible parameters)
//     // args(listOf("-h"))
// }

jmh {
    jmhVersion = "1.23"
    humanOutputFile = project.file("${project.buildDir}/reports/jmh/human.txt")
    resultsFile = project.file("${project.buildDir}/reports/jmh/results.json")
    resultFormat = "JSON"
}
