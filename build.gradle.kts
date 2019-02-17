import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

group = "ru.ifmo.fbsat"
version = "1.0"

plugins {
    application
    kotlin("jvm") version Versions.kotlin
    id("com.github.johnrengelman.shadow") version Versions.shadow
    id("me.champeau.gradle.jmh") version Versions.jmh
}

dependencies {
    implementation(kotlin("stdlib"))
    implementation(Libs.clikt)

    testImplementation(Libs.junit_jupiter_api)
    testRuntimeOnly(Libs.junit_jupiter_engine)
}

repositories {
    jcenter()
    mavenCentral()
}

application {
    mainClassName = "ru.ifmo.fbsat.MainKt"
}

tasks.withType<JavaExec> {
    args("--help")
}

tasks.withType<KotlinCompile> {
    kotlinOptions.jvmTarget = "1.8"
}

tasks.test {
    useJUnit()
}

tasks.jar {
    manifest {
        attributes(
            "Main-Class" to "ru.ifmo.fbsat.MainKt"
        )
    }
    from(configurations.runtimeClasspath.get().map { if (it.isDirectory) it else zipTree(it) })
}

tasks.wrapper {
    gradleVersion = "5.2.1"
    distributionType = Wrapper.DistributionType.ALL
}
