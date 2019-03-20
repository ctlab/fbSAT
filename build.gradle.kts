import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

group = "ru.ifmo.fbsat"

plugins {
    `build-scan`
    application
    kotlin("jvm") version Versions.kotlin
    id("org.jlleitschuh.gradle.ktlint") version Versions.ktlint
    id("fr.brouillard.oss.gradle.jgitver") version Versions.jgitver
    id("com.github.johnrengelman.shadow") version Versions.shadow
}

repositories {
    jcenter()
}

dependencies {
    implementation(kotlin("stdlib"))
    implementation(Libs.clikt)
    implementation(Libs.multiarray)
    implementation(Libs.okio)

    // testImplementation(Libs.kotlintest_runner_junit5)
    testImplementation(Libs.junit_jupiter_api)
    testRuntimeOnly(Libs.junit_jupiter_engine)
    testImplementation(Libs.junit_jupiter_params)
    testImplementation(Libs.kluent)
}

buildScan {
    termsOfServiceUrl = "https://gradle.com/terms-of-service"
    termsOfServiceAgree = "yes"
}

application {
    mainClassName = "ru.ifmo.fbsat.MainKt"
}

ktlint {
    ignoreFailures.set(true)
}

tasks {
    withType<JavaExec> {
        args("--help")
    }

    withType<KotlinCompile> {
        kotlinOptions.jvmTarget = "1.8"
    }

    withType<Test> {
        useJUnit()
    }

    jar {
        manifest {
            attributes(
                "Main-Class" to "ru.ifmo.fbsat.MainKt"
            )
        }
    }

    shadowJar {
        archiveBaseName.set(rootProject.name)
        archiveClassifier.set(null as String?)
        archiveVersion.set(null as String?)
    }

    wrapper {
        gradleVersion = "5.2.1"
        distributionType = Wrapper.DistributionType.ALL
    }
}
