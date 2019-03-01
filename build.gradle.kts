import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

group = "ru.ifmo.fbsat"
version = "1.0"

plugins {
    application
    kotlin("jvm") version Versions.kotlin
    id("org.jlleitschuh.gradle.ktlint") version Versions.ktlint
}

repositories {
    jcenter()
}

dependencies {
    implementation(kotlin("stdlib"))
    implementation(Libs.clikt)

    testImplementation(Libs.junit_jupiter_api)
    testRuntimeOnly(Libs.junit_jupiter_engine)
    testImplementation(Libs.junit_jupiter_params)
    testImplementation(Libs.kluent)
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

    test {
        useJUnit()
    }

    jar {
        manifest {
            attributes(
                "Main-Class" to "ru.ifmo.fbsat.MainKt"
            )
        }
        from(configurations.runtimeClasspath.get().map { if (it.isDirectory) it else zipTree(it) })
    }

    wrapper {
        gradleVersion = "5.2.1"
        distributionType = Wrapper.DistributionType.ALL
    }
}
