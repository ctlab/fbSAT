import org.jetbrains.kotlin.gradle.tasks.KotlinCompile
import org.jlleitschuh.gradle.ktlint.KtlintExtension

plugins {
    idea
    kotlin("jvm") version Versions.kotlin
    kotlin("plugin.serialization") version Versions.kotlin apply false
    id("fr.brouillard.oss.gradle.jgitver") version Versions.jgitver
    id("com.github.ben-manes.versions") version Versions.gradle_versions
    id("org.jlleitschuh.gradle.ktlint") version Versions.ktlint_gradle apply false
    id("com.github.johnrengelman.shadow") version Versions.shadow apply false
}

allprojects {
    group = "ru.ifmo.fbsat"
    repositories {
        mavenCentral()
        jcenter()
        maven(url = "https://jitpack.io")
    }
}

subprojects {
    apply(plugin = "kotlin")
    apply(plugin = "org.jlleitschuh.gradle.ktlint")

    dependencies {
        implementation(kotlin("stdlib-jdk8"))
        implementation(Libs.pretty_print)

        testImplementation(Libs.junit_jupiter_api)
        testRuntimeOnly(Libs.junit_jupiter_engine)
        testImplementation(Libs.junit_jupiter_params)
        testImplementation(Libs.kluent)
    }

    configure<KtlintExtension> {
        version.set(Versions.ktlint)
        ignoreFailures.set(true)
        enableExperimentalRules.set(true)
        disabledRules.set(setOf("import-ordering"))
    }

    tasks.withType<KotlinCompile> {
        kotlinOptions.jvmTarget = "1.8"
    }
}

idea {
    module {
        isDownloadSources = true
        isDownloadJavadoc = true
    }
}

tasks.wrapper {
    gradleVersion = "6.3"
    distributionType = Wrapper.DistributionType.ALL
}

defaultTasks("clean", "build", "installDist")
