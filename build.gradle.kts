import org.jetbrains.kotlin.gradle.tasks.KotlinCompile
import org.jlleitschuh.gradle.ktlint.KtlintExtension

plugins {
    idea
    kotlin("jvm") version Versions.kotlin
    id("fr.brouillard.oss.gradle.jgitver") version Versions.jgitver
    id("com.github.ben-manes.versions") version Versions.gradle_versions
    id("org.jlleitschuh.gradle.ktlint") version Versions.ktlint apply false
    id("com.github.johnrengelman.shadow") version Versions.shadow apply false
    kotlin("plugin.serialization") version Versions.kotlin
}

allprojects {
    group = "ru.ifmo.fbsat"
    repositories {
        maven(url = "https://jitpack.io")
        jcenter()
    }
}

subprojects {
    apply(plugin = "kotlin")
    apply(plugin = "org.jlleitschuh.gradle.ktlint")

    dependencies {
        implementation(kotlin("stdlib-jdk8"))
    }

    configure<KtlintExtension> {
        ignoreFailures.set(true)
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
