import org.jetbrains.kotlin.gradle.tasks.KotlinCompile
import org.jlleitschuh.gradle.ktlint.KtlintExtension

plugins {
    `build-scan`
    kotlin("jvm") version Versions.kotlin
    id("fr.brouillard.oss.gradle.jgitver") version Versions.jgitver
    id("com.github.ben-manes.versions") version Versions.gradle_versions
    id("org.jlleitschuh.gradle.ktlint") version Versions.ktlint apply false
    id("com.github.johnrengelman.shadow") version Versions.shadow apply false
}

allprojects {
    group = "ru.ifmo.fbsat"
    repositories {
        jcenter()
    }
}

subprojects {
    apply(plugin = "kotlin")
    apply(plugin = "org.jlleitschuh.gradle.ktlint")

    dependencies {
        implementation(kotlin("stdlib"))
    }

    configure<KtlintExtension> {
        ignoreFailures.set(true)
    }

    tasks.withType<KotlinCompile> {
        kotlinOptions.jvmTarget = "1.8"
    }
}

buildScan {
    termsOfServiceUrl = "https://gradle.com/terms-of-service"
    termsOfServiceAgree = "yes"
}

tasks.wrapper {
    gradleVersion = "5.4"
    distributionType = Wrapper.DistributionType.ALL
}

defaultTasks("clean", "build", "installDist")
