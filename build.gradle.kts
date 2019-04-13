import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    `build-scan`
    kotlin("jvm") version Versions.kotlin
    id("org.jlleitschuh.gradle.ktlint") version Versions.ktlint
    id("fr.brouillard.oss.gradle.jgitver") version Versions.jgitver
    id("com.github.ben-manes.versions") version Versions.gradle_versions
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

    // Without an explicit `this`, IDEA goes crazy during gradle project import:
    // it finds and auto-imports `Versions.ktlint` before `(this).ktlint`.
    this.ktlint {
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
    gradleVersion = "5.3.1"
    distributionType = Wrapper.DistributionType.ALL
}

defaultTasks("clean", "build", "installDist")
