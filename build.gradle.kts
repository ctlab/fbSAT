import org.jetbrains.kotlin.gradle.tasks.KotlinCompile
import org.jlleitschuh.gradle.ktlint.KtlintExtension

plugins {
    idea
    kotlin("jvm") version Versions.kotlin
    kotlin("plugin.serialization") version Versions.kotlin apply false
    kotlin("kapt") version Versions.kotlin apply false
    with(Plugins.Jgitver) { id(id) version version }
    with(Plugins.GradleVersions) { id(id) version version }
    with(Plugins.Ktlint) { id(id) version version apply false }
    with(Plugins.Shadow) { id(id) version version apply false }
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
    apply(plugin = Plugins.Ktlint.id)

    dependencies {
        implementation(kotlin("stdlib-jdk8"))
        implementation(Libs.PrettyPrint.pretty_print)

        testImplementation(Libs.JUnit.jupiter_api)
        testRuntimeOnly(Libs.JUnit.jupiter_engine)
        testImplementation(Libs.JUnit.jupiter_params)
        testImplementation(Libs.Kluent.kluent)
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
    gradleVersion = "6.7"
    distributionType = Wrapper.DistributionType.ALL
}

defaultTasks("clean", "build", "installDist")
