import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

group = "ru.ifmo.fbsat"

plugins {
    `build-scan`
    application
    kotlin("jvm") version Versions.kotlin
    id("org.jlleitschuh.gradle.ktlint") version Versions.ktlint
    id("fr.brouillard.oss.gradle.jgitver") version Versions.jgitver
    id("com.github.johnrengelman.shadow") version Versions.shadow
    id("org.jetbrains.dokka") version Versions.dokka
    id("com.github.ben-manes.versions") version Versions.gradle_versions
}

repositories {
    jcenter()
}

dependencies {
    implementation(kotlin("stdlib"))
    implementation(kotlin("reflect", Versions.kotlin))
    implementation(Libs.clikt)
    implementation(Libs.multiarray)
    implementation(Libs.okio)
    implementation(Libs.mordant)
    implementation(Libs.kotlin_xml_builder)

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

defaultTasks("clean", "build", "installDist")

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

    dokka {
        outputFormat = "html"
        outputDirectory = "$buildDir/javadoc"
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
        gradleVersion = "5.4"
        distributionType = Wrapper.DistributionType.ALL
    }
}
