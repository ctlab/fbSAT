plugins {
    application
    id("com.github.johnrengelman.shadow")
    kotlin("plugin.serialization")
}

dependencies {
    implementation(project(":core"))
    implementation(Libs.clikt)
    implementation(Libs.multiarray)
    implementation(Libs.klock)

    implementation(Libs.kotlinx_serialization)
    implementation(Libs.xmlutil_jvm)
    implementation(Libs.xmlutil_serialization_jvm)
}

application {
    mainClassName = "ru.ifmo.fbsat.cli.MainKt"
}

tasks.withType<JavaExec> {
    args("--help")
}

tasks.startScripts {
    applicationName = rootProject.name
}

tasks.jar {
    manifest.attributes("Main-Class" to application.mainClassName)
}

tasks.shadowJar {
    archiveBaseName.set(rootProject.name)
    archiveClassifier.set("")
    archiveVersion.set("")
    minimize {
        // exclude(dependency("org.jetbrains.kotlin:kotlin-reflect"))
    }
}
