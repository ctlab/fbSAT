plugins {
    application
    id("com.github.johnrengelman.shadow")
}

dependencies {
    implementation(project(":core"))
    implementation(Libs.clikt)
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
    archiveClassifier.set(null as String?)
    archiveVersion.set(null as String?)
    minimize()
}
