plugins {
    id("com.github.johnrengelman.shadow")
}

dependencies {
    implementation(project(":core"))
    implementation(Libs.okio)
}

tasks.shadowJar {
    archiveBaseName.set("convertTracesToScenarios")
    archiveClassifier.set("")
    archiveVersion.set("")
    manifest.attributes("Main-Class" to "ru.ifmo.fbsat.t2s.TracesToScenariosKt")
}
