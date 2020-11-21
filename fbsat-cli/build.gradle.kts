plugins {
    application
    id(Plugins.Shadow.id)
    kotlin("plugin.serialization")
}

dependencies {
    implementation(project(":core"))
    implementation(Libs.Clikt.clikt)
    implementation(Libs.MultiArray.multiarray)
    implementation(Libs.Klock.klock_jvm)

    implementation(Libs.KotlinxSerialization.serialization_json)
    implementation(Libs.XmlUtil.xmlutil_jvm)
    implementation(Libs.XmlUtil.xmlutil_serialization_jvm)
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
