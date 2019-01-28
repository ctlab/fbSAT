import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

group = "ru.ifmo.fbsat"
version = "1.0"

plugins {
    kotlin("jvm") version "1.3.20"
    // id("com.github.johnrengelman.shadow") version "4.0.4"
}

dependencies {
    implementation(kotlin("stdlib-jdk8"))
    implementation("com.github.ajalt:clikt:1.6.0")
}

repositories {
    mavenCentral()
}

tasks.withType<KotlinCompile> {
    kotlinOptions.jvmTarget = "1.8"
}

// tasks.withType<ShadowJar> {
//     baseName = "fbSAT"
//     manifest.attributes.apply {
//         put("Main-Class", "ru.ifmo.fbsat.MainKt")
//     }
// }

tasks.withType<Jar> {
    manifest {
        attributes["Main-Class"] = "ru.ifmo.fbsat.MainKt"
    }
    from(configurations.compileClasspath.get().map { if (it.isDirectory) it else zipTree(it) })
}
