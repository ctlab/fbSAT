import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

group = "ru.ifmo.fbsat"
version = "1.0"

plugins {
    kotlin("jvm") version "1.3.21"
}

repositories {
    jcenter()
    mavenCentral()
}

dependencies {
    implementation(kotlin("stdlib-jdk8"))
    implementation("com.github.ajalt:clikt:1.6.0")

    testImplementation("org.junit.jupiter:junit-jupiter:5.4.0")
}

tasks.withType<KotlinCompile> {
    kotlinOptions.jvmTarget = "1.8"
}

tasks.withType<Test> {
    useJUnitPlatform()
}

tasks.withType<Jar> {
    manifest {
        attributes["Main-Class"] = "ru.ifmo.fbsat.MainKt"
    }
    from(configurations.compileClasspath.get().map { if (it.isDirectory) it else zipTree(it) })
}

tasks.withType<Wrapper> {
    gradleVersion = "5.2.1"
    distributionType = Wrapper.DistributionType.BIN
}
