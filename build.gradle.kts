import de.undercouch.gradle.tasks.download.DownloadAction
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile
import org.jlleitschuh.gradle.ktlint.KtlintExtension

plugins {
    idea
    kotlin("jvm") version Versions.kotlin
    kotlin("plugin.serialization") version Versions.kotlin apply false
    with(Plugins.Jgitver) { id(id) version version }
    with(Plugins.GradleVersions) { id(id) version version }
    with(Plugins.Ktlint) { id(id) version version apply false }
    with(Plugins.Shadow) { id(id) version version apply false }
    with(Plugins.GradleDownloadTask) { id(id) version version }
}

allprojects {
    group = "ru.ifmo.fbsat"

    repositories {
        // mavenLocal()
        mavenCentral()
        maven(url = "https://jitpack.io")
        jcenter()
    }
}

subprojects {
    apply(plugin = "kotlin")
    apply(plugin = Plugins.Ktlint.id)

    dependencies {
        implementation(platform(kotlin("bom")))
        implementation(kotlin("stdlib-jdk8"))
        implementation(Libs.KotlinLogging.kotlin_logging)
        implementation(Libs.Log4j.log4j_slf4j_impl)

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

fun Task.download(action: DownloadAction.() -> Unit) =
    download.configure(delegateClosureOf(action))

val osArch: String = run {
    val osName = System.getProperty("os.name")
    val os = when {
        osName.startsWith("Linux") -> "linux"
        osName.startsWith("Windows") -> "win"
        osName.startsWith("Mac OS X") || osName.startsWith("Darwin") -> "osx"
        else -> return@run "unknown"
    }
    val arch = when (System.getProperty("os.arch")) {
        "x86", "i386" -> "32"
        "x86_64", "amd64" -> "64"
        else -> return@run "unknown"
    }
    "$os$arch"
}

tasks.register("downloadLibs") {
    doLast {
        val urlTemplate = "https://github.com/Lipen/kotlin-satlib/releases/download/${Libs.Satlib.version}/%s"
        val libResDir = project(":core").projectDir.resolve("src/main/resources/lib/$osArch")

        fun ensureDirExists(dir: File) {
            if (!dir.exists()) {
                check(dir.mkdirs()) { "Cannot create dirs for '$dir'" }
            }
            check(dir.exists()) { "'$dir' still does not exist" }
        }

        fun downloadLibs(names: List<String>, dest: File) {
            ensureDirExists(dest)
            download {
                src(names.map { urlTemplate.format(it) })
                dest(dest)
                tempAndMove(true)
            }
        }

        when (osArch) {
            "linux64" -> {
                val jLibs = listOf(
                    "libjminisat.so",
                    "libjglucose.so",
                    "libjcms.so",
                    "libjcadical.so",
                )
                downloadLibs(jLibs, libResDir)

                val solverLibs = listOf(
                    "libminisat.so",
                    "libglucose.so",
                    "libcryptominisat5.so",
                    "libcadical.so",
                )
                val solverLibDir = rootDir.resolve("libs")
                downloadLibs(solverLibs, solverLibDir)
            }
            "win64" -> {
                val jLibs = listOf(
                    "jminisat.dll",
                    "jglucose.dll",
                    "jcadical.dll",
                    "jcms.dll",
                )
                downloadLibs(jLibs, libResDir)

                val solverLibs = listOf(
                    "libminisat.dll",
                    "glucose.dll",
                    "cadical.dll",
                    "libcryptominisat5win.dll",
                )
                downloadLibs(solverLibs, rootDir)
            }
            else -> {
                error("$osArch is not supported, sorry")
            }
        }
    }
}

idea {
    module {
        isDownloadSources = true
        isDownloadJavadoc = true
    }
}

tasks.wrapper {
    gradleVersion = "7.2"
    distributionType = Wrapper.DistributionType.ALL
}

defaultTasks("clean", "build", "installDist")
