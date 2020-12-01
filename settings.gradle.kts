rootProject.name = "fbSAT"

plugins {
    id("com.gradle.enterprise") version "3.4.1"
}

gradleEnterprise {
    buildScan {
        termsOfServiceUrl = "https://gradle.com/terms-of-service"
        termsOfServiceAgree = "yes"
    }
}

fun myInclude(name:String) {
    include(name)
    project(":$name").projectDir = file("fbsat-$name")
}

myInclude("core")
myInclude("cli")
