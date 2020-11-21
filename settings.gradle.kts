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

include("core")
project(":core").projectDir = file("fbsat-core")

include("cli")
project(":cli").projectDir = file("fbsat-cli")
