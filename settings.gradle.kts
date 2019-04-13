rootProject.name = "fbSAT"

include("core")
project(":core").projectDir = file("fbsat-core")

include("cli")
project(":cli").projectDir = file("fbsat-cli")
