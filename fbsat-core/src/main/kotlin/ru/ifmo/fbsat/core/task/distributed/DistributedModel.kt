package ru.ifmo.fbsat.core.task.distributed

import java.io.File
import kotlinx.serialization.*
import kotlinx.serialization.json.*

@Serializable
data class DistributedModel(val inferredModules: List<Module>) {
    @Serializable
    data class Module(
        val name: String,
        val inputEvents: Map<String, String>,
        val inputVars: Map<String, String>,
        val outputEvents: List<String>,
        val outputVars: List<String>
    )

    companion object {
        fun fromFile(file: File): DistributedModel {
            val json = Json(JsonConfiguration.Stable)
            return json.parse(serializer(), file.readText())
        }
    }
}
