package ru.ifmo.fbsat.core.utils

import com.soywiz.klock.DateTime
import org.redundent.kotlin.xml.xml
import ru.ifmo.fbsat.core.automaton.ModularAutomaton
import java.io.File

fun ModularAutomaton.generateEventMerger(name: String? = null): String {
    require(M >= 2) { "E_MERGE must be at least 2-ary" }
    require(M != 2) { "Use the standard 2-ary IEC61499.Standard.E_MERGE instead" }

    fun r() = "%.3f".format((1.0..1000.0).random())

    return xml("FBType") {
        if (name != null) {
            attribute("Name", name)
        }
        attribute("Namespace", "Main")
        "Identification"("Standard" to "61499-2")
        "VersionInfo"(
            "Organization" to "nxtControl GmbH",
            "Version" to "0.0",
            "Author" to "fbSAT",
            "Date" to DateTime.nowLocal().format("yyyy-MM-dd")
        )
        "InterfaceList" {
            "EventInputs" {
                for (m in 1..M) {
                    "Event"("Name" to "EI$m")
                }
            }
            "EventOutputs" {
                "Event"("Name" to "EO")
            }
        }
        "FBNetwork" {
            for (m in 1 until M) {
                "FB"(
                    "Name" to "EM$m",
                    "Type" to "E_MERGE",
                    "Namespace" to "IEC61499.Standard",
                    "x" to r(), "y" to r()
                )
            }
            "EventConnections" {
                for (m in listOf(1, 2)) {
                    "Connection"(
                        "Source" to "EI$m",
                        "Destination" to "EM1.EI$m"
                    )
                }
                for (m in 3..M) {
                    "Connection"(
                        "Source" to "EM${m - 2}.EO",
                        "Destination" to "EM${m - 1}.EI1"
                    )
                    "Connection"(
                        "Source" to "EI$m",
                        "Destination" to "EM${m - 1}.EI2"
                    )
                }
                "Connection"(
                    "Source" to "EM${M - 1}.EO",
                    "Destination" to "EO"
                )
            }
        }
    }.toString(Globals.xmlPrintOptions)
}

fun ModularAutomaton.writeEventMerger(file: File, name: String? = null) {
    file.writeText(generateEventMerger(name = name))
}
