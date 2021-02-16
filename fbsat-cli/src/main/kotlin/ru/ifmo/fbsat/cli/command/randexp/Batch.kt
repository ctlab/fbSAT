@file:Suppress("DuplicatedCode")

package ru.ifmo.fbsat.cli.command.randexp

import kotlinx.serialization.json.Json
import okio.buffer
import okio.sink
import okio.source
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.ensureParentExists
import ru.ifmo.fbsat.core.utils.fbsatSerializersModule
import ru.ifmo.fbsat.core.utils.lineSequence
import ru.ifmo.fbsat.core.utils.useWith
import ru.ifmo.fbsat.core.utils.writeln
import java.io.File
import java.util.concurrent.TimeUnit

private val logger = MyLogger {}

private val myJson = Json {
    serializersModule = fbsatSerializersModule
    prettyPrint = true
}

fun runBatch(
    C: Int, // number of states
    // I: Int, // number of input events
    // O: Int, // number of output events
    X: Int, // number of input variables
    Z: Int, // number of output variables
    n: Int, // number of scenarios
    k: Int, // scenario length
    solver: String,
    automatonSeed: Int,
    scenariosSeed: Int,
    solverSeed: Int,
    outBaseDir: File = File("out/randexp-batch"),
    fbsatBin: String = "fbsat",
    sbatchBin: String = "sbatch",
    sbatchOptions: List<String> = emptyList(),
    runFileName: String = "run.sh",
    ignoreLock: Boolean = false,
    dryRun: Boolean = false,
) {
    // a -- automaton seed
    // s -- scenarios seed
    // r -- solver seed
    val name = "exp" +
        "_C${C}_X${X}_Z${Z}_a${automatonSeed}" +
        "_n${n}_k${k}_s${scenariosSeed}" +
        "_${solver}_r${solverSeed}"
    val outDir = outBaseDir.resolve(name)

    val resultFile = outDir.resolve("result.json")
    val batchLockFile = outDir.resolve("batch.lock")

    val needRun: Boolean = when {
        resultFile.exists() -> {
            logger.info { "Results already exist for '$name' => not running" }
            false
        }
        batchLockFile.exists() -> {
            if (ignoreLock) {
                logger.info { "Results absent, but lock-file exists for '$name', which we ignore" }

                logger.debug { "Removing lock file..." }
                batchLockFile.delete()

                true
            } else {
                logger.info { "Results absent, but lock-file exists for '$name' => not running" }
                false
            }
        }
        // If there is no reason not to run -- run!
        else -> true
    }

    if (needRun) {
        logger.info { "Running random experiment for '$name'..." }

        val fbsatCmd = buildString {
            append(fbsatBin)
            // Note: do not forget spaces!
            append(" randexp")
            // Automaton params
            append(" -C $C -X $X -Z $Z --automaton-seed $automatonSeed")
            // Scenarios params
            append(" -n $n -k $k --scenarios-seed $scenariosSeed")
            // Inference params
            append(" --method extended-min -P 5")
            append(" --$solver --solver-seed $solverSeed")
            if (solver in listOf("minisat", "glucose")) {
                append(" --solver-rnd-freq 0.1 --solver-rnd-pol --solver-rnd-init")
            }
            // Misc params
            append(" --outdir $outDir")
            append(" --no-render-with-dot")
            append(" --debug")
        }
        logger.debug { "fbsat command: $fbsatCmd" }

        val runFile = outDir.resolve(runFileName)
        runFile.ensureParentExists().sink().buffer().useWith {
            writeln("#!/bin/bash")
            writeln("## Default sbatch options. Note: they might be overridden below.")
            writeln("#SBATCH --mem=1G")
            writeln("#SBATCH --time=00:30:00")
            writeln("#SBATCH --output=$outDir/slurm-%j.log")
            if (sbatchOptions.isNotEmpty()) {
                writeln("## Custom sbatch options. Note: they might override the default options above.")
                for (opt in sbatchOptions) {
                    writeln("#SBATCH $opt")
                }
            }
            writeln("echo pwd: $(pwd); echo hostname: $(hostname); date -Iseconds").writeln()

            writeln(fbsatCmd)

            // Remove lock file after execution
            writeln().writeln("rm -f $batchLockFile")

            writeln().writeln("date -Iseconds")
        }

        val sbatchCmd = "$sbatchBin $runFile"
        logger.debug { "sbatch command: $sbatchCmd" }

        if (!dryRun) {
            // Create lock file signaling that the computation is still running
            logger.debug { "Creating lock file '$batchLockFile'..." }
            batchLockFile.createNewFile()

            val p = Runtime.getRuntime().exec(sbatchCmd)
            p.outputStream.close()
            logger.debug { "Awaiting for sbatch to submit a job..." }
            for (line in p.inputStream.source().buffer().lineSequence()) {
                logger.debug { "(stdout) $line" }
            }
            for (line in p.errorStream.source().buffer().lineSequence()) {
                logger.debug { "(stderr) $line" }
            }
            p.waitFor(1, TimeUnit.MINUTES)
        }
    }
}
