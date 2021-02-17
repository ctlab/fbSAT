@file:Suppress("DuplicatedCode")

package ru.ifmo.fbsat.cli.command.randexp

import kotlinx.serialization.json.Json
import okio.buffer
import okio.sink
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.fbsatSerializersModule
import ru.ifmo.fbsat.core.utils.useWith
import ru.ifmo.fbsat.core.utils.writeln
import java.io.File

private val logger = MyLogger {}

private val myJson = Json {
    serializersModule = fbsatSerializersModule
    prettyPrint = true
}

fun generateBatch(
    C: Int, // number of states
    // I: Int, // number of input events
    // O: Int, // number of output events
    X: Int, // number of input variables
    Z: Int, // number of output variables
    n: Int, // number of scenarios
    k: Int, // scenario length
    solver: String,
    automatonSeeds: IntRange,
    scenariosSeeds: IntRange,
    solverSeeds: IntRange,
    outBaseDir: File = File("out/randexp-batch"),
    fbsatBin: String = "fbsat",
    slurmFile: File,
    maxSimultaneousTasks: Int? = null,
) {
    val arrayAutomatonSeeds = mutableListOf<Int>()
    val arrayScenariosSeeds = mutableListOf<Int>()
    val arraySolverSeeds = mutableListOf<Int>()
    for (automatonSeed in automatonSeeds)
        for (scenariosSeed in scenariosSeeds)
            for (solverSeed in solverSeeds) {
                arrayAutomatonSeeds += automatonSeed
                arrayScenariosSeeds += scenariosSeed
                arraySolverSeeds += solverSeed
            }

    val outDir = outBaseDir.resolve(
        "exp" +
            "_C${C}_X${X}_Z${Z}_a\$automatonSeed" +
            "_n${n}_k${k}_s\$scenariosSeed" +
            "_${solver}_r\$solverSeed"
    )
    val lockFile = outDir.resolve("lock")

    val fbsatCmd = buildString {
        append(fbsatBin)
        // Note: do not forget spaces!
        append(" randexp")
        // Automaton params
        append(" -C $C -X $X -Z $Z --automaton-seed \$automatonSeed")
        // Scenarios params
        append(" -n $n -k $k --scenarios-seed \$scenariosSeed")
        // Inference params
        append(" --method extended-min -P 5")
        append(" --$solver --solver-seed \$solverSeed")
        if (solver in listOf("minisat", "glucose")) {
            append(" --solver-rnd-freq 0.01 --solver-rnd-pol --solver-rnd-init")
        }
        append(" --outdir $outDir")
        // Misc params
        append(" --no-render-with-dot")
        append(" --debug")
    }
    logger.debug { "fbsat command: $fbsatCmd" }

    slurmFile.sink().buffer().useWith {
        // sbatch header
        writeln("""
            #!/bin/bash
            #SBATCH --job-name=randexp-batch
            #SBATCH --output=${slurmFile.resolveSibling("slurm-%A_%a.log")}
            #
            #SBATCH --ntasks=1
            #SBATCH --time=01:00:00
            #SBATCH --mem=1G
            #
            #SBATCH --array=0-${arraySolverSeeds.size - 1}${if (maxSimultaneousTasks != null) "%$maxSimultaneousTasks" else ""}
        """.trimIndent())

        // general info
        writeln("echo pwd: $(pwd); echo hostname: $(hostname); echo date: $(date -Iseconds)").writeln()
        writeln("echo SLURM_ARRAY_TASK_ID=\$SLURM_ARRAY_TASK_ID")

        // data for array jobs
        writeln("""
            AUTOMATON_SEEDS=(${arrayAutomatonSeeds.joinToString(" ")})
            SCENARIOS_SEEDS=(${arrayScenariosSeeds.joinToString(" ")})
            SOLVER_SEEDS=(${arraySolverSeeds.joinToString(" ")})

            automatonSeed=${'$'}{AUTOMATON_SEEDS[${'$'}SLURM_ARRAY_TASK_ID]}
            scenariosSeed=${'$'}{SCENARIOS_SEEDS[${'$'}SLURM_ARRAY_TASK_ID]}
            solverSeed=${'$'}{SOLVER_SEEDS[${'$'}SLURM_ARRAY_TASK_ID]}
        """.trimIndent())

        // ensure output dir exists
        writeln().writeln("mkdir -p $outDir")

        // create lock file
        writeln().writeln("touch $lockFile")

        // run experiment
        writeln().writeln("srun $fbsatCmd")

        // remove lock file
        writeln().writeln("rm -f $lockFile")

        // general info
        writeln().writeln("echo date: $(date -Iseconds)")
    }
}
