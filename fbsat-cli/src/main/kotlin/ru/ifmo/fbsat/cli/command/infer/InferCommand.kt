package ru.ifmo.fbsat.cli.command.infer

import com.github.ajalt.clikt.core.NoOpCliktCommand
import com.github.ajalt.clikt.core.subcommands
import ru.ifmo.fbsat.cli.command.infer.distributed.InferDistributedBasicCommand
import ru.ifmo.fbsat.cli.command.infer.modular.arbitrary.InferArbitraryModularBasicCommand
import ru.ifmo.fbsat.cli.command.infer.modular.arbitrary.InferArbitraryModularBasicMinCCommand
import ru.ifmo.fbsat.cli.command.infer.modular.arbitrary.InferArbitraryModularBasicMinCommand
import ru.ifmo.fbsat.cli.command.infer.modular.consecutive.InferConsecutiveModularBasicCommand
import ru.ifmo.fbsat.cli.command.infer.modular.consecutive.InferConsecutiveModularBasicMinCCommand
import ru.ifmo.fbsat.cli.command.infer.modular.consecutive.InferConsecutiveModularBasicMinCommand
import ru.ifmo.fbsat.cli.command.infer.modular.consecutive.InferConsecutiveModularExtendedCommand
import ru.ifmo.fbsat.cli.command.infer.modular.consecutive.InferConsecutiveModularExtendedMinCommand
import ru.ifmo.fbsat.cli.command.infer.modular.parallel.InferParallelModularBasicCommand
import ru.ifmo.fbsat.cli.command.infer.modular.parallel.InferParallelModularBasicMinCCommand
import ru.ifmo.fbsat.cli.command.infer.modular.parallel.InferParallelModularBasicMinCommand
import ru.ifmo.fbsat.cli.command.infer.modular.parallel.InferParallelModularExtendedCommand
import ru.ifmo.fbsat.cli.command.infer.modular.parallel.InferParallelModularExtendedMinCommand
import ru.ifmo.fbsat.cli.command.infer.mono.InferBasicCommand
import ru.ifmo.fbsat.cli.command.infer.mono.InferBasicMinCCommand
import ru.ifmo.fbsat.cli.command.infer.mono.InferBasicMinCommand
import ru.ifmo.fbsat.cli.command.infer.mono.InferCegisCommand
import ru.ifmo.fbsat.cli.command.infer.mono.InferCegisMinCommand
import ru.ifmo.fbsat.cli.command.infer.mono.InferCompleteCommand
import ru.ifmo.fbsat.cli.command.infer.mono.InferCompleteMinCommand
import ru.ifmo.fbsat.cli.command.infer.mono.InferExtendedCommand
import ru.ifmo.fbsat.cli.command.infer.mono.InferExtendedMinCommand
import ru.ifmo.fbsat.cli.command.infer.mono.InferExtendedMinUBCommand

class InferCommand : NoOpCliktCommand() {
    init {
        subcommands(
            InferBasicCommand(),
            InferBasicMinCommand(),
            InferBasicMinCCommand(),
            InferExtendedCommand(),
            InferExtendedMinCommand(),
            InferExtendedMinUBCommand(),
            InferCompleteCommand(),
            InferCompleteMinCommand(),
            InferCegisCommand(),
            InferCegisMinCommand(),
            InferParallelModularBasicCommand(),
            InferParallelModularBasicMinCommand(),
            InferParallelModularBasicMinCCommand(),
            InferParallelModularExtendedCommand(),
            InferParallelModularExtendedMinCommand(),
            InferConsecutiveModularBasicCommand(),
            InferConsecutiveModularBasicMinCommand(),
            InferConsecutiveModularBasicMinCCommand(),
            InferConsecutiveModularExtendedCommand(),
            InferConsecutiveModularExtendedMinCommand(),
            InferArbitraryModularBasicCommand(),
            InferArbitraryModularBasicMinCommand(),
            InferArbitraryModularBasicMinCCommand(),
            InferDistributedBasicCommand()
        )
    }
}
