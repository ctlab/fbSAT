[ ![Build Status](https://travis-ci.org/Lipen/fbSAT.svg) ](https://travis-ci.org/Lipen/fbSAT)

# fbSAT

Tool for automatic inference and generalization of function block finite-state models.

---

## Build

* In order to build everything and run tests use gradle:

    ```
    ./gradlew clean build
    ```

## Run

* If you want to run fbSAT with specific arguments, use `run` gradle task:

    ```
    ./gradlew run --args="-h"
    ```

* If you want to build fat-jar with all dependencies included and execute it manually, use `shadowJar` gradle task:

    ```
    ./gradlew clean shadowJar && java -jar build/libs/fbSAT.jar -h
    ```

* If you want to create and install application distribution and use provided binary, use `isntallDist` gradle task:

    ```
    ./gradlew clean installDist && ./build/install/fbSAT/bin/fbSAT -h
    ```

## Command-line interface

```
Usage: fbsat [OPTIONS]

Options:
  -i, --scenarios <path>                  File with scenarios [required]
  -ce, --counterexamples <path>           File with counter-examples
  -smv, --smvdir <path>                   Folder with SMV files/scripts for verification
  -o, --outdir <path>                     Output directory [default: current directory]
  -m, --method <method>                   Method to use [required]
  -C INT                                  Number of automaton states
  -K INT                                  Maximum number of transitions from each state
  -P INT                                  Maximum number of nodes in guard's boolean formula's parse tree
  -T INT                                  Upper bound on total number of transitions in automaton
  -N INT                                  Upper bound on total number of nodes in all guard-trees
  --solver <cmd>                          SAT-solver [default: incremental-cryptominisat]
  --incremental / --no-incremental        Use IncrementalSolver backend [default: true]
  --forbid-loops / --no-forbid-loops      Forbid loops [default: true]
  --fail-verify-st / --no-fail-verify-st  Halt if verification of scenario tree has failed [default: true]
  --fail-verify-ce / --no-fail-verify-ce  Halt if verification of counterexamples has failed [default: true]
  --vis FILE                              [DEBUG] Visualize given counterexamples via graphviz
  -h, --help                              Show this message and exit
```
