[ ![Build Status](https://travis-ci.org/ctlab/fbSAT.svg) ](https://travis-ci.org/ctlab/fbSAT)

# fbSAT

Tool for automatic inference and generalization of function block finite-state models.

---

## Build

* In order to build everything, use shipped gradle wrapper:

    ```sh
    ./gradlew
    ```

    By default, it runs `clean build installDist` gradle tasks.

## Run

* Gradle task `installDist` produces `fbSAT` binaries in `build/install/fbSAT/bin/` directory:

    ```
    ## on Unix
    ./build/install/fbSAT/bin/fbSAT -h

    ## on Windows
    build/install/fbSAT/bin/fbSAT.bat -h
    ```

* If you want to build fat-jar with all dependencies included, use `shadowJar` gradle task:

    ```
    ./gradlew clean shadowJar
    java -jar build/libs/fbSAT.jar -h
    ```

## Command-line interface

```
Usage: fbsat [OPTIONS]

Options:
  -i, --scenarios <path>               File with scenarios [required]
  -ce, --counterexamples <path>        File with counter-examples
  -smv, --smvdir <path>                Directory with SMV files/scripts for verification
  -o, --outdir <path>                  Output directory [default: current directory]
  -m, --method <method>                Method to use [required]
  -C <int>                             Number of automaton states
  -K <int>                             Maximum number of transitions from each state
  -P <int>                             Maximum number of nodes in guard's boolean formula's parse tree
  -T <int>                             Upper bound on total number of transitions in automaton
  -N <int>                             Upper bound on total number of nodes in all guard-trees
  -w <int>                             Maximum plateau width
  --solver <cmd>                       SAT-solver [default: incremental-cryptominisat]
  --incremental / --no-incremental     Use IncrementalSolver backend [default: true]
  --forbid-loops / --no-forbid-loops   Forbid loops [default: true]
  -h, --help                           Show this message and exit
```
