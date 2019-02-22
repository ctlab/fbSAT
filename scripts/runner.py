import os
import time
import shutil
from pathlib import Path

time_start = time.time()
iterations = 1000

tests_name = 'tests-4'
C = 8  # 6 / 8
P = 7  # 3 / 5
extra_args = ''

path_fbsat = Path('~/dev/kt/fbSAT').expanduser()
path_scenarios = path_fbsat / 'data' / tests_name
path_counterexamples = path_fbsat / 'ce'
path_output = path_fbsat / 'out' / tests_name
path_automaton = path_output / 'automaton.smv'
path_smv = path_fbsat / 'data/pnp/smv'
path_control = path_smv / 'control.smv'

os.chdir(path_fbsat)
os.system('./gradlew -q jar')

# os.system(f'rm -f {path_counterexamples}')
# os.system(f'mv -f {path_counterexamples} {path_counterexamples}.old')
os.system(f'touch {path_counterexamples}')

for i in range(1, iterations):
    print(f'===== Iteration #{i} / {iterations} =====')

    os.system(f'rm -rf {path_output}-last')
    os.system(f'mv {path_output} {path_output}-last')

    os.chdir(path_fbsat)
    cmd = f'./fbsat -i {path_scenarios} -o {path_output} -m extended -C {C} -P {P} -ce {path_counterexamples} {extra_args}'
    print(f"[$] Running '{cmd}'")
    os.system(cmd)

    print(f"[*] Copying '{path_automaton}' to '{path_control}'...")
    shutil.copy(path_automaton, path_control)

    print(f"[*] Making...")
    os.chdir(path_smv)
    os.system('make model counterexamples')

    if os.path.exists('counterexamples'):
        print('[*] Appending new counterexamples...')
        os.system(f'cat counterexamples >> {path_counterexamples}')
    else:
        print('[+] There is no counterexamples, fully-verified automaton has been inferred!')
        break
else:
    print(f'[!] Stopped after {iterations} iterations.')

time_verify = time.time() - time_start

print('[*] Post-minimizing...')
os.chdir(path_fbsat)
cmd = f'./fbsat -i {path_scenarios} -o {path_output}-min -m extended-min -C {C} -P {P} -ce {path_counterexamples} {extra_args}'
print(f"[$] Running '{cmd}'")
os.system(cmd)

time_minimize = time.time() - (time_verify + time_start)
time_total = time.time() - time_start
print(f'[@] All done after {i} iterations + minimization!\n    Iterative verification process: {time_verify:.3f} s\n    Minimization time: {time_minimize:.3f} s\n    Total running time: {time_total:.3f} s')
