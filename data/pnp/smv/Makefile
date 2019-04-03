all:
	@echo "Specify target [model counterexamples]"

.PHONY: model
model: control.smv extra.smv plant.smv main.smv spec.smv
	bash -c "cat <(echo '-- control.smv') control.smv <(echo '-- extra.smv') extra.smv <(echo -e '\n\n') <(echo '-- plant.smv') plant.smv <(echo -e '\n\n') <(echo '-- main.smv') main.smv <(echo -e '\n\n') <(echo '-- spec.smv') spec.smv > model.smv"

.PHONY: counterexamples
counterexamples: commands model.smv
	rm -f ltl
	touch ltl  # because auto-created file is owned by root

	rm -f counterexamples

	NuSMV -source commands model.smv
	@echo "Found $(shell grep LTL counterexamples | wc -l) counterexample(s)"
