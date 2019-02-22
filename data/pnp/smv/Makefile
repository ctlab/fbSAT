all:
	@echo "Specify target [model counterexamples]"

.PHONY: model
model:
	bash -c "cat <(echo '-- control.smv') control.smv <(echo '-- extra.smv') extra.smv <(echo -e '\n\n') <(echo '-- plant.smv') plant.smv <(echo -e '\n\n') <(echo '-- main.smv') main.smv <(echo -e '\n\n') <(echo '-- specification.smv') spec.smv > model.smv"

.PHONY: counterexamples
counterexamples:
	rm -f ltl
	touch ltl  # because auto-created file is owned by root

	rm -f counterexamples
	# touch counterexamples

	NuSMV -source commands model.smv
	@echo "Found $(shell ag --silent LTL counterexamples | wc -l) counterexample(s)"
