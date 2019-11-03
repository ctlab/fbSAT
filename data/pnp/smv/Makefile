all:
	@echo "Specify target [model counterexamples clean]"

.PHONY: model
model: control.smv extra.smv plant.smv main.smv spec.smv
	@echo "-- control.smv" > model.smv
	@cat control.smv >> model.smv
	@echo "-- extra.smv" >> model.smv
	@cat extra.smv >> model.smv
	@echo -e "\n\n-- plant.smv" >> model.smv
	@cat plant.smv >> model.smv
	@echo -e "\n\n-- main.smv" >> model.smv
	@cat main.smv >> model.smv
	@echo -e "\n\n-- spec.smv" >> model.smv
	@cat spec.smv >> model.smv
	@echo "Model created"

.PHONY: counterexamples
counterexamples: commands model.smv
	rm -f ltl
	touch ltl  # because auto-created file is owned by root

	rm -f counterexamples

	NuSMV -source commands model.smv
	@echo "Found $(shell grep LTL counterexamples | wc -l) counterexample(s)"

.PHONY: clean
clean:
	rm -f ltl
	rm -f counterexamples
	rm -f model.smv
