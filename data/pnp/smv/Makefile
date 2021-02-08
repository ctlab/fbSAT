all:
	@echo "Specify target [model counterexamples clean]"

.PHONY: model
model: control.smv extra.smv plant.smv main.smv spec.smv
	cat $^ > model.smv

.PHONY: counterexamples
counterexamples: commands model.smv
	rm -f ltl && touch ltl && \
	rm -f counterexamples && \
	rm -f nusmv-output.txt && touch nusmv-output.txt && \
	NuSMV -source commands model.smv > nusmv-output.txt

.PHONY: clean
clean:
	rm -f ltl counterexamples model.smv
