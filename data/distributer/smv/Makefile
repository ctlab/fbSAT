all:
	@echo "Specify target [model counterexamples clean]"

.PHONY: model
model: main.smv alpha.smv beta.smv gamma.smv omega.smv
	cat $^ > model.smv

.PHONY: counterexamples
counterexamples: commands model.smv
	rm -f ltl && touch ltl && \
	rm -f counterexamples && \
	rm -f counterexamples.xml && \
	rm -f nusmv-output.txt && touch nusmv-output.txt && \
	NuSMV -source commands model.smv > nusmv-output.txt

.PHONY: clean
clean:
	rm -f ltl counterexamples counterexamples.xml model.smv
