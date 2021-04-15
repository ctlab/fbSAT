all:
	@echo "Specify target [model counterexamples clean]"

.PHONY: model
model: control.smv extra.smv plant.smv main.smv spec.smv $(eval SHELL:=/bin/bash)
	@echo "-- control.smv" > model.smv
	@cat control1.smv >> model.smv
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


.PHONY: traces
traces: model.smv $(eval SHELL:=/bin/bash)
	rm -rf traces_f
	mkdir traces -p
	mkdir traces_raw -p
	mkdir traces_smv -p
	rm -rf traces/*
	rm -rf traces_raw/*
	rm -rf traces_smv/*

	len_range=`seq 15 25`; \
	traces_count_range=`seq 10`; \
	count_range=`seq 5`; \
	echo -e "go" > commands2simulate; \
	for len in $${len_range}; do \
		for traces_count in $${traces_count_range}; do \
			for count in $${count_range}; do \
				mkdir -p traces_f/$${traces_count}/$$len/$$count/xml; \
				mkdir -p traces_f/$${traces_count}/$$len/$$count/smv; \
				for trace_num in `seq $$traces_count`; do \
					echo -e "pick_state -v -r\nsimulate -k $$len -r" >> commands2simulate; \
					echo -e "show_traces -p4 -o traces_f/$${traces_count}/$$len/$$count/xml/$${trace_num}" >> commands2simulate; \
					echo -e "show_traces -v  -o traces_f/$${traces_count}/$$len/$$count/smv/$${trace_num}" >> commands2simulate; \
				done; \
			done; \
		done; \
	done; \
	echo -e "time\nquit" >> commands2simulate; \
	NuSMV -source commands2simulate model.smv > logs; \
	p1_counter=0; \
	for len in $${len_range}; do \
		for traces_count in $${traces_count_range}; do \
			for count in $${count_range}; do \
				p1_counter=$$(( p1_counter + traces_count )); \
				printf -v file 'traces/traces_%02d_%02d_%02d.xml' "$${traces_count}" "$${len}" "$${count}"; \
				printf -v file_smv 'traces_smv/traces_%02d_%02d_%02d.smvout' "$${traces_count}" "$${len}" "$${count}"; \
				printf -v file_raw 'traces_raw/traces_%02d_%02d_%02d.xml' "$${traces_count}" "$${len}" "$${count}"; \
				cat traces_f/$${traces_count}/$$len/$$count/xml/* > $$file; \
				cat traces_f/$${traces_count}/$$len/$$count/smv/* > $${file_smv}; \
				cp $$file $${file_raw}; \
				sed '/<?xml version="1.0" encoding="UTF-8"?>/d' $$file > tmp; \
				sed '1i\<?xml version="1.0" encoding="UTF-8"?>\n<all>' tmp > $$file; \
				echo -e "</all>\n<!--" >> $$file; \
				grep -m $${p1_counter} "    p1 =" logs | tail -n $${traces_count} >> $$file; \
				echo "-->" >> $$file; \
			done \
		done \
	done

	rm -rf traces_f

	python3 translate.py

.PHONY: clean
clean:
	rm -f ltl
	rm -f counterexamples
	rm -f model.smv
