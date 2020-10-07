FLAP?=../_build/default/src/flap.exe
JALONS=01-Parsing.results 02-Interpreter.results
EXTS=hopix hobix fopix retrolix

.PHONY: all clean test FAKE
.PRECIOUS: %.output %.expected %.score

all: all.results

clean:
	@rm -f all.results $(JALONS)
	@find \( -name "*.output" -or -name "*.score" \) -exec rm {} \;

test: clean
	@$(MAKE) -s all

FAKE:

$(FLAP): FAKE
	@cd .. && dune build --display=quiet

all.results: $(JALONS)
	@cat $^ > $@
	@echo -ne "\033[1mTotal: "
	@cat $@ | cut -d: -f2 | \
		awk -F "/" \
		'{ score += $$1; all += $$2 } END { print score "/" all }'
	@echo -ne "\e[0m"

.SECONDEXPANSION:
%.results: $$(subst .expected,.score,$$(wildcard %/*.expected))
	@echo -n "$*: " > $@
	@awk '{ score += $$1; all += 1 } END { print score "/" all  }' $^ >> $@
	@echo -e "\033[1m`cat $@`\e[0m"

%.score: %.output %.expected
	@diff -uwB $^ 1>/dev/null 2>&1 && echo 1 > $@ || echo 0 > $@
	@if [ `cat $@` -ne 0 ]; then \
		echo -e "\e[32m[OK] `basename $@ .score`\e[0m"; \
	else \
		echo -e "\e[31m[KO] `basename $@ .score`\e[0m"; \
		diff -uwB $^; exit 0; \
	fi

%.output: %.hopix $(FLAP)
	@$(FLAP) -s hopix --verbose true --dry true --unsafe true \
		$< >$@ 2>&1; exit 0

%.output: %.eval.hopix $(FLAP)
	@$(FLAP) -s hopix --run true --dry true --unsafe true --sexp-in true \
		$< >$@ 2>&1; exit 0
