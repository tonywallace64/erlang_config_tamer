# Author Tony Wallace
all:config_check ebin/substitute.beam

config_check:ebin/config_check.beam ebin/term_defs.beam rebar.config
	./mad release script config_check

ebin/%.beam: src/%.erl
	erlc -o ebin $< 

.PHONY: test clean
test:
	erl -s term_defs test -s init stop
clean:
	rm config_check ebin/*.beam


