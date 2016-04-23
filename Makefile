# Author Tony Wallace
all:config_check

config_check:ebin/config_check.beam ebin/term_defs.beam rebar.config
	./mad release script config_check

ebin/%.beam: src/%.erl
	erlc -o ebin $< 

test:
	erl -s term_defs test -s init stop
clean:
	rm config_check ebin/*.beam


