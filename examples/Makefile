test: hello.conf.etf ebin/hello.beam
	escript ebin/hello.beam

hello.conf.etf:hello_conf.def hello.conf
	./config_check hello.conf hello_conf.def

ebin/hello.beam: src/hello.erl
	erlc -o ebin src/hello.erl

clean:
	rm hello.conf.etf ebin/hello.beam
