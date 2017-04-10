REBAR = ./rebar

all: compile

compile: deps
	${REBAR} compile

deps:
	${REBAR} get-deps

test: deps compile
	${REBAR} eunit skip_deps=true

clean:
	${REBAR} clean
	rm -rf deps
