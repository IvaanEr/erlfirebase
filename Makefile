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
run:
	erl -pa ebin deps/jsx/ebin deps/lager/ebin -eval "application:start(erlfirebase)"
