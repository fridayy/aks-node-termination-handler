.PHONY: all
all: build

format:
	rebar3 fmt

check-types:
	rebar3 gradualizer

check-fmt:
	rebar3 fmt --check

compile:
	rebar3 compile

test:
	rebar3 as test ct -v

build_release:
	rebar3 as prod release

release: build_release
	./bin/release.sh

.PHONY: clean
clean:
	rebar3 clean

build: check-fmt check-types compile test
