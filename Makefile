.PHONY: all
all: build

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

build: check-fmt compile test

