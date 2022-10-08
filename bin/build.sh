#!/usr/bin/env bash
set -eou pipefail

build() {
  rebar3 fmt --check
  rebar3 compile
  rebar3 as prod release
}

test() {
  rebar3 ct -v
}

build
test