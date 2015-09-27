#!/usr/bin/env bash
rebar compile
erl -pa ebin -pa deps/*/ebin -eval "application:start(compiler), application:start(syntax_tools), application:start(goldrush), application:start(lager), application:start(khronos)."