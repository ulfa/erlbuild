#!/bin/sh

erl -pa $PWD/ebin $PWD/test $PWD/deps/*/ebin -boot start_sasl -s erlbuild
